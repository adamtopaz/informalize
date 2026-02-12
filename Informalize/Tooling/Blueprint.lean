import Lean
import Informalize.Extension

open Lean Elab Command

namespace Informalize.Tooling

inductive BlueprintFormat where
  | markdown
  | json
  deriving Inhabited, Repr, BEq

def schemaVersion : String := "1"

private def statusToString : InformalStatus → String
  | .informal => "informal"
  | .formalized => "formalized"

private def sourcePointer (entry : InformalEntry) : String :=
  s!"{entry.sourceInfo.moduleName}:{entry.sourceInfo.line}:{entry.sourceInfo.column}"

private def fileDisplayName (filePath : String) : String :=
  match filePath.splitOn "/" |>.getLast? with
  | some fileName => fileName
  | none => filePath

private def oneLine (s : String) : String :=
  (s.replace "\n" " ").replace "\t" " "

private def escapeMd (s : String) : String :=
  ((oneLine s).replace "|" "\\|").trimAscii.toString

private def declLabel (entry : InformalEntry) : String :=
  match entry.declName with
  | some declName => toString declName
  | none => "(anonymous)"

private def dedupSortedNames (names : Array Name) : Array Name := Id.run do
  let mut seen : NameSet := {}
  let mut result : Array Name := #[]
  for name in names do
    if !seen.contains name then
      seen := seen.insert name
      result := result.push name
  return result

private def mergeUniqueNames (base extra : Array Name) : Array Name := Id.run do
  let mut seen : NameSet := {}
  let mut result : Array Name := #[]
  for name in base do
    if !seen.contains name then
      seen := seen.insert name
      result := result.push name
  for name in extra do
    if !seen.contains name then
      seen := seen.insert name
      result := result.push name
  return result

private def refsForDecl (declRefs : Array (Name × Array Name)) (declName : Name) : Array Name :=
  match declRefs.find? (·.1 == declName) with
  | some (_, refs) => refs
  | none => #[]

private def insertDeclRefs
    (declRefs : Array (Name × Array Name))
    (declName : Name)
    (refs : Array Name) : Array (Name × Array Name) := Id.run do
  let mut found := false
  let mut result : Array (Name × Array Name) := #[]
  for (name, currentRefs) in declRefs do
    if name == declName then
      found := true
      result := result.push (name, mergeUniqueNames currentRefs refs)
    else
      result := result.push (name, currentRefs)
  if !found then
    result := result.push (declName, refs)
  return result

private def buildDeclDependencyGraph (entries : InformalEntries) : Array (Name × Array Name) :=
  let declNames :=
    dedupSortedNames ((entries.filterMap (·.declName)).qsort Name.quickLt)
  let declSet := declNames.foldl (init := ({} : NameSet)) fun set declName =>
    set.insert declName
  let declRefs := Id.run do
    let mut refs : Array (Name × Array Name) := #[]
    for entry in entries do
      if let some declName := entry.declName then
        refs := insertDeclRefs refs declName entry.referencedConstants
    return refs
  declNames.map fun declName =>
    let refs := refsForDecl declRefs declName
    let targets := Id.run do
      let mut seen : NameSet := {}
      let mut targets : Array Name := #[]
      for refName in refs do
        if refName != declName && declSet.contains refName && !seen.contains refName then
          seen := seen.insert refName
          targets := targets.push refName
      return targets.qsort Name.quickLt
    (declName, targets)

private def dependencyEdgesFromEntries (entries : InformalEntries) : Array (Name × Array Name) :=
  buildDeclDependencyGraph entries

def dependencyEdges : CoreM (Array (Name × Array Name)) := do
  let entries ← Informalize.allEntries
  pure <| dependencyEdgesFromEntries entries

private def mkSourceJson (source : SourceRef) : Json :=
  Json.mkObj [
    ("module", toString source.moduleName),
    ("file", fileDisplayName source.fileName),
    ("line", source.line),
    ("column", source.column),
    ("endLine", source.endLine),
    ("endColumn", source.endColumn)
  ]

private def mkParamJson (param : Name × String) : Json :=
  Json.mkObj [
    ("name", toString param.1),
    ("type", param.2)
  ]

private def mkEntryJson (entry : InformalEntry) : Json :=
  Json.mkObj [
    ("declName", match entry.declName with | some n => Json.str (toString n) | none => Json.null),
    ("status", statusToString entry.status),
    ("description", entry.description),
    ("expectedType", entry.expectedType),
    ("params", Json.arr (entry.params.map mkParamJson)),
    ("referencedConstants", Json.arr (entry.referencedConstants.map (Json.str ∘ toString))),
    ("sourceInfo", mkSourceJson entry.sourceInfo)
  ]

private def mkEdgeJson (src : Name) (dst : Name) : Json :=
  Json.mkObj [
    ("from", toString src),
    ("to", toString dst)
  ]

private def mkGraphNodeJson (src : Name) (targets : Array Name) : Json :=
  Json.mkObj [
    ("declaration", toString src),
    ("targets", Json.arr (targets.map (Json.str ∘ toString)))
  ]

private def flattenEdges (graph : Array (Name × Array Name)) : Array (Name × Name) :=
  graph.foldl (init := #[]) fun acc (src, targets) =>
    acc ++ targets.map (fun dst => (src, dst))

def renderBlueprintJson : CoreM String := do
  let entries ← Informalize.allEntries
  let graph := dependencyEdgesFromEntries entries
  let edges := flattenEdges graph
  let informalCount : Nat := entries.foldl (init := 0) fun acc entry =>
    if entry.status == .informal then acc + 1 else acc
  let formalizedCount : Nat := entries.foldl (init := 0) fun acc entry =>
    if entry.status == .formalized then acc + 1 else acc
  let json := Json.mkObj [
    ("schemaVersion", schemaVersion),
    ("summary", Json.mkObj [
      ("totalEntries", entries.size),
      ("informalEntries", informalCount),
      ("formalizedEntries", formalizedCount)
    ]),
    ("entries", Json.arr (entries.map mkEntryJson)),
    ("dependencyGraph", Json.arr (graph.map (fun (src, targets) => mkGraphNodeJson src targets))),
    ("dependencyEdges", Json.arr (edges.map (fun (src, dst) => mkEdgeJson src dst)))
  ]
  return Json.pretty json

def renderBlueprintMarkdown : CoreM String := do
  let entries ← Informalize.allEntries
  let graph := dependencyEdgesFromEntries entries
  let informalCount : Nat := entries.foldl (init := 0) fun acc entry =>
    if entry.status == .informal then acc + 1 else acc
  let formalizedCount : Nat := entries.foldl (init := 0) fun acc entry =>
    if entry.status == .formalized then acc + 1 else acc

  let header := #[
    "# Informalize Blueprint",
    "",
    s!"- Schema version: {schemaVersion}",
    s!"- Total entries: {entries.size}",
    s!"- Informal entries: {informalCount}",
    s!"- Formalized entries: {formalizedCount}",
    "",
    "## Entries",
    "",
    "| Status | Declaration | Type | Description | Source |",
    "| --- | --- | --- | --- | --- |"
  ]

  let entryLines :=
    if entries.isEmpty then
      #["| (none) | (none) | (none) | (none) | (none) |"]
    else
      entries.map fun entry =>
        let status := statusToString entry.status
        let decl := escapeMd (declLabel entry)
        let typ := escapeMd entry.expectedType
        let desc := escapeMd entry.description
        let src := escapeMd (sourcePointer entry)
        s!"| {status} | `{decl}` | `{typ}` | {desc} | `{src}` |"

  let depHeader := #["", "## Dependency Graph", ""]
  let depLines :=
    if graph.isEmpty then
      #["- (none)"]
    else
      graph.map fun (src, targets) =>
        if targets.isEmpty then
          s!"- `{src}` -> _(none)_"
        else
          s!"- `{src}` -> {", ".intercalate (targets.toList.map (fun n => s!"`{n}`"))}"

  return "\n".intercalate (header ++ entryLines ++ depHeader ++ depLines).toList

private def emitBlueprint (fmt : BlueprintFormat) : CommandElabM Unit := do
  let output ←
    match fmt with
      | .markdown => liftCoreM renderBlueprintMarkdown
      | .json => liftCoreM renderBlueprintJson
  logInfo output

syntax (name := exportBlueprintCmd) "#export_blueprint" (ppSpace str)? : command

@[command_elab exportBlueprintCmd] def elabExportBlueprintCmd : CommandElab := fun stx => do
  match stx with
  | `(command| #export_blueprint) =>
    emitBlueprint .markdown
  | `(command| #export_blueprint $fmt:str) =>
    match fmt.getString with
    | "markdown" => emitBlueprint .markdown
    | "json" => emitBlueprint .json
    | other =>
      throwError "unknown blueprint format '{other}'; expected \"markdown\" or \"json\""
  | _ =>
    throwUnsupportedSyntax

end Informalize.Tooling
