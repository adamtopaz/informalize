module

public import Lean
public import Informalize.Extension

public section

open Lean Elab Command

namespace Informalize.Tooling

inductive BlueprintFormat where
  | markdown
  | json
  deriving Inhabited, Repr, BEq

def schemaVersion : String := "2"

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

private def usedConstants (constInfo : ConstantInfo) : Array Name :=
  Id.run do
    let mut refs : Array Name := #[]
    for constName in constInfo.getUsedConstantsAsSet do
      refs := refs.push constName
    return refs.qsort Name.quickLt

private def buildDeclDependencyGraph (env : Environment) (entries : InformalEntries) : Array (Name × Array Name) :=
  let declNames :=
    dedupSortedNames ((entries.filterMap (·.declName)).qsort Name.quickLt)
  let declSet := declNames.foldl (init := ({} : NameSet)) fun set declName =>
    set.insert declName
  declNames.map fun declName =>
    let refs :=
      match env.find? declName with
      | some constInfo => usedConstants constInfo
      | none => #[]
    let targets := Id.run do
      let mut seen : NameSet := {}
      let mut targets : Array Name := #[]
      for refName in refs do
        if refName != declName && declSet.contains refName && !seen.contains refName then
          seen := seen.insert refName
          targets := targets.push refName
      return targets.qsort Name.quickLt
    (declName, targets)

private def dependencyEdgesFromEntries (env : Environment) (entries : InformalEntries) : Array (Name × Array Name) :=
  buildDeclDependencyGraph env entries

def dependencyEdges : CoreM (Array (Name × Array Name)) := do
  let env ← getEnv
  let entries ← Informalize.allEntries
  pure <| dependencyEdgesFromEntries env entries

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

private def mkDocRefJson (docRef : DocRef) : Json :=
  Json.mkObj [
    ("path", docRef.path),
    ("id", match docRef.id? with | some id => Json.str id | none => Json.null),
    ("raw", docRef.raw)
  ]

private def mkEntryJson (entry : InformalEntry) : Json :=
  Json.mkObj [
    ("declName", match entry.declName with | some n => Json.str (toString n) | none => Json.null),
    ("status", statusToString entry.status),
    ("description", entry.description),
    ("docRef", match entry.docRef? with | some docRef => mkDocRefJson docRef | none => Json.null),
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
  let env ← getEnv
  let graph := dependencyEdgesFromEntries env entries
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
  let env ← getEnv
  let graph := dependencyEdgesFromEntries env entries
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
    "| Status | Declaration | Type | Description | DocRef | Source |",
    "| --- | --- | --- | --- | --- | --- |"
  ]

  let entryLines :=
    if entries.isEmpty then
      #["| (none) | (none) | (none) | (none) | (none) | (none) |"]
    else
      entries.map fun entry =>
        let status := statusToString entry.status
        let decl := escapeMd (declLabel entry)
        let typ := escapeMd entry.expectedType
        let desc := escapeMd entry.description
        let docRef :=
          match entry.docRef? with
          | some docRef => escapeMd docRef.raw
          | none => "(none)"
        let src := escapeMd (sourcePointer entry)
        s!"| {status} | `{decl}` | `{typ}` | {desc} | {docRef} | `{src}` |"

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

private meta unsafe def emitBlueprint (fmt : BlueprintFormat) : CommandElabM Unit := do
  let output ←
    match fmt with
      | .markdown => do
        let renderMarkdown ← liftCoreM <| Lean.evalConst (CoreM String) ``Informalize.Tooling.renderBlueprintMarkdown
        liftCoreM renderMarkdown
      | .json => do
        let renderJson ← liftCoreM <| Lean.evalConst (CoreM String) ``Informalize.Tooling.renderBlueprintJson
        liftCoreM renderJson
  logInfo output

syntax (name := exportBlueprintCmd) "#export_blueprint" (ppSpace str)? : command

@[command_elab exportBlueprintCmd] meta unsafe def elabExportBlueprintCmd : CommandElab := fun stx => do
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
