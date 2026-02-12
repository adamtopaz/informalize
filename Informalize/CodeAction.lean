module

public import Lean
public import Lean.DeclarationRange
public import Informalize.Extension
public meta import Init.Data.String.Legacy

public section

open Lean Elab Command Meta Term

namespace Informalize

inductive ActionKind where
  | formalizeThis
  | cleanUp
  deriving Inhabited, Repr, BEq

instance : ToString ActionKind where
  toString
    | .formalizeThis => "formalize"
    | .cleanUp => "cleanup"

structure TextRange where
  startLine : Nat
  startColumn : Nat
  endLine : Nat
  endColumn : Nat
  deriving Inhabited, Repr, BEq

structure TextEdit where
  range : TextRange
  newText : String
  deriving Inhabited, Repr, BEq

structure CodeAction where
  kind : ActionKind
  title : String
  target : String
  source : SourceRef
  edits : Array TextEdit := #[]
  deriving Inhabited, Repr

private def fileDisplayName (filePath : String) : String :=
  match filePath.splitOn "/" |>.getLast? with
  | some fileName => fileName
  | none => filePath

private def oneLine (s : String) : String :=
  ((s.replace "\n" " ").replace "\t" " ").trimAscii.toString

private def clip (maxLen : Nat) (s : String) : String :=
  if s.length <= maxLen then
    s
  else
    String.ofList (s.toList.take (maxLen - 3)) ++ "..."

private def sourcePointer (source : SourceRef) : String :=
  s!"{source.moduleName}:{source.line}:{source.column}"

private def entryLabel (entry : InformalEntry) : String :=
  match entry.declName with
  | some declName => toString declName
  | none => "(anonymous)"

private def mkEntryRange (entry : InformalEntry) : TextRange :=
  {
    startLine := entry.sourceInfo.line
    startColumn := entry.sourceInfo.column
    endLine := entry.sourceInfo.endLine
    endColumn := entry.sourceInfo.endColumn
  }

private def escapeStringLiteral (s : String) : String :=
  (((s.replace "\\" "\\\\").replace "\"" "\\\"").replace "\n" " ").replace "\t" " "

private def escapeDocComment (s : String) : String :=
  (oneLine s).replace "-/" "- /"

private def renderDocRef (docRef : DocRef) : String :=
  docRef.raw

private def formalizedDocRefSuffix (entry : InformalEntry) : String :=
  match entry.docRef? with
  | some docRef =>
    s!" from \"{escapeStringLiteral (renderDocRef docRef)}\""
  | none =>
    ""

private def cleanupDocComment (entry : InformalEntry) : String :=
  match entry.docRef? with
  | some docRef =>
    s!"/-- {escapeDocComment entry.description}\n\nSee: {escapeDocComment (renderDocRef docRef)}\n-/\n"
  | none =>
    s!"/-- {escapeDocComment entry.description} -/\n"

def availableActionsForDecl (declName : Name) : CoreM (Array ActionKind) := do
  let entries ← entriesByDecl declName
  let mut kinds : Array ActionKind := #[]
  for entry in entries do
    match entry.status with
    | .informal =>
      if !kinds.contains .formalizeThis then
        kinds := kinds.push .formalizeThis
    | .formalized =>
      if !kinds.contains .cleanUp then
        kinds := kinds.push .cleanUp
  return kinds

private def sourceEq (a b : SourceRef) : Bool :=
  a.moduleName == b.moduleName &&
    a.fileName == b.fileName &&
    a.line == b.line &&
    a.column == b.column &&
    a.endLine == b.endLine &&
    a.endColumn == b.endColumn

private def entryIdentityEq (a b : InformalEntry) : Bool :=
  a.declName == b.declName &&
    a.status == b.status &&
    a.description == b.description &&
    a.docRef? == b.docRef? &&
    sourceEq a.sourceInfo b.sourceInfo

private partial def findBodyForEntry?
    (target : InformalEntry)
    (declName : Option Name)
    (expr : Expr) : Option Expr :=
  match expr with
  | .forallE _ domain body _ =>
    match findBodyForEntry? target declName domain with
    | some bodyExpr => some bodyExpr
    | none => findBodyForEntry? target declName body
  | .lam _ domain body _ =>
    match findBodyForEntry? target declName domain with
    | some bodyExpr => some bodyExpr
    | none => findBodyForEntry? target declName body
  | .letE _ type value body _ =>
    match findBodyForEntry? target declName type with
    | some bodyExpr => some bodyExpr
    | none =>
      match findBodyForEntry? target declName value with
      | some bodyExpr => some bodyExpr
      | none => findBodyForEntry? target declName body
  | .app fn arg =>
    match findBodyForEntry? target declName fn with
    | some bodyExpr => some bodyExpr
    | none => findBodyForEntry? target declName arg
  | .mdata md body =>
    match decodeEntryMetadata? declName md with
    | some decoded =>
      if entryIdentityEq decoded target then
        some body
      else
        findBodyForEntry? target declName body
    | none =>
      findBodyForEntry? target declName body
  | .proj _ _ body =>
    findBodyForEntry? target declName body
  | _ =>
    none

def bodyExprForEntry? (entry : InformalEntry) : CoreM (Option Expr) := do
  let some declName := entry.declName
    | return none
  let env ← getEnv
  match env.find? declName with
  | none => return none
  | some constInfo =>
    let inValue :=
      match constInfo.value? with
      | some value => findBodyForEntry? entry (some declName) value
      | none => none
    match inValue with
    | some bodyExpr => return some bodyExpr
    | none =>
      return findBodyForEntry? entry (some declName) constInfo.type

def hoverEntryForDecl? (declName : Name) : CoreM (Option InformalEntry) := do
  let entries ← entriesByDecl declName
  let formalized := entries.filter (·.status == .formalized)
  if let some latest := formalized.back? then
    return some latest
  else
    return entries.back?

def entriesForFileHint (fileHint : String) : CoreM InformalEntries := do
  let normalized := fileHint.trimAscii.toString
  let directEntries ← entriesByFile normalized
  if !directEntries.isEmpty then
    return directEntries
  let entries ← allEntries
  return entries.filter fun entry =>
    let fileName := entry.sourceInfo.fileName
    fileName == normalized || fileDisplayName fileName == normalized

private def prettyBodyExpr (bodyExpr : Expr) : CommandElabM String := do
  let bodyFmt ← liftTermElabM <| Meta.ppExpr bodyExpr
  return oneLine (toString bodyFmt)

private def declarationInsertRange? (declName : Name) : CommandElabM (Option TextRange) := do
  let declRanges? ← findDeclarationRanges? declName
  match declRanges? with
  | none =>
    return none
  | some declRanges =>
    return some {
      startLine := declRanges.range.pos.line
      startColumn := declRanges.range.pos.column
      endLine := declRanges.range.pos.line
      endColumn := declRanges.range.pos.column
    }

private def mkFormalizeAction (entry : InformalEntry) : CodeAction :=
  {
    kind := .formalizeThis
    title := "Formalize this"
    target := entryLabel entry
    source := entry.sourceInfo
    edits := #[{
      range := mkEntryRange entry
      newText := s!"formalized \"{escapeStringLiteral entry.description}\"{formalizedDocRefSuffix entry} as _"
    }]
  }

private def mkCleanupAction (entry : InformalEntry) (bodySnippet : String) (docInsert? : Option TextRange) : CodeAction :=
  let replaceEdit : TextEdit := {
    range := mkEntryRange entry
    newText := bodySnippet
  }
  let edits :=
    match docInsert? with
    | some docRange =>
      #[{
        range := docRange
        newText := cleanupDocComment entry
      }, replaceEdit]
    | none =>
      #[replaceEdit]
  {
    kind := .cleanUp
    title := "Clean up"
    target := entryLabel entry
    source := entry.sourceInfo
    edits
  }

private def actionsForEntry (entry : InformalEntry) : CommandElabM (Array CodeAction) :=
  match entry.status with
  | .informal =>
    return #[mkFormalizeAction entry]
  | .formalized => do
    let bodyExpr? ← liftCoreM <| bodyExprForEntry? entry
    let bodySnippet ←
      match bodyExpr? with
      | some bodyExpr => prettyBodyExpr bodyExpr
      | none => pure "_"
    let docInsert? ←
      match entry.declName with
      | some declName => declarationInsertRange? declName
      | none => pure none
    return #[mkCleanupAction entry bodySnippet docInsert?]

private def collectActions (entries : InformalEntries) : CommandElabM (Array CodeAction) := do
  let mut actions : Array CodeAction := #[]
  for entry in entries do
    actions := actions ++ (← actionsForEntry entry)
  return actions

private def renderRange (range : TextRange) : String :=
  s!"{range.startLine}:{range.startColumn}-{range.endLine}:{range.endColumn}"

private def renderEdit (edit : TextEdit) : String :=
  s!"    - {renderRange edit.range} => {clip 120 (oneLine edit.newText)}"

private def renderAction (action : CodeAction) : Array String :=
  #[s!"- {action.title} [{action.kind}] for {action.target} @ {sourcePointer action.source}"] ++
    (action.edits.map renderEdit)

private def renderActions (header : String) (actions : Array CodeAction) : String :=
  let lines :=
    if actions.isEmpty then
      #[header, "(none)"]
    else
      #[header] ++ (actions.foldl (init := #[]) fun acc action => acc ++ renderAction action)
  "\n".intercalate lines.toList

private def prettyBodyExprCore (bodyExpr : Expr) : CoreM String := do
  let env ← getEnv
  let opts ← getOptions
  let fmt ← (Lean.PrettyPrinter.ppExprLegacy env {} {} opts bodyExpr : IO Format)
  return oneLine (toString fmt)

private def renderCodeActionSuggestion (entry : InformalEntry) : CoreM (Array String) :=
  match entry.status with
  | .informal =>
    let suggestion := s!"formalized \"{escapeStringLiteral entry.description}\"{formalizedDocRefSuffix entry} as _"
    pure #[
      s!"- Formalize this [formalize] for {entryLabel entry} @ {sourcePointer entry.sourceInfo}",
      s!"    - suggestion => {clip 120 (oneLine suggestion)}"
    ]
  | .formalized => do
    let bodyExpr? ← bodyExprForEntry? entry
    let bodySnippet ←
      match bodyExpr? with
      | some bodyExpr =>
        try
          prettyBodyExprCore bodyExpr
        catch _ =>
          pure "_"
      | none =>
        pure "_"
    pure #[
      s!"- Clean up [cleanup] for {entryLabel entry} @ {sourcePointer entry.sourceInfo}",
      s!"    - suggestion => {clip 120 (oneLine bodySnippet)}",
      s!"    - doc-comment => {clip 120 (oneLine (cleanupDocComment entry))}"
    ]

private def renderCodeActionSuggestions
    (header : String)
    (entries : InformalEntries) : CoreM String := do
  if entries.isEmpty then
    return "\n".intercalate #[header, "(none)"].toList
  let mut lines : Array String := #[header]
  for entry in entries do
    lines := lines ++ (← renderCodeActionSuggestion entry)
  return "\n".intercalate lines.toList

def renderCodeActions : CoreM String := do
  let entries ← allEntries
  renderCodeActionSuggestions s!"Code actions ({entries.size}):" entries

def renderCodeActionsForDecl (declName : Name) : CoreM String := do
  let entries ← entriesByDecl declName
  renderCodeActionSuggestions s!"Code actions for {declName} ({entries.size}):" entries

private def renderHover (declName : Name) (entry : InformalEntry) : String :=
  let status :=
    match entry.status with
    | .informal => "informal"
    | .formalized => "formalized"
  let docLines :=
    match entry.docRef? with
    | some docRef => #[s!"doc: {renderDocRef docRef}"]
    | none => #[]
  "\n".intercalate (
    (#[
      s!"Hover: {declName}",
      s!"status: {status}",
      s!"description: {entry.description}"
    ] ++ docLines ++ #[
      s!"type: {entry.expectedType}",
      s!"source: {sourcePointer entry.sourceInfo}"
    ]).toList
  )

def renderHoverForDecl (declName : Name) : CoreM String := do
  let entry? ← hoverEntryForDecl? declName
  match entry? with
  | some entry =>
    return renderHover declName entry
  | none =>
    return s!"Hover: {declName}\n(no metadata found)"

private def renderPanel (fileLabel : String) (entries : InformalEntries) : String :=
  let informalCount := entries.foldl (init := 0) fun acc entry =>
    if entry.status == .informal then acc + 1 else acc
  let formalizedCount := entries.foldl (init := 0) fun acc entry =>
    if entry.status == .formalized then acc + 1 else acc
  let entryLines :=
    if entries.isEmpty then
      #["- (none)"]
    else
      entries.foldl (init := #[]) fun acc entry =>
        let status :=
          match entry.status with
          | .informal => "informal"
          | .formalized => "formalized"
        let baseLine :=
          s!"- [{status}] {entryLabel entry} @ {sourcePointer entry.sourceInfo} - {clip 96 (oneLine entry.description)}"
        let acc := acc.push baseLine
        match entry.docRef? with
        | some docRef =>
          acc.push s!"  doc: {clip 96 (oneLine (renderDocRef docRef))}"
        | none =>
          acc
  let lines :=
    #[
      s!"Informal panel: {fileLabel}",
      s!"total: {entries.size}",
      s!"informal: {informalCount}",
      s!"formalized: {formalizedCount}",
      "",
      "Entries:"
    ] ++ entryLines
  "\n".intercalate lines.toList

def renderPanelForFileHint (fileHint : String) : CoreM String := do
  let entries ← entriesForFileHint fileHint
  return renderPanel fileHint entries

syntax (name := informalCodeActionsCmd) "#informal_code_actions" : command
syntax (name := informalCodeActionsForCmd) "#informal_code_actions" ppSpace ident : command

@[command_elab informalCodeActionsCmd] meta unsafe def elabInformalCodeActionsCmd : CommandElab := fun _stx => do
  let renderCodeActionsCmd ← liftCoreM <| Lean.evalConst (CoreM String) ``Informalize.renderCodeActions
  logInfo (← liftCoreM renderCodeActionsCmd)

@[command_elab informalCodeActionsForCmd] meta unsafe def elabInformalCodeActionsForCmd : CommandElab := fun stx => do
  let declName := stx[1].getId
  let renderCodeActionsForDeclCmd ← liftCoreM <| Lean.evalConst (Name → CoreM String) ``Informalize.renderCodeActionsForDecl
  logInfo (← liftCoreM <| renderCodeActionsForDeclCmd declName)

syntax (name := informalHoverCmd) "#informal_hover" ppSpace ident : command

@[command_elab informalHoverCmd] meta unsafe def elabInformalHoverCmd : CommandElab := fun stx => do
  let declName := stx[1].getId
  let renderHoverCmd ← liftCoreM <| Lean.evalConst (Name → CoreM String) ``Informalize.renderHoverForDecl
  logInfo (← liftCoreM <| renderHoverCmd declName)

syntax (name := informalPanelCmd) "#informal_panel" : command
syntax (name := informalPanelForCmd) "#informal_panel" ppSpace str : command

@[command_elab informalPanelCmd] meta unsafe def elabInformalPanelCmd : CommandElab := fun _stx => do
  let fileName ← getFileName
  let fileHint :=
    match fileName.splitOn "/" |>.getLast? with
    | some hint => hint
    | none => fileName
  let renderPanelForFileHintCmd ← liftCoreM <| Lean.evalConst (String → CoreM String) ``Informalize.renderPanelForFileHint
  logInfo (← liftCoreM <| renderPanelForFileHintCmd fileHint)

@[command_elab informalPanelForCmd] meta unsafe def elabInformalPanelForCmd : CommandElab := fun stx => do
  let some fileHint := stx[1].isStrLit?
    | throwError "expected string literal path, e.g. #informal_panel \"MyFile.lean\""
  let renderPanelForFileHintCmd ← liftCoreM <| Lean.evalConst (String → CoreM String) ``Informalize.renderPanelForFileHint
  logInfo (← liftCoreM <| renderPanelForFileHintCmd fileHint)

end Informalize
