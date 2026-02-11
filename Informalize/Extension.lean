import Lean

namespace Informalize

open Lean

structure SourceRef where
  moduleName : Name := .anonymous
  fileName : String := "<unknown>"
  line : Nat := 0
  column : Nat := 0
  endLine : Nat := 0
  endColumn : Nat := 0
  deriving Inhabited, Repr, BEq

inductive InformalStatus where
  | informal
  | formalized
  deriving Inhabited, Repr, BEq, DecidableEq

structure InformalEntry where
  declName : Option Name := none
  description : String := ""
  params : Array (Name × String) := #[]
  expectedType : String := ""
  referencedConstants : Array Name := #[]
  sourceInfo : SourceRef := {}
  status : InformalStatus := .informal
  deriving Repr

abbrev InformalEntries := Array InformalEntry

private def statusToString : InformalStatus → String
  | .informal => "informal"
  | .formalized => "formalized"

private def statusOfString? (status : String) : Option InformalStatus :=
  match status with
  | "informal" => some .informal
  | "formalized" => some .formalized
  | _ => none

private def markerKey : Name := `Informalize.informalMeta.marker
private def statusKey : Name := `Informalize.informalMeta.status
private def descriptionKey : Name := `Informalize.informalMeta.description
private def expectedTypeKey : Name := `Informalize.informalMeta.expectedType

private def sourceModuleKey : Name := `Informalize.informalMeta.sourceModule
private def sourceFileKey : Name := `Informalize.informalMeta.sourceFile
private def sourceLineKey : Name := `Informalize.informalMeta.sourceLine
private def sourceColumnKey : Name := `Informalize.informalMeta.sourceColumn
private def sourceEndLineKey : Name := `Informalize.informalMeta.sourceEndLine
private def sourceEndColumnKey : Name := `Informalize.informalMeta.sourceEndColumn

private def paramCountKey : Name := `Informalize.informalMeta.paramCount
private def paramNameKey (idx : Nat) : Name := (`Informalize.informalMeta.paramName).num idx
private def paramTypeKey (idx : Nat) : Name := (`Informalize.informalMeta.paramType).num idx

private def refConstCountKey : Name := `Informalize.informalMeta.refConstCount
private def refConstKey (idx : Nat) : Name := (`Informalize.informalMeta.refConst).num idx

def encodeEntryMetadata (entry : InformalEntry) : MData := Id.run do
  let mut md : MData := {}
  md := md.setBool markerKey true
  md := md.setString statusKey (statusToString entry.status)
  md := md.setString descriptionKey entry.description
  md := md.setString expectedTypeKey entry.expectedType

  md := md.setName sourceModuleKey entry.sourceInfo.moduleName
  md := md.setString sourceFileKey entry.sourceInfo.fileName
  md := md.setNat sourceLineKey entry.sourceInfo.line
  md := md.setNat sourceColumnKey entry.sourceInfo.column
  md := md.setNat sourceEndLineKey entry.sourceInfo.endLine
  md := md.setNat sourceEndColumnKey entry.sourceInfo.endColumn

  md := md.setNat paramCountKey entry.params.size
  for idx in [0:entry.params.size] do
    let (paramName, paramType) := entry.params[idx]!
    md := md.setName (paramNameKey idx) paramName
    md := md.setString (paramTypeKey idx) paramType

  md := md.setNat refConstCountKey entry.referencedConstants.size
  for idx in [0:entry.referencedConstants.size] do
    md := md.setName (refConstKey idx) entry.referencedConstants[idx]!

  return md

def decodeEntryMetadata? (declName : Option Name) (md : MData) : Option InformalEntry := do
  if !(md.getBool markerKey false) then
    none
  else
    let status ← statusOfString? (md.getString statusKey "")
    let paramCount := md.getNat paramCountKey 0
    let params := Id.run do
      let mut params : Array (Name × String) := #[]
      for idx in [0:paramCount] do
        let paramName := md.getName (paramNameKey idx)
        let paramType := md.getString (paramTypeKey idx) ""
        params := params.push (paramName, paramType)
      return params

    let refConstCount := md.getNat refConstCountKey 0
    let referencedConstants := Id.run do
      let mut referencedConstants : Array Name := #[]
      for idx in [0:refConstCount] do
        referencedConstants := referencedConstants.push (md.getName (refConstKey idx))
      return referencedConstants

    let sourceInfo : SourceRef := {
      moduleName := md.getName sourceModuleKey
      fileName := md.getString sourceFileKey "<unknown>"
      line := md.getNat sourceLineKey 0
      column := md.getNat sourceColumnKey 0
      endLine := md.getNat sourceEndLineKey 0
      endColumn := md.getNat sourceEndColumnKey 0
    }

    return {
      declName
      description := md.getString descriptionKey ""
      params
      expectedType := md.getString expectedTypeKey ""
      referencedConstants
      sourceInfo
      status
    }

def annotateExprWithEntry (entry : InformalEntry) (expr : Expr) : Expr :=
  Expr.mdata (encodeEntryMetadata entry) expr

private structure ScanState where
  visited : ExprSet := {}
  entries : InformalEntries := #[]

private partial def scanExpr (declName : Option Name) (expr : Expr) (state : ScanState) : ScanState :=
  if state.visited.contains expr then
    state
  else
    let state := { state with visited := state.visited.insert expr }
    match expr with
    | .forallE _ domain body _ =>
      let state := scanExpr declName domain state
      scanExpr declName body state
    | .lam _ domain body _ =>
      let state := scanExpr declName domain state
      scanExpr declName body state
    | .letE _ type value body _ =>
      let state := scanExpr declName type state
      let state := scanExpr declName value state
      scanExpr declName body state
    | .app fn arg =>
      let state := scanExpr declName fn state
      scanExpr declName arg state
    | .mdata md body =>
      let state :=
        match decodeEntryMetadata? declName md with
        | some entry => { state with entries := state.entries.push entry }
        | none => state
      scanExpr declName body state
    | .proj _ _ body =>
      scanExpr declName body state
    | _ =>
      state

private def entriesInConstant (declName : Name) (constInfo : ConstantInfo) : InformalEntries :=
  let state := scanExpr (some declName) constInfo.type {}
  let state :=
    match constInfo.value? with
    | some value => scanExpr (some declName) value state
    | none => state
  state.entries

private def sourceLt (a b : SourceRef) : Bool :=
  if Name.quickLt a.moduleName b.moduleName then
    true
  else if Name.quickLt b.moduleName a.moduleName then
    false
  else if a.fileName < b.fileName then
    true
  else if b.fileName < a.fileName then
    false
  else if a.line < b.line then
    true
  else if b.line < a.line then
    false
  else if a.column < b.column then
    true
  else if b.column < a.column then
    false
  else if a.endLine < b.endLine then
    true
  else if b.endLine < a.endLine then
    false
  else
    a.endColumn < b.endColumn

private def declNameLt (a b : Option Name) : Bool :=
  match a, b with
  | none, none => false
  | none, some _ => true
  | some _, none => false
  | some aName, some bName => Name.quickLt aName bName

private def entryLt (a b : InformalEntry) : Bool :=
  if sourceLt a.sourceInfo b.sourceInfo then
    true
  else if sourceLt b.sourceInfo a.sourceInfo then
    false
  else if declNameLt a.declName b.declName then
    true
  else if declNameLt b.declName a.declName then
    false
  else if a.description < b.description then
    true
  else if b.description < a.description then
    false
  else
    a.referencedConstants.size < b.referencedConstants.size

private def sortEntries (entries : InformalEntries) : InformalEntries :=
  entries.qsort entryLt

def allEntries : CoreM InformalEntries := do
  let env ← getEnv
  let entries := env.constants.fold (init := #[]) fun acc declName constInfo =>
    acc ++ entriesInConstant declName constInfo
  return sortEntries entries

def entriesByStatus (status : InformalStatus) : CoreM InformalEntries :=
  return (← allEntries).filter (·.status == status)

def entriesByDecl (declName : Name) : CoreM InformalEntries :=
  return (← allEntries).filter (·.declName == some declName)

def entriesByFile (fileName : String) : CoreM InformalEntries :=
  return (← allEntries).filter (·.sourceInfo.fileName == fileName)

def entriesReferencing (constName : Name) : CoreM InformalEntries :=
  return (← allEntries).filter (fun entry => entry.referencedConstants.contains constName)

def countsByStatus : CoreM (Nat × Nat) := do
  let entries ← allEntries
  let informalCount := entries.foldl (init := 0) fun acc entry =>
    if entry.status == .informal then acc + 1 else acc
  let formalizedCount := entries.foldl (init := 0) fun acc entry =>
    if entry.status == .formalized then acc + 1 else acc
  pure (informalCount, formalizedCount)

end Informalize
