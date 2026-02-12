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

structure InformalState where
  entries : InformalEntries := #[]
  informalIdxs : Array Nat := #[]
  formalizedIdxs : Array Nat := #[]
  byDecl : NameMap (Array Nat) := {}
  byFile : Std.HashMap String (Array Nat) := {}
  byRefConst : NameMap (Array Nat) := {}
  deriving Inhabited

private def appendIndexByName
    (index : NameMap (Array Nat))
    (key : Name)
    (entryIdx : Nat) : NameMap (Array Nat) :=
  let values :=
    match index.find? key with
    | some values => values
    | none => #[]
  index.insert key (values.push entryIdx)

private def appendIndexByFile
    (index : Std.HashMap String (Array Nat))
    (key : String)
    (entryIdx : Nat) : Std.HashMap String (Array Nat) :=
  let values := index.getD key #[]
  index.insert key (values.push entryIdx)

private def InformalState.addEntry (state : InformalState) (entry : InformalEntry) : InformalState :=
  let entryIdx := state.entries.size
  let informalIdxs :=
    if entry.status == .informal then
      state.informalIdxs.push entryIdx
    else
      state.informalIdxs
  let formalizedIdxs :=
    if entry.status == .formalized then
      state.formalizedIdxs.push entryIdx
    else
      state.formalizedIdxs
  let byDecl :=
    match entry.declName with
    | some declName => appendIndexByName state.byDecl declName entryIdx
    | none => state.byDecl
  let byFile := appendIndexByFile state.byFile entry.sourceInfo.fileName entryIdx
  let byRefConst := Id.run do
    let mut byRefConst := state.byRefConst
    let mut seen : NameSet := {}
    for constName in entry.referencedConstants do
      if !seen.contains constName then
        seen := seen.insert constName
        byRefConst := appendIndexByName byRefConst constName entryIdx
    return byRefConst
  {
    entries := state.entries.push entry
    informalIdxs
    formalizedIdxs
    byDecl
    byFile
    byRefConst
  }

initialize informalExt : PersistentEnvExtension InformalEntry InformalEntry InformalState ←
  registerPersistentEnvExtension {
    name := `Informalize.informalExt
    mkInitial := pure {}
    addImportedFn := fun imported => do
      let mut state : InformalState := {}
      for importedEntries in imported do
        for entry in importedEntries do
          state := state.addEntry entry
      return state
    addEntryFn := fun state entry =>
      state.addEntry entry
    exportEntriesFn := fun state =>
      state.entries
    asyncMode := .sync
  }

def addInformalEntry (entry : InformalEntry) : CoreM Unit :=
  modifyEnv fun env => informalExt.addEntry env entry

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

private def getInformalState : CoreM InformalState := do
  let env ← getEnv
  pure (informalExt.getState env)

private def entriesAtIdxs (entries : InformalEntries) (idxs : Array Nat) : InformalEntries := Id.run do
  let mut result : InformalEntries := #[]
  for idx in idxs do
    match entries[idx]? with
    | some entry =>
      result := result.push entry
    | none =>
      pure ()
  return result

def allEntries : CoreM InformalEntries := do
  let state ← getInformalState
  return sortEntries state.entries

def entriesByStatus (status : InformalStatus) : CoreM InformalEntries := do
  let state ← getInformalState
  let idxs :=
    match status with
    | .informal => state.informalIdxs
    | .formalized => state.formalizedIdxs
  return sortEntries (entriesAtIdxs state.entries idxs)

def entriesByDecl (declName : Name) : CoreM InformalEntries := do
  let state ← getInformalState
  let idxs :=
    match state.byDecl.find? declName with
    | some idxs => idxs
    | none => #[]
  return sortEntries (entriesAtIdxs state.entries idxs)

def entriesByFile (fileName : String) : CoreM InformalEntries := do
  let state ← getInformalState
  let idxs := state.byFile.getD fileName #[]
  return sortEntries (entriesAtIdxs state.entries idxs)

def entriesReferencing (constName : Name) : CoreM InformalEntries := do
  let state ← getInformalState
  let idxs :=
    match state.byRefConst.find? constName with
    | some idxs => idxs
    | none => #[]
  return sortEntries (entriesAtIdxs state.entries idxs)

def countsByStatus : CoreM (Nat × Nat) := do
  let state ← getInformalState
  pure (state.informalIdxs.size, state.formalizedIdxs.size)

end Informalize
