module

public import Lean

public section

namespace Informalize

open Lean

structure InformalOccurrence where
  declName : Name
  location? : Option String := none
  deriving Inhabited, Repr, BEq

structure InformalDeclEntry where
  declName : Name
  locations : Array String := #[]
  deriving Inhabited, Repr, BEq

abbrev InformalOccurrences := Array InformalOccurrence

structure InformalState where
  occurrences : InformalOccurrences := #[]
  localOccurrences : InformalOccurrences := #[]
  declOrder : Array Name := #[]
  byDecl : NameMap (Array String) := {}
  deriving Inhabited

private def pushLocationDedup (locations : Array String) (location : String) : Array String :=
  if locations.contains location then
    locations
  else
    locations.push location

private def InformalState.addOccurrence
    (state : InformalState)
    (occurrence : InformalOccurrence)
    (isLocal : Bool := true) : InformalState :=
  let existing? := state.byDecl.find? occurrence.declName
  let existing := existing?.getD #[]
  let updated :=
    match occurrence.location? with
    | some location =>
      pushLocationDedup existing location
    | none =>
      existing
  let declOrder :=
    match existing? with
    | some _ =>
      state.declOrder
    | none =>
      state.declOrder.push occurrence.declName
  let localOccurrences :=
    if isLocal then
      state.localOccurrences.push occurrence
    else
      state.localOccurrences
  {
    occurrences := state.occurrences.push occurrence
    localOccurrences
    declOrder
    byDecl := state.byDecl.insert occurrence.declName updated
  }

initialize informalExt : PersistentEnvExtension InformalOccurrence InformalOccurrence InformalState ←
  registerPersistentEnvExtension {
    name := `Informalize.informalExt
    mkInitial := pure {}
    addImportedFn := fun imported => do
      let mut state : InformalState := {}
      for importedOccurrences in imported do
        for occurrence in importedOccurrences do
          state := state.addOccurrence occurrence (isLocal := false)
      return state
    addEntryFn := fun state occurrence =>
      state.addOccurrence occurrence
    exportEntriesFn := fun state =>
      state.localOccurrences
    asyncMode := .sync
  }

def addInformalOccurrence
    (declName : Name)
    (location? : Option String := none) : CoreM Unit :=
  modifyEnv fun env =>
    informalExt.addEntry env {
      declName
      location?
    }

private def getInformalState : CoreM InformalState := do
  let env ← getEnv
  pure (informalExt.getState env)

private def declEntryLt (a b : InformalDeclEntry) : Bool :=
  Name.quickLt a.declName b.declName

def allInformalDeclEntries : CoreM (Array InformalDeclEntry) := do
  let state ← getInformalState
  let entries := state.declOrder.foldl (init := #[]) fun acc declName =>
    let locations :=
      match state.byDecl.find? declName with
      | some locations => locations
      | none => #[]
    acc.push {
      declName
      locations
    }
  return entries.qsort declEntryLt

def informalDeclEntry? (declName : Name) : CoreM (Option InformalDeclEntry) := do
  let state ← getInformalState
  match state.byDecl.find? declName with
  | some locations =>
    return some {
      declName
      locations
    }
  | none =>
    return none

end Informalize
