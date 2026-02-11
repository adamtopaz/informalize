import Informalize

noncomputable def phase5Placeholder : Nat :=
  informal "Phase 5 placeholder declaration"

noncomputable def phase5Formalized : Nat :=
  formalized "Phase 5 formalized declaration" as Nat.succ phase5Placeholder

def phase5Clean : Nat :=
  Nat.succ 5

theorem phase5FormalizedProof : True := by
  formalized "Phase 5 formalized theorem" as
    exact True.intro

run_cmd do
  let placeholderActions ← Lean.Elab.Command.liftCoreM <| Informalize.availableActionsForDecl ``phase5Placeholder
  unless placeholderActions.contains .formalizeThis do
    throwError "expected phase5Placeholder to have a formalize action"

  let formalizedActions ← Lean.Elab.Command.liftCoreM <| Informalize.availableActionsForDecl ``phase5Formalized
  unless formalizedActions.contains .cleanUp do
    throwError "expected phase5Formalized to have a cleanup action"

  let proofActions ← Lean.Elab.Command.liftCoreM <| Informalize.availableActionsForDecl ``phase5FormalizedProof
  unless proofActions.contains .cleanUp do
    throwError "expected phase5FormalizedProof to have a cleanup action"

  let hoverEntry? ← Lean.Elab.Command.liftCoreM <| Informalize.hoverEntryForDecl? ``phase5Formalized
  let some hoverEntry := hoverEntry?
    | throwError "expected hover metadata for phase5Formalized"
  unless hoverEntry.status == .formalized do
    throwError "expected hover entry status formalized"

  let fileEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesForFileHint "Phase5.lean"
  unless fileEntries.any (·.declName == some ``phase5Placeholder) do
    throwError "expected panel query entries to include phase5Placeholder"
  unless fileEntries.any (·.declName == some ``phase5Formalized) do
    throwError "expected panel query entries to include phase5Formalized"
