import Informalize

noncomputable def migrationBefore : Nat :=
  informal "Lifecycle phase before migration"

def migrationAfter : Nat :=
  formalized "Lifecycle phase after migration" as Nat.succ 0

/-- Clean lifecycle stage declaration without wrappers. -/
def migrationClean : Nat :=
  Nat.succ 1

theorem formalizedTacticProof : True := by
  formalized "Formalized tactic proof step" as
    exact True.intro

def formalizedWithInterpolation : Nat :=
  formalized "Formalized interpolation using {Nat.succ migrationAfter}" as
    Nat.succ migrationAfter

#guard_expr (formalized "Body semantics" as Nat.succ 2) = Nat.succ 2

run_cmd do
  let beforeEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``migrationBefore
  unless beforeEntries.any (·.status == .informal) do
    throwError "expected migrationBefore to be tracked as informal"

  let afterEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``migrationAfter
  unless afterEntries.any (·.status == .formalized) do
    throwError "expected migrationAfter to be tracked as formalized"

  let cleanEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``migrationClean
  unless cleanEntries.isEmpty do
    throwError "expected migrationClean to have no informal metadata"

  let tacticEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``formalizedTacticProof
  unless tacticEntries.any (·.status == .formalized) do
    throwError "expected formalized tactic theorem metadata"

  let interpolationEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``formalizedWithInterpolation
  unless interpolationEntries.any (fun entry => entry.referencedConstants.contains ``Nat.succ) do
    throwError "expected interpolation constants to include Nat.succ"

  let some beforeEntry := beforeEntries[0]?
    | throwError "expected at least one entry for migrationBefore"
  let sameFileEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByFile beforeEntry.sourceInfo.fileName
  unless sameFileEntries.any (fun entry => entry.declName == some ``migrationBefore) do
    throwError "expected entriesByFile to include migrationBefore"

  let natSuccRefs ← Lean.Elab.Command.liftCoreM <| Informalize.entriesReferencing ``Nat.succ
  unless natSuccRefs.any (fun entry => entry.declName == some ``formalizedWithInterpolation) do
    throwError "expected entriesReferencing Nat.succ to include formalizedWithInterpolation"

  let (informalCount, formalizedCount) ← Lean.Elab.Command.liftCoreM Informalize.countsByStatus
  if informalCount == 0 then
    throwError "expected at least one informal metadata entry"
  if formalizedCount == 0 then
    throwError "expected at least one formalized metadata entry"

  let env ← Lean.Elab.Command.liftCoreM Lean.getEnv
  let state := Informalize.informalExt.getState env
  unless state.entries.any (fun entry =>
      entry.declName == some ``migrationBefore && entry.status == .informal) do
    throwError "expected extension state entry for migrationBefore"
  unless state.entries.any (fun entry =>
      entry.declName == some ``migrationAfter && entry.status == .formalized) do
    throwError "expected extension state entry for migrationAfter"
