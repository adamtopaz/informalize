import Tests.Unit.Phase1

noncomputable def BlueprintA : Nat :=
  informal "Blueprint stage A placeholder"

noncomputable def BlueprintB : Nat :=
  informal "Blueprint stage B uses {Nat.succ BlueprintA}"

theorem blueprintGoal : True := by
  informal "Blueprint-level informal theorem"

run_cmd do
  let entries ← Lean.Elab.Command.liftCoreM Informalize.allEntries
  if entries.isEmpty then
    throwError "expected informal metadata entries"

  let some sampleDecl := entries.findSome? (·.declName)
    | throwError "expected at least one metadata entry with an enclosing declaration name"

  let sameDeclEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl sampleDecl
  if sameDeclEntries.isEmpty then
    throwError "expected entriesByDecl to return at least one entry"

  let natSuccRefs ← Lean.Elab.Command.liftCoreM <| Informalize.entriesReferencing ``Nat.succ
  if natSuccRefs.isEmpty then
    throwError "expected at least one entry referencing Nat.succ"

  let (informalCount, _) ← Lean.Elab.Command.liftCoreM Informalize.countsByStatus
  if informalCount == 0 then
    throwError "expected at least one informal entry"
