import Tests.Integration.Phase3

run_cmd do
  let allEntries ← Lean.Elab.Command.liftCoreM Informalize.allEntries
  if allEntries.size != 4 then
    throwError s!"expected exactly 4 imported metadata entries, got {allEntries.size}"

  let informalEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByStatus .informal
  if informalEntries.size != 3 then
    throwError s!"expected exactly 3 informal entries, got {informalEntries.size}"

  let formalizedEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByStatus .formalized
  if formalizedEntries.size != 1 then
    throwError s!"expected exactly 1 formalized entry, got {formalizedEntries.size}"

  let phase3BaseEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``phase3Base
  if phase3BaseEntries.size != 1 then
    throwError s!"expected a single metadata entry for phase3Base, got {phase3BaseEntries.size}"

  let phase3FormalizedEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``phase3Formalized
  if phase3FormalizedEntries.size != 1 then
    throwError s!"expected a single metadata entry for phase3Formalized, got {phase3FormalizedEntries.size}"
