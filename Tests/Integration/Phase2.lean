import Tests.Unit.Phase2

run_cmd do
  let allEntries ← Lean.Elab.Command.liftCoreM Informalize.allEntries
  let informalEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByStatus .informal
  let formalizedEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByStatus .formalized

  if informalEntries.isEmpty then
    throwError "expected informal entries in mixed-status project"
  if formalizedEntries.isEmpty then
    throwError "expected formalized entries in mixed-status project"

  if allEntries.size != informalEntries.size + formalizedEntries.size then
    throwError "status partition does not match all metadata entries"

  let migrationRefs ← Lean.Elab.Command.liftCoreM <| Informalize.entriesReferencing ``migrationAfter
  if migrationRefs.isEmpty then
    throwError "expected at least one metadata entry referencing migrationAfter"

  let cleanEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``migrationClean
  unless cleanEntries.isEmpty do
    throwError "expected clean-stage declaration to have no metadata entries"

  let env ← Lean.Elab.Command.liftCoreM Lean.getEnv
  let state := Informalize.informalExt.getState env
  unless state.entries.any (fun entry => entry.sourceInfo.moduleName == `Tests.Unit.Phase2) do
    throwError "expected extension state to include entries imported from Tests.Unit.Phase2"
