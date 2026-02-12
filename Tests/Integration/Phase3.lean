import Tests.Unit.Phase3

run_cmd do
  let entries ← Lean.Elab.Command.liftCoreM Informalize.allEntries
  let informalEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByStatus .informal
  let formalizedEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByStatus .formalized

  if entries.isEmpty then
    throwError "expected metadata entries for phase 3 integration"
  if informalEntries.isEmpty then
    throwError "expected informal entries for phase 3 integration"
  if formalizedEntries.isEmpty then
    throwError "expected formalized entries for phase 3 integration"

  let cleanEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``phase3Clean
  unless cleanEntries.isEmpty do
    throwError "expected phase3Clean to remain metadata-free"

/--
info: Informal (3):
  phase3Base : Nat - "Phase 3 base informal declaration" @ Tests.Unit.Phase3:7:2
  phase3Dependent : Nat - "Phase 3 dependent informal declaration uses phase3Base.succ" @ Tests.Unit.Phase3:14:2
  phase3InformalTheorem : True - "Phase 3 informal theorem" @ Tests.Unit.Phase3:20:2

Formalized (1):
  phase3Formalized : Nat - "Phase 3 formalized declaration referencing phase3Base" @ Tests.Unit.Phase3:10:2

Progress: 1/4 (25%)
-/
#guard_msgs(info, drop warning) in
#informal_status

/--
info: Informal dependency graph:
  phase3Dependent -> phase3Base
  phase3InformalTheorem -> (none)
  phase3Base -> (none)

Leaves (no informal dependencies): phase3InformalTheorem, phase3Base
-/
#guard_msgs(info, drop warning) in
#informal_deps

/--
info: informal linter found no issues
-/
#guard_msgs(info, drop warning) in
#informal_lint
