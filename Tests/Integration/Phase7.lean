import Tests.Examples.Workflow

run_cmd do
  let stage1GalQEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``Tests.Examples.Workflow.Stage1.GalQ
  unless stage1GalQEntries.any (·.status == .informal) do
    throwError "expected Stage1.GalQ to be tracked as informal"

  let stage3OutEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``Tests.Examples.Workflow.Stage3.Out
  unless stage3OutEntries.any (·.status == .formalized) do
    throwError "expected Stage3.Out to be tracked as formalized"

  let stage4GalQEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``Tests.Examples.Workflow.Stage4.GalQ
  unless stage4GalQEntries.isEmpty do
    throwError "expected cleaned Stage4.GalQ to have no metadata entry"

  let refsToStage1GalQ ← Lean.Elab.Command.liftCoreM <| Informalize.entriesReferencing ``Tests.Examples.Workflow.Stage1.GalQ
  unless refsToStage1GalQ.any (·.declName == some ``Tests.Examples.Workflow.Stage1.Out) do
    throwError "expected Stage1.Out to reference Stage1.GalQ via interpolation dependency"

/--
info: 'Tests.Examples.Workflow.Stage4.main' does not depend on any axioms
-/
#guard_msgs(info, drop warning) in
#print axioms Tests.Examples.Workflow.Stage4.main

/--
info: 'Tests.Examples.Workflow.Stage1.mainSketch' depends on axioms: [Informalize.Informal]
-/
#guard_msgs(info, drop warning) in
#print axioms Tests.Examples.Workflow.Stage1.mainSketch
