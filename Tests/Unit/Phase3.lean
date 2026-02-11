import Informalize
import Informalize.Tooling.Status
import Informalize.Tooling.Deps
import Informalize.Tooling.Linter

noncomputable def phase3Base : Nat :=
  informal "Phase 3 base informal declaration"

noncomputable def phase3Formalized : Nat :=
  formalized "Phase 3 formalized declaration referencing {phase3Base}" as
    Nat.succ phase3Base

noncomputable def phase3Dependent : Nat :=
  informal "Phase 3 dependent informal declaration uses {Nat.succ phase3Base}"

def phase3Clean : Nat :=
  Nat.succ 10

theorem phase3InformalTheorem : True := by
  informal "Phase 3 informal theorem"

theorem phase3SorryTheorem : True := by
  sorry

run_cmd do
  let graph ← Lean.Elab.Command.liftCoreM Informalize.Tooling.dependencyGraph
  let some dependentNode := graph.find? (·.1 == ``phase3Dependent)
    | throwError "expected dependency graph node for phase3Dependent"
  unless dependentNode.2.contains ``phase3Base do
    throwError "expected phase3Dependent to depend on phase3Base"

  let leaves ← Lean.Elab.Command.liftCoreM Informalize.Tooling.dependencyLeaves
  unless leaves.contains ``phase3Base do
    throwError "expected phase3Base to appear as an informal dependency leaf"

  let report ← Lean.Elab.Command.liftCoreM Informalize.Tooling.collectReport
  unless report.informalDecls.contains ``phase3Base do
    throwError "expected linter to report phase3Base as referencing Informal"
  unless report.sorryDecls.contains ``phase3SorryTheorem do
    throwError "expected linter to report phase3SorryTheorem as referencing sorryAx"
  unless report.orphanEntries.isEmpty do
    throwError "expected no orphan metadata entries"
