import Informalize
import Informalize.CodeAction
import Informalize.Tooling.Status
import Informalize.Tooling.Deps
import Informalize.Tooling.Blueprint
import Informalize.Tooling.Linter

theorem unitInformalSmoke : True := by
  informal "Unit-level tactic smoke test"

def unitWitness : Nat :=
  formalized "Unit-level term smoke test" as 7
