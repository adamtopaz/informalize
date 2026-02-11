import Informalize

namespace Tests.Examples.Workflow

namespace Stage1

noncomputable def GalQ : Nat :=
  informal "Stage 1 placeholder for GalQ"

noncomputable def Out : Nat :=
  informal "Stage 1 placeholder for Out using {GalQ}"

theorem mainSketch : True := by
  informal "Stage 1 theorem sketch"

end Stage1

namespace Stage2

noncomputable def GalQ : Nat :=
  informal "Stage 2 refined sketch for GalQ"

theorem mainSketch : True := by
  have step1 : True := by
    informal "Stage 2 intermediate claim"
  exact step1

end Stage2

namespace Stage3

noncomputable def GalQ : Nat :=
  informal "Stage 3 GalQ remains informal"

def Out : Nat :=
  formalized "Stage 3 Out is now formalized from {GalQ}" as Nat.succ 0

theorem mainPartial : Out = Nat.succ 0 :=
  rfl

theorem bridge : True := by
  informal "Stage 3 bridge theorem still informal"

end Stage3

namespace Stage4

/-- Stage 4 cleaned GalQ definition. -/
def GalQ : Nat :=
  Nat.succ 0

/-- Stage 4 final theorem with no informal dependency. -/
theorem main : GalQ = Nat.succ 0 :=
  rfl

end Stage4

end Tests.Examples.Workflow
