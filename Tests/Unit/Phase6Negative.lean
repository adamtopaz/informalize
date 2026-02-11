import Informalize

/--
error: Unknown identifier `doesNotExist`
-/
#guard_msgs(error, drop warning) in
example : True := by
  exact informal "Broken interpolation {doesNotExist}"

/--
error: don't know how to synthesize placeholder
context:
⊢ Nat
-/
#guard_msgs(error, drop warning) in
example : Nat :=
  formalized "Missing body" as _
