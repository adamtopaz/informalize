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

/--
error: invalid doc reference '#missing-path': doc reference path must be non-empty
-/
#guard_msgs(error, drop warning) in
example : Nat :=
  informal "Malformed ref" from "#missing-path"

/--
error: invalid doc reference 'docs/DocRefs.md#one#two': doc reference must match path[#id]
-/
#guard_msgs(error, drop warning) in
example : Nat :=
  formalized "Malformed ref" from "docs/DocRefs.md#one#two" as Nat.succ 0
