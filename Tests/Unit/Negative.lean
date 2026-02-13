import Informalize

/--
error: informal id `Foo` must have at least two components (`File.Section`)
-/
#guard_msgs(error, drop warning) in
example : Nat :=
  informal[Foo]

/--
error: informal id `Missing.bar` points to missing file `informal/Missing.md`
-/
#guard_msgs(error, drop warning) in
example : Nat :=
  informal[Missing.bar]

/--
error: informal id `Foo.missing` is invalid in `informal/Foo.md`: missing section `missing`
-/
#guard_msgs(error, drop warning) in
example : Nat :=
  informal[Foo.missing]

/--
error: informal id `Foo.bar.nope` is invalid in `informal/Foo.md`: missing subsection `nope` under `bar`
-/
#guard_msgs(error, drop warning) in
example : Nat :=
  informal[Foo.bar.nope]

/--
error: `informal` may only be used inside declaration values or proofs
-/
#guard_msgs(error, drop warning) in
#check informal 1 2
