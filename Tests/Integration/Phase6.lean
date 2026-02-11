import Tests.Unit.Phase6

/--
info: 'phase6FinalTheorem' does not depend on any axioms
-/
#guard_msgs(info, drop warning) in
#print axioms phase6FinalTheorem

/--
info: 'phase6InformalTheorem' depends on axioms: [Informalize.Informal]
-/
#guard_msgs(info, drop warning) in
#print axioms phase6InformalTheorem

/--
info: 'phase6FormalTheorem' does not depend on any axioms
-/
#guard_msgs(info, drop warning) in
#print axioms phase6FormalTheorem
