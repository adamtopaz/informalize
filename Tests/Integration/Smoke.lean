import Tests.Unit.Smoke

def integrationWitness : Nat :=
  formalized "Integration smoke term using {unitWitness}" as unitWitness + 1

theorem integrationFormalizedSmoke : True := by
  formalized "Integration-level tactic smoke test" as
    exact trivial

#informal_status
#informal_deps
#export_blueprint
