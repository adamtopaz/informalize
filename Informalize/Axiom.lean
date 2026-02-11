namespace Informalize

/--
Unsound placeholder axiom used during gradual formalization.
Every phase after bootstrap is expected to reduce dependencies on this constant.
-/
axiom Informal.{u} (alpha : Sort u) : alpha

end Informalize
