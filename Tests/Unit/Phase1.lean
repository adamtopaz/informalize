import Informalize

noncomputable def simpleDef : Nat :=
  informal "Simple informal definition"

theorem simpleTheorem : True := by
  informal "Simple informal theorem"

section AutoBinding

variable (α : Type)

noncomputable def autoBoundFromExpectedType : α :=
  informal "Auto-bind this placeholder from the expected type"

noncomputable example (β : Type) : β :=
  autoBoundFromExpectedType β

end AutoBinding

section Interpolation

variable (α : Type) (x : α)

noncomputable def interpolationWithComplexTerm : α :=
  informal "Use interpolation with {(fun y => y) x}"

noncomputable def interpolationWithConstants : Nat :=
  informal "Use a nontrivial interpolation term {Nat.succ (Nat.succ 0)}"

noncomputable def interpolationWithMultipleTerms : Nat :=
  informal "Use multiple interpolation terms {Nat.succ 0} and {Nat.pred (Nat.succ 3)}"

noncomputable def inferTypeForSubexpression : Nat :=
  let sketch := informal "Infer this let-bound subexpression from later use"
  Nat.succ sketch

end Interpolation

theorem tacticUsageSmoke : True := by
  have h : True := by
    informal "Informal tactic inside a nested by block"
  exact h

universe u

noncomputable def universePolymorphicPlaceholder (α : Sort u) : α :=
  informal "Universe-polymorphic placeholder for {α}"

noncomputable def uniqLeft : Nat :=
  informal "Unique informal placeholder left"

noncomputable def uniqRight : Nat :=
  informal "Unique informal placeholder right"

example : True := by
  fail_if_success
    have : uniqLeft = uniqRight := by
      rfl
  exact True.intro
