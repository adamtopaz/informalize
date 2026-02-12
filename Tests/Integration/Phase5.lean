import Tests.Unit.Phase5

/--
info: Hover: phase5Formalized
status: formalized
description: Phase 5 formalized declaration
type: Nat
source: Tests.Unit.Phase5:7:2
-/
#guard_msgs(info, drop warning) in
#informal_hover phase5Formalized

/--
info: Informal panel: Phase5.lean
total: 3
informal: 1
formalized: 2

Entries:
- [informal] phase5Placeholder @ Tests.Unit.Phase5:4:2 - Phase 5 placeholder declaration
- [formalized] phase5Formalized @ Tests.Unit.Phase5:7:2 - Phase 5 formalized declaration
- [formalized] phase5FormalizedProof @ Tests.Unit.Phase5:13:2 - Phase 5 formalized theorem
-/
#guard_msgs(info, drop warning) in
#informal_panel "Phase5.lean"

/--
info: Code actions for phase5Placeholder (1):
- Formalize this [formalize] for phase5Placeholder @ Tests.Unit.Phase5:4:2
    - suggestion => formalized "Phase 5 placeholder declaration" as _
-/
#guard_msgs(info, drop warning) in
#informal_code_actions phase5Placeholder

/--
info: Code actions for phase5Formalized (1):
- Clean up [cleanup] for phase5Formalized @ Tests.Unit.Phase5:7:2
    - suggestion => phase5Placeholder.succ
    - doc-comment => /-- Phase 5 formalized declaration -/
-/
#guard_msgs(info, drop warning) in
#informal_code_actions phase5Formalized
