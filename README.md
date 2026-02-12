# Informalize

Informalize is a Lean 4 framework for gradual formalization.
It lets you start with natural-language placeholders that still typecheck,
then progressively replace them with formal Lean terms and proofs.

## What this gives you

- `informal "..."` placeholders that keep projects compiling.
- `formalized "..." as ...` migration wrappers that preserve prose context.
- Optional long-form markdown references via `from "path[#id]"`.
- Metadata tracking for status, source locations, dependencies, and descriptions.
- Tooling commands for progress dashboards, dependency views, linting, exports,
  and editor-style assistance.

## Important safety note

Informalize is intentionally unsound while placeholders remain.
It relies on:

```lean
axiom Informalize.Informal.{u} (alpha : Sort u) : alpha
```

Use this framework for staged development, and treat
`#print axioms <target>` as your final gate before claiming full formalization.

## Lifecycle model

Typical declaration lifecycle:

```lean
-- Stage 1: informal
noncomputable def GalQ : Nat :=
  informal "The absolute Galois group of Q"

-- Stage 2: formalized
def GalQ : Nat :=
  formalized "The absolute Galois group of Q" as Nat.succ 0

-- Stage 3: clean
/-- The absolute Galois group of Q -/
def GalQ : Nat :=
  Nat.succ 0
```

## Quick start

This repo uses Lean toolchain `leanprover/lean4:v4.27.0`.

```bash
lake build
lake exe tests
```

Minimal usage sketch:

```lean
import Informalize

noncomputable def seed : Nat :=
  informal "initial sketch"

theorem seed_ok : True := by
  informal "initial proof sketch"

def seed' : Nat :=
  formalized "implemented version" as Nat.succ 0
```

## Core syntax

### Term forms

- `informal "description with {interpolations}"`
- `informal "description with {interpolations}" from "docs/DocRefs.md#my-id"`
- `formalized "description with {interpolations}" as <term>`
- `formalized "description with {interpolations}" from "docs/DocRefs.md#my-id" as <term>`

### Tactic forms

- `informal "description with {interpolations}"`
- `informal "description with {interpolations}" from "docs/DocRefs.md#my-id"`
- `formalized "description with {interpolations}" as <tacticSeq>`
- `formalized "description with {interpolations}" from "docs/DocRefs.md#my-id" as <tacticSeq>`

Interpolations can be arbitrary Lean terms. Informalize records constants from
these terms for dependency tracking.

For long-form notes, point to markdown files with stable markers:

```md
<!-- informalize:id=my-id -->
Long-form explanation here.
```

References use repo-relative paths and optional marker ids:

- `from "docs/DocRefs.md"` (whole file)
- `from "docs/DocRefs.md#my-id"` (specific marker)

## Command reference (quick)

### Progress and dependency tooling

- `#informal_status`
  - grouped informal/formalized entries
  - progress ratio
- `#informal_deps`
  - dependency graph among tracked declarations
  - leaf declarations for prioritization
- `#informal_lint`
  - declarations using `Informalize.Informal`
  - declarations using `sorryAx`
  - orphan metadata entries

### Blueprint export

- `#export_blueprint` (Markdown)
- `#export_blueprint "json"` (JSON)

JSON includes:

- `schemaVersion`
- `summary`
- `entries`
- `dependencyGraph`
- `dependencyEdges`

### Editor-style helper commands

- `#informal_code_actions`
- `#informal_code_actions myDecl`
- `#informal_hover myDecl`
- `#informal_panel`
- `#informal_panel "MyFile.lean"`

These provide command-driven equivalents of code actions, hover details, and
per-file infoview status.

## Soundness gate and completion checks

Recommended final checks:

```lean
#print axioms MyFinalTheorem
```

Target should not depend on `Informalize.Informal`.

Also run:

```bash
lake build
lake exe tests
```

The test suite includes staged lifecycle coverage and `#print axioms`
regressions through Phase 7.

## Repository layout

Core library:

- `Informalize/Axiom.lean`
- `Informalize/Elaborator.lean`
- `Informalize/Extension.lean`
- `Informalize/CodeAction.lean`
- `Informalize/Tooling/Status.lean`
- `Informalize/Tooling/Deps.lean`
- `Informalize/Tooling/Blueprint.lean`
- `Informalize/Tooling/Linter.lean`
- `Informalize.lean`

Tests and examples:

- `Tests/Unit/Phase1.lean` ... `Tests/Unit/Phase8.lean`
- `Tests/Integration/Phase1.lean` ... `Tests/Integration/Phase8.lean`
- `Tests/Examples/Workflow.lean`
- `Tests.lean`

## Additional documentation

- `docs/UserGuide.md`
- `docs/CommandReference.md`
- `docs/FAQ.md`
- `docs/WorkflowExample.md`

## Current status

The planned phases are implemented, including extra hardening phases beyond the
original 1-5 roadmap (completion gate tests, full workflow examples, and doc
reference coverage).
