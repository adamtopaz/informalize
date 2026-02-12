# Informalize User Guide

This guide describes the gradual formalization workflow:

1. start with `informal` placeholders,
2. migrate to `formalized` implementations,
3. remove wrappers and keep documentation comments.

## Stage 1: Start Informal

Use `informal` when you want code to typecheck before writing the full proof/term.

```lean
noncomputable def seed : Nat :=
  informal "initial placeholder"

theorem seed_ok : True := by
  informal "initial proof placeholder"
```

The command layer tracks these placeholders as `informal` metadata entries.

## Stage 2: Migrate to Formalized

Replace a placeholder with a real implementation while preserving explanation text.

```lean
def seed : Nat :=
  formalized "now fully implemented" as Nat.succ 0
```

For tactic proofs:

```lean
theorem seed_ok : True := by
  formalized "proof completed" as
    exact True.intro
```

`formalized` keeps behavior equal to the provided body and records status as `formalized`.

### Optional long-form references

If the short inline summary is not enough, attach a markdown reference:

```lean
def seed : Nat :=
  formalized "now fully implemented" from "docs/DocRefs.md#seed-proof" as Nat.succ 0
```

References use `path[#id]`:

- `path` is repo-relative markdown file path.
- `#id` is optional; omit it to reference the full file.

In markdown, define stable marker ids explicitly:

```md
<!-- informalize:id=seed-proof -->
Long-form proof notes for `seed`.
```

## Stage 3: Clean Final Code

After the declaration is fully formalized and stable, remove wrappers and keep the description as doc text.

```lean
/-- now fully implemented -/
def seed : Nat :=
  Nat.succ 0
```

Clean declarations do not create new metadata entries.

## Stage 4: Editor Assistance

Use command-driven editor helpers while migrating:

```lean
#informal_code_actions
#informal_hover myDecl
#informal_panel
```

- `#informal_code_actions` suggests `formalize`/`cleanup` edits.
- `#informal_hover` shows stored description/status/type for a declaration.
- `#informal_panel` shows per-file counts and source pointers for quick navigation.

For a complete staged walkthrough, see `Tests/Examples/Workflow.lean` and `docs/WorkflowExample.md`.

## Workflow Checks

- `#informal_status` shows informal/formalized progress.
- `#informal_deps` shows dependency edges between tracked declarations.
- `#informal_lint` reports `Informal` users, `sorry` users, and orphan entries.
- `#informal_lint` also checks doc references (missing files, non-markdown paths, missing/duplicate marker ids) and warns on long summaries without doc refs.
- `#informal_code_actions` suggests migration edits.
- `#informal_hover <decl>` prints hover metadata for a declaration.
- `#informal_panel` shows per-file summary and navigation pointers.
- `#export_blueprint` and `#export_blueprint "json"` export project snapshots.

Outside Lean files, use the CLI equivalents:

- `lake exe informalize status --module <Module.Name>`
- `lake exe informalize deps --module <Module.Name>`
- `lake exe informalize lint --module <Module.Name>`
- `lake exe informalize blueprint --module <Module.Name> [--format markdown|json]`
- `lake exe informalize code-actions --module <Module.Name> [--decl <Decl.Name>]`
- `lake exe informalize hover --module <Module.Name> --decl <Decl.Name>`
- `lake exe informalize panel --module <Module.Name> --file <FileHint>`

## Final Soundness Gate

For target theorems meant to be fully formalized, check:

```lean
#print axioms TargetTheorem
```

The final output should not include `Informalize.Informal`.
