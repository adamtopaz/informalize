# Informalize FAQ

## Why is `Informalize.Informal` unsound?

It is intentionally an axiom-backed placeholder mechanism for incremental development.
The goal is to keep files compiling while formalization is in progress.

## When do I need `noncomputable`?

If a definition depends on an `informal` placeholder, mark the definition `noncomputable`.
This matches Lean's usual requirements for axiom-based or non-executable content.

## Are `informal` placeholders unique?

Yes. Each placeholder carries a unique tag so two placeholders at the same type are not defeq by default.

## Do interpolation terms affect dependencies?

Yes. Constants referenced in interpolation expressions are tracked as dependency signals.

## How does this interact with instances and implicit parameters?

`informal` captures free locals from both interpolation expressions and expected types.
This supports instance-style declarations where relevant variables are implicit in type context.

## What about universes?

`Informal` is universe-polymorphic, and tests cover universe-polymorphic expected-type scenarios.

## Can dependencies contain cycles?

Yes. Mutual or cyclic relationships are tolerated by dependency tooling and should not crash reporting.

## Is `sorry` treated the same as `informal`?

No. Linter output reports `Informal` and `sorry` usage separately so cleanup plans stay explicit.

## How do I use editor-style helpers today?

Use:

- `#informal_code_actions` for migration suggestions,
- `#informal_hover <decl>` for metadata hover text,
- `#informal_panel` for per-file progress summaries.

## How do long-form markdown references work?

Attach `from "path[#id]"` to `informal` or `formalized`.

- `path` should be a repo-relative markdown file.
- `#id` is optional; if present it targets marker `<!-- informalize:id=<id> -->`.

Example marker:

```md
<!-- informalize:id=my-proof -->
Long-form notes.
```

`#informal_lint` warns when references are invalid (missing file, non-markdown path, missing marker, duplicate marker ids).

## How do I know I'm fully formalized?

Use `#print axioms` on final targets and confirm `Informalize.Informal` is absent.
