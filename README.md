# Informalize

Informalize is now a minimal Lean 4 rewrite centered on a single term elaborator:

```lean
informal[Foo.bar]
informal[Foo.bar] x y
informal[Foo.bar.baz] x y
informal x y
```

When location is provided, `Foo.bar` resolves to section `bar` in `informal/Foo.md`.
`Foo.bar.baz` resolves to subsection `baz` under section `bar` in `informal/Foo.md`.

If the file or heading path does not exist, elaboration fails.
Location is optional: plain `informal ...` skips markdown lookup.

## Core behavior

- `informal` is term-only.
- `informal` is only allowed inside declaration types/values (including proofs).
- location is optional and written as `informal[Foo.bar...]`.
- each use elaborates to an unsound placeholder based on:

```lean
axiom Informalize.Informal.{u} (tag : Lean.Name) (alpha : Sort u) : alpha
```

The elaborator generates a unique `Lean.Name` tag (similar to sorry labels), then
builds an expression equivalent to `Informal <unique_name> ...` with any provided
arguments.

The environment extension tracks declarations that contain `informal` and stores
the deduplicated set of markdown locations referenced in each declaration (possibly
empty when only `informal ...` without brackets is used).

## Repository layout

- `Informalize/Axiom.lean`
- `Informalize/Elaborator.lean`
- `informal/` markdown snippets referenced by ids
- `Tests/Unit/*` rewrite test modules

## Build and tests

```bash
lake build
lake exe tests
```
