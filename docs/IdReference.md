# Informal Id Reference

`informal[<id>] ...args` uses dotted ids.

- `informal a b c` is valid and does not check markdown locations.
- `informal[Foo.bar] a b c` checks markdown location at elaboration time.

- `Foo.bar` -> `informal/Foo.md`, heading `bar`
- `Foo.bar.baz` -> `informal/Foo.md`, heading path `bar` -> `baz`
- deeper ids continue the same subsection rule

Validation happens during elaboration.

Each successful elaboration is tracked in an environment extension keyed by
declaration name, with a deduplicated location set for that declaration.

You can inspect this data with the CLI:

- `lake exe informalize status --module <Module.Name>`
- `lake exe informalize deps --module <Module.Name>`
- `lake exe informalize decls --module <Module.Name>`
- `lake exe informalize decl --module <Module.Name> --decl <Decl.Name>`
- `lake exe informalize locations --module <Module.Name>`
- `lake exe informalize location --module <Module.Name> --location <Location.Name>`
