# Informal Id Reference

`informal[<id>] ...args` uses dotted ids.

- `informal a b c` is valid and does not check markdown locations.
- `informal[Foo.bar] a b c` checks markdown location at elaboration time.

- `Foo.bar` -> `informal/Foo.md`, heading `bar`
- `Foo.bar.baz` -> `informal/Foo.md`, heading path `bar` -> `baz`
- deeper ids continue the same subsection rule

Validation happens during elaboration.
