# Informalize Command Reference

## CLI equivalents

You can run command-line equivalents outside a Lean file:

```bash
lake exe informalize status --module Tests.Unit.Phase3
lake exe informalize deps --module Tests.Unit.Phase3
lake exe informalize lint --module Tests.Integration.Phase8
lake exe informalize blueprint --module Tests.Unit.Phase4
lake exe informalize blueprint --module Tests.Unit.Phase4 --format json
lake exe informalize code-actions --module Tests.Unit.Phase5 --decl phase5Formalized
lake exe informalize hover --module Tests.Integration.Phase8 --decl integrationPhase8FormalizedWithRef
lake exe informalize panel --module Tests.Integration.Phase8 --file Phase8.lean
```

- `--module` / `-m` is required and repeatable.
- `--format` is only for `blueprint` (`markdown` or `json`).
- `--decl` is used by `code-actions` (optional) and `hover` (required).
- `--file` is required for `panel`.

## `#informal_status`

Prints grouped entries and progress:

- informal entries
- formalized entries
- completion ratio

Each entry includes declaration label, expected type snippet, description, source pointer, and doc reference snippet when present.

## `#informal_deps`

Prints declaration dependency graph for tracked entries.

- shows `decl -> deps`
- includes leaf declarations with no tracked dependencies

This helps prioritize formalization order.

## `#informal_lint`

Runs lints for gradual formalization:

- declarations that transitively reference `Informalize.Informal`
- declarations that transitively reference `sorryAx`
- orphan metadata entries
- long summaries without doc references
- invalid doc references (missing files, non-markdown paths, missing markers, duplicate marker IDs)

The command logs warnings for findings and an info message when no issues are found.

## `#informal_code_actions`

Prints suggested migration edits for all tracked entries.

- informal entries get a `Formalize this` action (`informal` -> `formalized "..." as _`)
- formalized entries get a `Clean up` action (insert doc comment and remove wrapper)

## `#informal_code_actions <decl>`

Prints suggested migration edits for a single declaration.

Example:

```lean
#informal_code_actions myDecl
```

## `#informal_hover <decl>`

Shows hover-style metadata for one declaration:

- status
- description
- doc reference (when present)
- expected type
- source pointer

## `#informal_panel`

Shows a per-file summary panel for the current file in infoview-style text:

- total/informal/formalized counts
- navigable source pointers for each entry
- per-entry `doc:` lines when references are present

## `#informal_panel "<file>"`

Shows the same panel for a file path or basename hint.

## `#export_blueprint`

Exports a Markdown blueprint snapshot as an info message.

Contains:

- schema version
- summary counts
- full entry table
- doc reference column
- dependency graph

## `#export_blueprint "json"`

Exports a JSON blueprint snapshot as an info message.

JSON includes:

- `schemaVersion` (`"2"`)
- `summary`
- `entries`
- `entries[].docRef` (object or `null`)
- `dependencyGraph`
- `dependencyEdges`
