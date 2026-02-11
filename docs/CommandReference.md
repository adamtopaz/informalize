# Informalize Command Reference

## `#informal_status`

Prints grouped entries and progress:

- informal entries
- formalized entries
- completion ratio

Each entry includes declaration label, expected type snippet, description, and source pointer.

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
- expected type
- source pointer

## `#informal_panel`

Shows a per-file summary panel for the current file in infoview-style text:

- total/informal/formalized counts
- navigable source pointers for each entry

## `#informal_panel "<file>"`

Shows the same panel for a file path or basename hint.

## `#export_blueprint`

Exports a Markdown blueprint snapshot as an info message.

Contains:

- schema version
- summary counts
- full entry table
- dependency graph

## `#export_blueprint "json"`

Exports a JSON blueprint snapshot as an info message.

JSON includes:

- `schemaVersion`
- `summary`
- `entries`
- `dependencyGraph`
- `dependencyEdges`
