# End-to-End Workflow Example

The file `Tests/Examples/Workflow.lean` demonstrates the Section 10 lifecycle in one place:

- `Examples.Workflow.Stage1` uses `informal` placeholders.
- `Examples.Workflow.Stage2` refines the proof sketch while still informal.
- `Examples.Workflow.Stage3` mixes informal and formalized declarations.
- `Examples.Workflow.Stage4` is clean and fully formalized.

## Suggested checks

```lean
#print axioms Tests.Examples.Workflow.Stage4.main
#print axioms Tests.Examples.Workflow.Stage1.mainSketch
```

Expected behavior:

- Stage 4 theorem has no `Informalize.Informal` dependency.
- Stage 1 theorem still reports `Informalize.Informal`.

You can also inspect metadata/tooling output with:

```lean
#informal_status
#informal_deps
#export_blueprint
```
