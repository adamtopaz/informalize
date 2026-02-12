import Tests.Unit.Phase4

/--
info: # Informalize Blueprint

- Schema version: 2
- Total entries: 3
- Informal entries: 2
- Formalized entries: 1

## Entries

| Status | Declaration | Type | Description | DocRef | Source |
| --- | --- | --- | --- | --- | --- |
| informal | `phase4Base` | `Nat` | Phase 4 base declaration | (none) | `Tests.Unit.Phase4:10:2` |
| informal | `phase4Dependent` | `Nat` | Phase 4 dependent declaration uses phase4Base.succ | (none) | `Tests.Unit.Phase4:13:2` |
| formalized | `phase4Formalized` | `Nat` | Phase 4 formalized declaration | (none) | `Tests.Unit.Phase4:16:2` |

## Dependency Graph

- `phase4Dependent` -> `phase4Base`
- `phase4Base` -> _(none)_
- `phase4Formalized` -> `phase4Base`
-/
#guard_msgs(info, drop warning) in
#export_blueprint

/--
info: {"summary": {"totalEntries": 3, "informalEntries": 2, "formalizedEntries": 1},
 "schemaVersion": "2",
 "entries":
 [{"status": "informal",
   "sourceInfo":
   {"module": "Tests.Unit.Phase4",
    "line": 10,
    "file": "Phase4.lean",
    "endLine": 10,
    "endColumn": 37,
    "column": 2},
   "referencedConstants": ["Nat"],
   "params": [],
   "expectedType": "Nat",
   "docRef": null,
   "description": "Phase 4 base declaration",
   "declName": "phase4Base"},
  {"status": "informal",
   "sourceInfo":
   {"module": "Tests.Unit.Phase4",
    "line": 13,
    "file": "Phase4.lean",
    "endLine": 13,
    "endColumn": 69,
    "column": 2},
   "referencedConstants": ["Nat.succ", "phase4Base", "Nat"],
   "params": [],
   "expectedType": "Nat",
   "docRef": null,
   "description": "Phase 4 dependent declaration uses phase4Base.succ",
   "declName": "phase4Dependent"},
  {"status": "formalized",
   "sourceInfo":
   {"module": "Tests.Unit.Phase4",
    "line": 16,
    "file": "Phase4.lean",
    "endLine": 16,
    "endColumn": 68,
    "column": 2},
   "referencedConstants": ["Nat"],
   "params": [],
   "expectedType": "Nat",
   "docRef": null,
   "description": "Phase 4 formalized declaration",
   "declName": "phase4Formalized"}],
 "dependencyGraph":
 [{"targets": ["phase4Base"], "declaration": "phase4Dependent"},
  {"targets": [], "declaration": "phase4Base"},
  {"targets": ["phase4Base"], "declaration": "phase4Formalized"}],
 "dependencyEdges":
 [{"to": "phase4Base", "from": "phase4Dependent"},
  {"to": "phase4Base", "from": "phase4Formalized"}]}
-/
#guard_msgs(info, drop warning) in
#export_blueprint "json"
