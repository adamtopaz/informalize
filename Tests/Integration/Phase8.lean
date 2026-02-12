import Informalize
import Informalize.Tooling.Blueprint

open Lean

def integrationPhase8FormalizedWithRef : Nat :=
  formalized "Integration phase 8 formalized declaration with doc reference" from "docs/DocRefs.md#phase8-proof" as Nat.succ 80

def integrationPhase8LongSummaryNoRef : Nat :=
  formalized "Integration phase 8 intentionally verbose description without an attached long-form reference so linter coverage can assert the summary-length warning" as Nat.succ 81

def integrationPhase8MissingFileRef : Nat :=
  formalized "Integration phase 8 declaration with missing markdown file" from "docs/MissingDocRef.md#missing-id" as Nat.succ 82

def integrationPhase8NonMarkdownRef : Nat :=
  formalized "Integration phase 8 declaration with non-markdown reference" from "lean-toolchain" as Nat.succ 83

def integrationPhase8MissingMarkerRef : Nat :=
  formalized "Integration phase 8 declaration with missing marker" from "docs/DocRefs.md#does-not-exist" as Nat.succ 84

def integrationPhase8DuplicateMarkerRef : Nat :=
  formalized "Integration phase 8 declaration with duplicate marker" from "docs/DocRefs.md#dup-id" as Nat.succ 85

/--
info: Hover: integrationPhase8FormalizedWithRef
status: formalized
description: Integration phase 8 formalized declaration with doc reference
doc: docs/DocRefs.md#phase8-proof
type: Nat
source: Tests.Integration.Phase8:7:2
-/
#guard_msgs(info, drop warning) in
#informal_hover integrationPhase8FormalizedWithRef

/--
info: Informal panel: Phase8.lean
total: 6
informal: 0
formalized: 6

Entries:
- [formalized] integrationPhase8FormalizedWithRef @ Tests.Integration.Phase8:7:2 - Integration phase 8 formalized declaration with doc reference
  doc: docs/DocRefs.md#phase8-proof
- [formalized] integrationPhase8LongSummaryNoRef @ Tests.Integration.Phase8:10:2 - Integration phase 8 intentionally verbose description without an attached long-form reference...
- [formalized] integrationPhase8MissingFileRef @ Tests.Integration.Phase8:13:2 - Integration phase 8 declaration with missing markdown file
  doc: docs/MissingDocRef.md#missing-id
- [formalized] integrationPhase8NonMarkdownRef @ Tests.Integration.Phase8:16:2 - Integration phase 8 declaration with non-markdown reference
  doc: lean-toolchain
- [formalized] integrationPhase8MissingMarkerRef @ Tests.Integration.Phase8:19:2 - Integration phase 8 declaration with missing marker
  doc: docs/DocRefs.md#does-not-exist
- [formalized] integrationPhase8DuplicateMarkerRef @ Tests.Integration.Phase8:22:2 - Integration phase 8 declaration with duplicate marker
  doc: docs/DocRefs.md#dup-id
-/
#guard_msgs(info, drop warning) in
#informal_panel

run_cmd do
  let mdOutput ← Lean.Elab.Command.liftCoreM Informalize.Tooling.renderBlueprintMarkdown
  unless mdOutput.contains "| Status | Declaration | Type | Description | DocRef | Source |" do
    throwError "expected markdown blueprint to include DocRef column"
  unless mdOutput.contains "docs/DocRefs.md#phase8-proof" do
    throwError "expected markdown blueprint to include doc reference value"

  let jsonText ← Lean.Elab.Command.liftCoreM Informalize.Tooling.renderBlueprintJson
  let jsonData ←
    match Lean.Json.parse jsonText with
    | Except.ok parsed => pure parsed
    | Except.error err => throwError s!"blueprint JSON should parse: {err}"

  let schemaVersion ←
    match (jsonData.getObjVal? "schemaVersion" >>= Lean.Json.getStr?) with
    | Except.ok schemaVersion => pure schemaVersion
    | Except.error err => throwError s!"expected schemaVersion string: {err}"
  unless schemaVersion == "2" do
    throwError "expected schema version 2 in blueprint JSON"

  let entries ←
    match (jsonData.getObjVal? "entries" >>= Lean.Json.getArr?) with
    | Except.ok entries => pure entries
    | Except.error err => throwError s!"expected entries array: {err}"

  let hasDocRefObject := entries.any fun entry =>
    match ((entry.getObjVal? "docRef") >>= (fun docRef => docRef.getObjVal? "raw") >>= Lean.Json.getStr?) with
    | Except.ok raw => raw == "docs/DocRefs.md#phase8-proof"
    | Except.error _ => false
  unless hasDocRefObject do
    throwError "expected at least one entry with docRef object in blueprint JSON"

/--
warning: description for `integrationPhase8LongSummaryNoRef` exceeds 120 characters and has no doc reference
---
warning: doc reference for `integrationPhase8MissingFileRef` points to missing file `docs/MissingDocRef.md`
---
warning: doc reference for `integrationPhase8NonMarkdownRef` points to non-markdown path `lean-toolchain`
---
warning: doc reference for `integrationPhase8MissingMarkerRef` is missing marker `does-not-exist` in `docs/DocRefs.md`
---
warning: doc reference marker `dup-id` appears 2 times in `docs/DocRefs.md`
-/
#guard_msgs(warning, drop info) in
#informal_lint
