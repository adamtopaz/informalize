import Informalize

noncomputable def phase8InformalWithRef : Nat :=
  informal "Phase 8 informal declaration with a long-form reference" from "docs/DocRefs.md#phase8-sketch"

def phase8FormalizedWithRef : Nat :=
  formalized "Phase 8 formalized declaration with a long-form reference" from "docs/DocRefs.md#phase8-proof" as Nat.succ 8

theorem phase8InformalTacticWithRef : True := by
  informal "Phase 8 tactic sketch with a long-form reference" from "docs/DocRefs.md#phase8-tactic"

theorem phase8FormalizedTacticWithRef : True := by
  formalized "Phase 8 tactic proof with a long-form reference" from "docs/DocRefs.md#phase8-tactic-proof" as
    exact True.intro

noncomputable def phase8FullFileRef : Nat :=
  informal "Phase 8 declaration referencing a full markdown file" from "docs/DocRefs.md"

noncomputable def phase8MissingFileRef : Nat :=
  informal "Phase 8 declaration with missing markdown file" from "docs/MissingDocRef.md#missing-id"

noncomputable def phase8NonMarkdownRef : Nat :=
  informal "Phase 8 declaration with non-markdown reference" from "lean-toolchain"

noncomputable def phase8MissingMarkerRef : Nat :=
  informal "Phase 8 declaration with missing marker" from "docs/DocRefs.md#does-not-exist"

noncomputable def phase8DuplicateMarkerRef : Nat :=
  informal "Phase 8 declaration with duplicate marker" from "docs/DocRefs.md#dup-id"

noncomputable def phase8LongSummaryNoRef : Nat :=
  informal "Phase 8 intentionally verbose description without an attached long-form reference so the linter can enforce the summary-length guardrail"

run_cmd do
  let withRefEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``phase8InformalWithRef
  let some withRefEntry := withRefEntries.find? (·.status == .informal)
    | throwError "expected phase8InformalWithRef metadata entry"
  let some withRefDoc := withRefEntry.docRef?
    | throwError "expected phase8InformalWithRef to include doc reference metadata"
  unless withRefDoc.path == "docs/DocRefs.md" && withRefDoc.id? == some "phase8-sketch" do
    throwError "unexpected parsed doc reference for phase8InformalWithRef"

  let fullFileEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``phase8FullFileRef
  let some fullFileEntry := fullFileEntries.find? (·.status == .informal)
    | throwError "expected phase8FullFileRef metadata entry"
  let some fullFileDoc := fullFileEntry.docRef?
    | throwError "expected phase8FullFileRef to include doc reference metadata"
  unless fullFileDoc.path == "docs/DocRefs.md" && fullFileDoc.id?.isNone do
    throwError "expected full-file doc reference without marker id"

  let formalizedEntries ← Lean.Elab.Command.liftCoreM <| Informalize.entriesByDecl ``phase8FormalizedWithRef
  let some formalizedEntry := formalizedEntries.find? (·.status == .formalized)
    | throwError "expected phase8FormalizedWithRef metadata entry"
  unless formalizedEntry.docRef? == some { path := "docs/DocRefs.md", id? := some "phase8-proof", raw := "docs/DocRefs.md#phase8-proof" } do
    throwError "expected formalized metadata to retain doc reference"

  let encoded := Informalize.encodeEntryMetadata formalizedEntry
  let decoded? := Informalize.decodeEntryMetadata? formalizedEntry.declName encoded
  let some decoded := decoded?
    | throwError "expected metadata decode to succeed"
  unless decoded.docRef? == formalizedEntry.docRef? do
    throwError "expected metadata encode/decode round-trip to preserve doc reference"

/--
info: Code actions for phase8InformalWithRef (1):
- Formalize this [formalize] for phase8InformalWithRef @ Tests.Unit.Phase8:4:2
    - suggestion => formalized "Phase 8 informal declaration with a long-form reference" from "docs/DocRefs.md#phase8-sketch" as _
-/
#guard_msgs(info, drop warning) in
#informal_code_actions phase8InformalWithRef

/--
info: Code actions for phase8FormalizedWithRef (1):
- Clean up [cleanup] for phase8FormalizedWithRef @ Tests.Unit.Phase8:7:2
    - suggestion => Nat.succ 8
    - doc-comment => /-- Phase 8 formalized declaration with a long-form reference  See: docs/DocRefs.md#phase8-proof -/
-/
#guard_msgs(info, drop warning) in
#informal_code_actions phase8FormalizedWithRef
