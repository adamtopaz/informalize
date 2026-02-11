import Informalize.Axiom
import Informalize.Extension
import Informalize.Elaborator
import Informalize.Tooling.Blueprint
import Lean.Data.Json.Parser

open Lean

noncomputable def phase4Base : Nat :=
  informal "Phase 4 base declaration"

noncomputable def phase4Dependent : Nat :=
  informal "Phase 4 dependent declaration uses {Nat.succ phase4Base}"

noncomputable def phase4Formalized : Nat :=
  formalized "Phase 4 formalized declaration" as Nat.succ phase4Base

def phase4Clean : Nat :=
  Nat.succ 41

run_cmd do
  let mdOutput ← Lean.Elab.Command.liftCoreM Informalize.Tooling.renderBlueprintMarkdown
  unless mdOutput.contains "# Informalize Blueprint" do
    throwError "expected markdown blueprint header"
  unless mdOutput.contains "`phase4Base`" do
    throwError "expected markdown export to include phase4Base"
  unless mdOutput.contains "`phase4Dependent` -> `phase4Base`" do
    throwError "expected markdown export dependency edge phase4Dependent -> phase4Base"

  let jsonText ← Lean.Elab.Command.liftCoreM Informalize.Tooling.renderBlueprintJson
  let jsonData ←
    match Lean.Json.parse jsonText with
    | Except.ok parsed => pure parsed
    | Except.error err => throwError s!"blueprint JSON should parse: {err}"

  let schemaVersion ←
    match (jsonData.getObjVal? "schemaVersion" >>= Lean.Json.getStr?) with
    | Except.ok schemaVersion => pure schemaVersion
    | Except.error err => throwError s!"expected schemaVersion string: {err}"
  unless schemaVersion == Informalize.Tooling.schemaVersion do
    throwError "unexpected schema version in blueprint JSON"

  let entries ←
    match (jsonData.getObjVal? "entries" >>= Lean.Json.getArr?) with
    | Except.ok entries => pure entries
    | Except.error err => throwError s!"expected entries array: {err}"
  if entries.isEmpty then
    throwError "expected non-empty entries array"

  let hasPhase4Base := entries.any fun entry =>
    match (entry.getObjVal? "declName" >>= Lean.Json.getStr?) with
    | Except.ok declName => declName == "phase4Base"
    | Except.error _ => false
  unless hasPhase4Base do
    throwError "expected JSON entries to include phase4Base"

  let edges ←
    match (jsonData.getObjVal? "dependencyEdges" >>= Lean.Json.getArr?) with
    | Except.ok edges => pure edges
    | Except.error err => throwError s!"expected dependencyEdges array: {err}"

  let hasExpectedEdge := edges.any fun edge =>
    match (edge.getObjVal? "from" >>= Lean.Json.getStr?), (edge.getObjVal? "to" >>= Lean.Json.getStr?) with
    | Except.ok src, Except.ok dst => src == "phase4Dependent" && dst == "phase4Base"
    | _, _ => false
  unless hasExpectedEdge do
    throwError "expected dependency edge phase4Dependent -> phase4Base in JSON export"
