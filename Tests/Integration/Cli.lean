import Informalize.Cli
import Lean.Data.Json.Parser

open Lean Elab Command

private def assertContains (label output needle : String) : CommandElabM Unit := do
  unless output.contains needle do
    throwError s!"{label}: expected output to contain `{needle}`"

private def assertExitCode
    (label : String)
    (result : Informalize.Cli.InvocationResult)
    (expected : UInt32) : CommandElabM Unit := do
  unless result.exitCode == expected do
    throwError s!"{label}: expected exit code {expected}, got {result.exitCode}"

run_cmd do
  let help := ← liftIO <| Informalize.Cli.invoke #["--help"]
  assertExitCode "help command" help 0
  assertContains "help command" help.stdout "Informalize CLI"
  assertContains "help command" help.stdout "status"
  assertContains "help command" help.stdout "blueprint"
  assertContains "help command" help.stdout "code-actions"

run_cmd do
  let missingModule := ← liftIO <| Informalize.Cli.invoke #["status"]
  assertExitCode "missing module" missingModule 1
  assertContains "missing module" missingModule.stderr "missing required option `--module <Module.Name>`"

run_cmd do
  let unknownCommand := ← liftIO <| Informalize.Cli.invoke #["bogus", "--module", "Tests.Unit.Phase3"]
  assertExitCode "unknown command" unknownCommand 1
  assertContains "unknown command" unknownCommand.stderr "unknown command `bogus`"

run_cmd do
  let badFormat := ← liftIO <| Informalize.Cli.invoke #["blueprint", "--module", "Tests.Unit.Phase4", "--format", "yaml"]
  assertExitCode "unknown blueprint format" badFormat 1
  assertContains "unknown blueprint format" badFormat.stderr "expected `markdown` or `json`"

run_cmd do
  let badDeclFlag := ← liftIO <| Informalize.Cli.invoke #["status", "--module", "Tests.Unit.Phase3", "--decl", "phase3Base"]
  assertExitCode "bad decl flag" badDeclFlag 1
  assertContains "bad decl flag" badDeclFlag.stderr "`--decl` is only valid for `code-actions` and `hover`"

run_cmd do
  let status := ← liftIO <| Informalize.Cli.invoke #["status", "--module", "Tests.Unit.Phase3"]
  assertExitCode "status command" status 0
  assertContains "status command" status.stdout "Informal (3):"
  assertContains "status command" status.stdout "Formalized (1):"
  assertContains "status command" status.stdout "Progress: 1/4 (25%)"

run_cmd do
  let deps := ← liftIO <| Informalize.Cli.invoke #["deps", "--module", "Tests.Unit.Phase3"]
  assertExitCode "deps command" deps 0
  assertContains "deps command" deps.stdout "phase3Dependent -> phase3Base"
  assertContains "deps command" deps.stdout "Leaves (no informal dependencies):"

run_cmd do
  let blueprint := ← liftIO <| Informalize.Cli.invoke #["blueprint", "--module", "Tests.Unit.Phase4"]
  assertExitCode "blueprint markdown" blueprint 0
  assertContains "blueprint markdown" blueprint.stdout "# Informalize Blueprint"
  assertContains "blueprint markdown" blueprint.stdout "`phase4Dependent` -> `phase4Base`"

run_cmd do
  let blueprintJson := ← liftIO <| Informalize.Cli.invoke #["blueprint", "--module", "Tests.Unit.Phase4", "--format", "json"]
  assertExitCode "blueprint json" blueprintJson 0

  let json ←
    match Lean.Json.parse blueprintJson.stdout with
    | Except.ok json =>
      pure json
    | Except.error err =>
      throwError s!"blueprint json should parse: {err}"

  let schemaVersion ←
    match (json.getObjVal? "schemaVersion" >>= Lean.Json.getStr?) with
    | Except.ok schemaVersion =>
      pure schemaVersion
    | Except.error err =>
      throwError s!"expected schemaVersion string: {err}"

  unless schemaVersion == "2" do
    throwError "expected schemaVersion 2 for CLI blueprint JSON"

  let entries ←
    match (json.getObjVal? "entries" >>= Lean.Json.getArr?) with
    | Except.ok entries =>
      pure entries
    | Except.error err =>
      throwError s!"expected entries array: {err}"

  unless entries.any (fun entry =>
      match (entry.getObjVal? "declName" >>= Lean.Json.getStr?) with
      | Except.ok declName => declName == "phase4Base"
      | Except.error _ => false) do
    throwError "expected phase4Base in CLI blueprint JSON"

run_cmd do
  let lintPhase3 := ← liftIO <| Informalize.Cli.invoke #["lint", "--module", "Tests.Unit.Phase3"]
  assertExitCode "lint phase3" lintPhase3 0
  assertContains "lint phase3" lintPhase3.stdout "phase3SorryTheorem"
  assertContains "lint phase3" lintPhase3.stdout "sorryAx"
  assertContains "lint phase3" lintPhase3.stdout "Informalize.Informal"

run_cmd do
  let lintPhase8 := ← liftIO <| Informalize.Cli.invoke #["lint", "--module", "Tests.Integration.Phase8"]
  assertExitCode "lint phase8" lintPhase8 0
  assertContains "lint phase8" lintPhase8.stdout "integrationPhase8LongSummaryNoRef"
  assertContains "lint phase8" lintPhase8.stdout "docs/MissingDocRef.md"
  assertContains "lint phase8" lintPhase8.stdout "non-markdown path `lean-toolchain`"
  assertContains "lint phase8" lintPhase8.stdout "missing marker `does-not-exist`"
  assertContains "lint phase8" lintPhase8.stdout "appears 2 times"

run_cmd do
  let codeActionsAll := ← liftIO <| Informalize.Cli.invoke #["code-actions", "--module", "Tests.Unit.Phase5"]
  assertExitCode "code-actions all" codeActionsAll 0
  assertContains "code-actions all" codeActionsAll.stdout "Code actions (3):"
  assertContains "code-actions all" codeActionsAll.stdout "Formalize this [formalize]"
  assertContains "code-actions all" codeActionsAll.stdout "phase5Placeholder"
  assertContains "code-actions all" codeActionsAll.stdout "Clean up [cleanup]"

run_cmd do
  let codeActionsDecl := ← liftIO <| Informalize.Cli.invoke #["code-actions", "--module", "Tests.Unit.Phase5", "--decl", "phase5Formalized"]
  assertExitCode "code-actions decl" codeActionsDecl 0
  assertContains "code-actions decl" codeActionsDecl.stdout "Code actions for phase5Formalized (1):"
  assertContains "code-actions decl" codeActionsDecl.stdout "Clean up [cleanup]"

run_cmd do
  let hover := ← liftIO <| Informalize.Cli.invoke #["hover", "--module", "Tests.Integration.Phase8", "--decl", "integrationPhase8FormalizedWithRef"]
  assertExitCode "hover command" hover 0
  assertContains "hover command" hover.stdout "Hover: integrationPhase8FormalizedWithRef"
  assertContains "hover command" hover.stdout "status: formalized"
  assertContains "hover command" hover.stdout "doc: docs/DocRefs.md#phase8-proof"

run_cmd do
  let missingDecl := ← liftIO <| Informalize.Cli.invoke #["hover", "--module", "Tests.Integration.Phase8"]
  assertExitCode "hover missing decl" missingDecl 1
  assertContains "hover missing decl" missingDecl.stderr "missing required option `--decl <Decl.Name>` for `hover`"

run_cmd do
  let panel := ← liftIO <| Informalize.Cli.invoke #["panel", "--module", "Tests.Integration.Phase8", "--file", "Phase8.lean"]
  assertExitCode "panel command" panel 0
  assertContains "panel command" panel.stdout "Informal panel: Phase8.lean"
  assertContains "panel command" panel.stdout "formalized: 6"
  assertContains "panel command" panel.stdout "integrationPhase8FormalizedWithRef"

run_cmd do
  let missingFile := ← liftIO <| Informalize.Cli.invoke #["panel", "--module", "Tests.Integration.Phase8"]
  assertExitCode "panel missing file" missingFile 1
  assertContains "panel missing file" missingFile.stderr "missing required option `--file <FileHint>` for `panel`"

run_cmd do
  let multiModuleStatus := ← liftIO <| Informalize.Cli.invoke #[
    "status",
    "--module", "Tests.Unit.Phase4",
    "--module", "Tests.Unit.Phase5"
  ]
  assertExitCode "multi-module status" multiModuleStatus 0
  assertContains "multi-module status" multiModuleStatus.stdout "phase4Base"
  assertContains "multi-module status" multiModuleStatus.stdout "phase5Placeholder"

run_cmd do
  let badModule := ← liftIO <| Informalize.Cli.invoke #["status", "--module", "Tests.DoesNotExist"]
  assertExitCode "bad module import" badModule 1
  assertContains "bad module import" badModule.stderr "Tests.DoesNotExist"
