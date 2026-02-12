module

public import Lean
public import Informalize.CodeAction
public import Informalize.Tooling.Status
public import Informalize.Tooling.Deps
public import Informalize.Tooling.Blueprint
public import Informalize.Tooling.Linter

public section

open Lean

namespace Informalize.Cli

inductive Command where
  | status
  | deps
  | lint
  | blueprint
  | codeActions
  | hover
  | panel
  deriving Inhabited, Repr, BEq

inductive BlueprintOutputFormat where
  | markdown
  | json
  deriving Inhabited, Repr, BEq

structure Config where
  command : Command
  modules : Array Name
  blueprintFormat : BlueprintOutputFormat := .markdown
  declName? : Option Name := none
  fileHint? : Option String := none
  deriving Inhabited, Repr

structure InvocationResult where
  exitCode : UInt32
  stdout : String := ""
  stderr : String := ""
  deriving Inhabited, Repr

def usage : String :=
  "\n".intercalate [
    "Informalize CLI",
    "",
    "Usage:",
    "  lake exe informalize <command> --module <Module.Name> [options]",
    "",
    "Commands:",
    "  status                   Show informal/formalized progress",
    "  deps                     Show informal dependency graph",
    "  lint                     Run informalization lints",
    "  blueprint                Export blueprint snapshot",
    "  code-actions             Show migration suggestions",
    "  hover                    Show hover metadata for a declaration",
    "  panel                    Show per-file metadata panel",
    "",
    "Required:",
    "  -m, --module <Module>    Import module before running command (repeatable)",
    "",
    "Options:",
    "  --format <markdown|json> Output format (default: markdown)",
    "  --decl <Decl.Name>       Target declaration (hover, optional for code-actions)",
    "  --file <FileHint>        File path/basename hint (panel)",
    "",
    "Examples:",
    "  lake exe informalize status --module Tests.Unit.Phase3",
    "  lake exe informalize deps -m Tests.Unit.Phase3",
    "  lake exe informalize lint --module Tests.Integration.Phase8",
    "  lake exe informalize blueprint --module Tests.Unit.Phase4 --format json",
    "  lake exe informalize code-actions --module Tests.Unit.Phase5 --decl phase5Formalized",
    "  lake exe informalize hover --module Tests.Integration.Phase8 --decl integrationPhase8FormalizedWithRef",
    "  lake exe informalize panel --module Tests.Integration.Phase8 --file Phase8.lean"
  ]

private def parseDottedName (kind : String) (raw : String) : Except String Name := do
  let trimmed := raw.trimAscii.toString
  if trimmed.isEmpty then
    throw s!"{kind} name must be non-empty"
  let parts := trimmed.splitOn "."
  if parts.any String.isEmpty then
    throw s!"invalid {kind} name `{raw}`"
  return parts.foldl (init := Name.anonymous) Name.str

private def parseModuleName (raw : String) : Except String Name :=
  parseDottedName "module" raw

private def parseDeclName (raw : String) : Except String Name :=
  parseDottedName "declaration" raw

private def parseCommand (raw : String) : Except String Command :=
  match raw with
  | "status" => .ok .status
  | "deps" => .ok .deps
  | "lint" => .ok .lint
  | "blueprint" => .ok .blueprint
  | "code-actions" => .ok .codeActions
  | "code_actions" => .ok .codeActions
  | "hover" => .ok .hover
  | "panel" => .ok .panel
  | other => .error s!"unknown command `{other}`"

private def parseBlueprintFormat (raw : String) : Except String BlueprintOutputFormat :=
  match raw with
  | "markdown" => .ok .markdown
  | "json" => .ok .json
  | other => .error s!"unknown blueprint format `{other}`; expected `markdown` or `json`"

private def validateConfig (cfg : Config) : Except String Config := do
  if cfg.modules.isEmpty then
    throw "missing required option `--module <Module.Name>`"
  match cfg.command with
  | .hover =>
    if cfg.declName?.isNone then
      throw "missing required option `--decl <Decl.Name>` for `hover`"
  | .panel =>
    if cfg.fileHint?.isNone then
      throw "missing required option `--file <FileHint>` for `panel`"
  | _ =>
    pure ()
  return cfg

private partial def parseOptions
    (cfg : Config)
    (args : List String) : Except String (Option Config) := do
  match args with
  | [] =>
    return some (← validateConfig cfg)
  | "--help" :: _ =>
    return none
  | "-h" :: _ =>
    return none
  | "--module" :: moduleRaw :: rest =>
    parseOptions { cfg with modules := cfg.modules.push (← parseModuleName moduleRaw) } rest
  | "--module" :: [] =>
    throw "expected module name after `--module`"
  | "-m" :: moduleRaw :: rest =>
    parseOptions { cfg with modules := cfg.modules.push (← parseModuleName moduleRaw) } rest
  | "-m" :: [] =>
    throw "expected module name after `-m`"
  | "--format" :: formatRaw :: rest =>
    if cfg.command != .blueprint then
      throw "`--format` is only valid for the `blueprint` command"
    else
      parseOptions { cfg with blueprintFormat := (← parseBlueprintFormat formatRaw) } rest
  | "--format" :: [] =>
    throw "expected format after `--format`"
  | "--decl" :: declRaw :: rest =>
    if cfg.command != .codeActions && cfg.command != .hover then
      throw "`--decl` is only valid for `code-actions` and `hover`"
    else
      parseOptions { cfg with declName? := some (← parseDeclName declRaw) } rest
  | "--decl" :: [] =>
    throw "expected declaration name after `--decl`"
  | "--file" :: fileHint :: rest =>
    if cfg.command != .panel then
      throw "`--file` is only valid for the `panel` command"
    else
      parseOptions { cfg with fileHint? := some fileHint } rest
  | "--file" :: [] =>
    throw "expected file hint after `--file`"
  | arg :: _ =>
    throw s!"unknown option `{arg}`"

def parseArgs (args : Array String) : Except String (Option Config) := do
  match args.toList with
  | [] =>
    return none
  | "--help" :: _ =>
    return none
  | "-h" :: _ =>
    return none
  | "help" :: _ =>
    return none
  | commandRaw :: rest =>
    parseOptions { command := (← parseCommand commandRaw), modules := #[] } rest

private unsafe def importEnvironment (modules : Array Name) : IO Environment := do
  let sysroot ← Lean.findSysroot
  Lean.initSearchPath sysroot
  Lean.enableInitializersExecution
  let imports := modules.map fun moduleName => ({ module := moduleName : Import })
  Lean.importModules imports {} (loadExts := true)

private def runCoreInEnv (env : Environment) (x : CoreM String) : IO String := do
  let ctx : Core.Context := {
    fileName := "<informalize-cli>"
    fileMap := FileMap.ofString ""
    options := {}
  }
  let state : Core.State := { env := env }
  x.toIO' ctx state

private def runCommand (cfg : Config) : CoreM String := do
  match cfg.command with
  | .status =>
    Informalize.Tooling.renderStatus
  | .deps =>
    Informalize.Tooling.renderDependencyGraph
  | .lint =>
    return Informalize.Tooling.renderLintWarnings (← Informalize.Tooling.lintWarningsForModules cfg.modules)
  | .blueprint =>
    match cfg.blueprintFormat with
    | .markdown => Informalize.Tooling.renderBlueprintMarkdown
    | .json => Informalize.Tooling.renderBlueprintJson
  | .codeActions =>
    match cfg.declName? with
    | some declName => Informalize.renderCodeActionsForDecl declName
    | none => Informalize.renderCodeActions
  | .hover =>
    match cfg.declName? with
    | some declName => Informalize.renderHoverForDecl declName
    | none =>
      return "error: missing declaration"
  | .panel =>
    match cfg.fileHint? with
    | some fileHint => Informalize.renderPanelForFileHint fileHint
    | none =>
      return "error: missing file hint"

def invoke (args : Array String) : IO InvocationResult := do
  match parseArgs args with
  | .error err =>
    return {
      exitCode := 1
      stderr := s!"error: {err}\n\n{usage}"
    }
  | .ok none =>
    return {
      exitCode := 0
      stdout := usage
    }
  | .ok (some cfg) =>
    try
      let env ← unsafe importEnvironment cfg.modules
      let output ← runCoreInEnv env (runCommand cfg)
      return {
        exitCode := 0
        stdout := output
      }
    catch ex =>
      return {
        exitCode := 1
        stderr := s!"error: {ex}"
      }

end Informalize.Cli
