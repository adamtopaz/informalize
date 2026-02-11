import Lean
import Informalize.Axiom
import Informalize.Extension

open Lean Elab Command

namespace Informalize.Tooling

structure StaleDescriptionHeuristicConfig where
  threshold : Nat := 120
  deriving Inhabited, Repr

structure LinterReport where
  informalDecls : Array Name := #[]
  sorryDecls : Array Name := #[]
  orphanEntries : InformalEntries := #[]
  deriving Inhabited, Repr

def staleDescriptionScore (_entry : InformalEntry) : Nat :=
  0

def isPotentiallyStaleDescription
    (entry : InformalEntry)
    (cfg : StaleDescriptionHeuristicConfig := {}) : Bool :=
  staleDescriptionScore entry > cfg.threshold

private def sourcePointer (entry : InformalEntry) : String :=
  s!"{entry.sourceInfo.moduleName}:{entry.sourceInfo.line}:{entry.sourceInfo.column}"

private def currentModuleDecls : CoreM (Array Name) := do
  let env ← getEnv
  let decls := env.constants.fold (init := #[]) fun acc declName _ =>
    if (env.getModuleIdxFor? declName).isNone then
      acc.push declName
    else
      acc
  return decls.qsort Name.quickLt

private def collectDeclsByAxiom (axiomName : Name) : CoreM (Array Name) := do
  let mut decls : Array Name := #[]
  for declName in (← currentModuleDecls) do
    let axioms ← collectAxioms declName
    if axioms.contains axiomName then
      decls := decls.push declName
  return decls

def informalDecls : CoreM (Array Name) :=
  collectDeclsByAxiom ``Informalize.Informal

def sorryDecls : CoreM (Array Name) :=
  collectDeclsByAxiom ``sorryAx

def orphanEntries : CoreM InformalEntries := do
  let entries ← Informalize.allEntries
  let env ← getEnv
  return entries.filter fun entry =>
    match entry.declName with
    | none => true
    | some declName =>
      !(env.contains (skipRealize := false) declName)

def collectReport : CoreM LinterReport := do
  return {
    informalDecls := (← informalDecls)
    sorryDecls := (← sorryDecls)
    orphanEntries := (← orphanEntries)
  }

def runLinter : CoreM Unit := do
  let report ← collectReport
  for declName in report.informalDecls do
    logWarning m!"declaration `{declName}` transitively references `Informalize.Informal`"
  for declName in report.sorryDecls do
    logWarning m!"declaration `{declName}` transitively references `sorryAx`"
  for entry in report.orphanEntries do
    match entry.declName with
    | some declName =>
      logWarning m!"orphan metadata entry for `{declName}` at {sourcePointer entry}"
    | none =>
      logWarning m!"orphan metadata entry at {sourcePointer entry}"
  if report.informalDecls.isEmpty && report.sorryDecls.isEmpty && report.orphanEntries.isEmpty then
    logInfo "informal linter found no issues"

syntax (name := informalLintCmd) "#informal_lint" : command

@[command_elab informalLintCmd] def elabInformalLintCmd : CommandElab := fun _stx => do
  liftCoreM runLinter

end Informalize.Tooling
