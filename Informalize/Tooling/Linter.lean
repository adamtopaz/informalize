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

private def oneLine (s : String) : String :=
  ((s.replace "\n" " ").replace "\t" " ").trimAscii.toString

def staleDescriptionScore (entry : InformalEntry) : Nat :=
  (oneLine entry.description).length

def isPotentiallyStaleDescription
    (entry : InformalEntry)
    (cfg : StaleDescriptionHeuristicConfig := {}) : Bool :=
  staleDescriptionScore entry > cfg.threshold

private def sourcePointer (entry : InformalEntry) : String :=
  s!"{entry.sourceInfo.moduleName}:{entry.sourceInfo.line}:{entry.sourceInfo.column}"

private def entryTarget (entry : InformalEntry) : String :=
  match entry.declName with
  | some declName => s!"`{declName}`"
  | none => s!"entry at {sourcePointer entry}"

private def markerForId (id : String) : String :=
  s!"<!-- informalize:id={id} -->"

private def markerOccurrences (content : String) (marker : String) : Nat :=
  if marker.isEmpty then
    0
  else
    (content.splitOn marker).length.pred

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

def runLinter : CommandElabM Unit := do
  let report ← liftCoreM collectReport
  let entries ← liftCoreM Informalize.allEntries
  let cfg : StaleDescriptionHeuristicConfig := {}
  let mut foundIssues := false

  for declName in report.informalDecls do
    foundIssues := true
    logWarning m!"declaration `{declName}` transitively references `Informalize.Informal`"
  for declName in report.sorryDecls do
    foundIssues := true
    logWarning m!"declaration `{declName}` transitively references `sorryAx`"
  for entry in report.orphanEntries do
    foundIssues := true
    match entry.declName with
    | some declName =>
      logWarning m!"orphan metadata entry for `{declName}` at {sourcePointer entry}"
    | none =>
      logWarning m!"orphan metadata entry at {sourcePointer entry}"

  for entry in entries do
    if isPotentiallyStaleDescription entry cfg && entry.docRef?.isNone then
      foundIssues := true
      logWarning m!"description for {entryTarget entry} exceeds {cfg.threshold} characters and has no doc reference"

    match entry.docRef? with
    | none =>
      pure ()
    | some docRef => do
      if !docRef.path.endsWith ".md" then
        foundIssues := true
        logWarning m!"doc reference for {entryTarget entry} points to non-markdown path `{docRef.path}`"

      let pathExists ← liftIO <| System.FilePath.pathExists docRef.path
      if !pathExists then
        foundIssues := true
        logWarning m!"doc reference for {entryTarget entry} points to missing file `{docRef.path}`"
      else
        try
          let content ← liftIO <| IO.FS.readFile docRef.path
          match docRef.id? with
          | some id =>
            let marker := markerForId id
            if !content.contains marker then
              foundIssues := true
              logWarning m!"doc reference for {entryTarget entry} is missing marker `{id}` in `{docRef.path}`"
            else
              let occurrences := markerOccurrences content marker
              if occurrences > 1 then
                foundIssues := true
                logWarning m!"doc reference marker `{id}` appears {occurrences} times in `{docRef.path}`"
          | none =>
            pure ()
        catch _ =>
          foundIssues := true
          logWarning m!"doc reference for {entryTarget entry} points to unreadable file `{docRef.path}`"

  if !foundIssues then
    logInfo "informal linter found no issues"

syntax (name := informalLintCmd) "#informal_lint" : command

@[command_elab informalLintCmd] def elabInformalLintCmd : CommandElab := fun _stx => do
  runLinter

end Informalize.Tooling
