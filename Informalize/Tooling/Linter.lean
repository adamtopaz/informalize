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

private def collectDeclsByAxiomInDecls (sourceDecls : Array Name) (axiomName : Name) : CoreM (Array Name) := do
  let mut hits : Array Name := #[]
  for declName in sourceDecls do
    let axioms ← collectAxioms declName
    if axioms.contains axiomName then
      hits := hits.push declName
  return hits

private def collectDeclsByAxiom (axiomName : Name) : CoreM (Array Name) := do
  collectDeclsByAxiomInDecls (← currentModuleDecls) axiomName

def declsInModules (moduleNames : Array Name) : CoreM (Array Name) := do
  let env ← getEnv
  let moduleIdxs := moduleNames.filterMap env.getModuleIdx?
  if moduleIdxs.isEmpty then
    return #[]
  let decls := env.constants.fold (init := #[]) fun acc declName _ =>
    match env.getModuleIdxFor? declName with
    | some moduleIdx =>
      if moduleIdxs.contains moduleIdx then
        acc.push declName
      else
        acc
    | none =>
      acc
  return decls.qsort Name.quickLt

def informalDeclsInDecls (decls : Array Name) : CoreM (Array Name) :=
  collectDeclsByAxiomInDecls decls ``Informalize.Informal

def sorryDeclsInDecls (decls : Array Name) : CoreM (Array Name) :=
  collectDeclsByAxiomInDecls decls ``sorryAx

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

def collectReportForDecls (decls : Array Name) : CoreM LinterReport := do
  return {
    informalDecls := (← informalDeclsInDecls decls)
    sorryDecls := (← sorryDeclsInDecls decls)
    orphanEntries := (← orphanEntries)
  }

def collectReportForModules (moduleNames : Array Name) : CoreM LinterReport := do
  collectReportForDecls (← declsInModules moduleNames)

private def lintWarningsFromReport (report : LinterReport) : CoreM (Array String) := do
  let entries ← Informalize.allEntries
  let cfg : StaleDescriptionHeuristicConfig := {}
  let mut warnings : Array String := #[]

  for declName in report.informalDecls do
    warnings := warnings.push s!"declaration `{declName}` transitively references `Informalize.Informal`"
  for declName in report.sorryDecls do
    warnings := warnings.push s!"declaration `{declName}` transitively references `sorryAx`"
  for entry in report.orphanEntries do
    match entry.declName with
    | some declName =>
      warnings := warnings.push s!"orphan metadata entry for `{declName}` at {sourcePointer entry}"
    | none =>
      warnings := warnings.push s!"orphan metadata entry at {sourcePointer entry}"

  for entry in entries do
    if isPotentiallyStaleDescription entry cfg && entry.docRef?.isNone then
      warnings := warnings.push s!"description for {entryTarget entry} exceeds {cfg.threshold} characters and has no doc reference"

    match entry.docRef? with
    | none =>
      pure ()
    | some docRef => do
      if !docRef.path.endsWith ".md" then
        warnings := warnings.push s!"doc reference for {entryTarget entry} points to non-markdown path `{docRef.path}`"

      let pathExists ← (System.FilePath.pathExists docRef.path : IO Bool)
      if !pathExists then
        warnings := warnings.push s!"doc reference for {entryTarget entry} points to missing file `{docRef.path}`"
      else
        try
          let content ← (IO.FS.readFile docRef.path : IO String)
          match docRef.id? with
          | some id =>
            let marker := markerForId id
            if !content.contains marker then
              warnings := warnings.push s!"doc reference for {entryTarget entry} is missing marker `{id}` in `{docRef.path}`"
            else
              let occurrences := markerOccurrences content marker
              if occurrences > 1 then
                warnings := warnings.push s!"doc reference marker `{id}` appears {occurrences} times in `{docRef.path}`"
          | none =>
            pure ()
        catch _ =>
          warnings := warnings.push s!"doc reference for {entryTarget entry} points to unreadable file `{docRef.path}`"

  return warnings

def lintWarnings : CoreM (Array String) := do
  lintWarningsFromReport (← collectReport)

def lintWarningsForModules (moduleNames : Array Name) : CoreM (Array String) := do
  lintWarningsFromReport (← collectReportForModules moduleNames)

def renderLintWarnings (warnings : Array String) : String :=
  if warnings.isEmpty then
    "informal linter found no issues"
  else
    "\n".intercalate ((warnings.map fun warning => s!"warning: {warning}").toList)

def renderLintReport : CoreM String := do
  return renderLintWarnings (← lintWarnings)

def runLinter : CommandElabM Unit := do
  let warnings ← liftCoreM lintWarnings
  if warnings.isEmpty then
    logInfo "informal linter found no issues"
  else
    for warning in warnings do
      logWarning m!"{warning}"

syntax (name := informalLintCmd) "#informal_lint" : command

@[command_elab informalLintCmd] def elabInformalLintCmd : CommandElab := fun _stx => do
  runLinter

end Informalize.Tooling
