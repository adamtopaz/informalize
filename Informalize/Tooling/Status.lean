module

public import Lean
public import Informalize.Extension

public section

open Lean Elab Command

namespace Informalize.Tooling

syntax (name := informalStatusCmd) "#informal_status" : command

private def oneLine (s : String) : String :=
  let compact := (s.replace "\n" " ").replace "\t" " "
  compact.trimAscii.toString

private def clip (maxLen : Nat) (s : String) : String :=
  if s.length <= maxLen then
    s
  else
    String.ofList (s.toList.take (maxLen - 3)) ++ "..."

private def sourcePointer (entry : InformalEntry) : String :=
  s!"{entry.sourceInfo.moduleName}:{entry.sourceInfo.line}:{entry.sourceInfo.column}"

private def entryLabel (entry : InformalEntry) : String :=
  match entry.declName with
  | some declName => toString declName
  | none => s!"(goal at {sourcePointer entry})"

private def renderEntry (entry : InformalEntry) : String :=
  let typeSnippet := clip 72 (oneLine entry.expectedType)
  let descSnippet := clip 90 (oneLine entry.description)
  let docSnippet :=
    match entry.docRef? with
    | some docRef =>
      s!" doc: {clip 72 (oneLine docRef.raw)}"
    | none =>
      ""
  s!"  {entryLabel entry} : {typeSnippet} - \"{descSnippet}\" @ {sourcePointer entry}{docSnippet}"

private def renderSection (title : String) (entries : InformalEntries) : Array String :=
  if entries.isEmpty then
    #[s!"{title} (0):", "  (none)"]
  else
    #[s!"{title} ({entries.size}):"] ++ entries.map renderEntry

def renderStatus : CoreM String := do
  let all ← Informalize.allEntries
  let informal := all.filter (·.status == .informal)
  let formalized := all.filter (·.status == .formalized)

  let total := all.size
  let completed := formalized.size
  let progressPct :=
    if total == 0 then
      0
    else
      (completed * 100) / total

  let lines :=
    renderSection "Informal" informal ++
    #[""] ++
    renderSection "Formalized" formalized ++
    #["", s!"Progress: {completed}/{total} ({progressPct}%)"]

  return "\n".intercalate lines.toList

@[command_elab informalStatusCmd] meta unsafe def elabInformalStatusCmd : CommandElab := fun _stx => do
  let renderStatusCmd ← liftCoreM <| Lean.evalConst (CoreM String) ``Informalize.Tooling.renderStatus
  logInfo (← liftCoreM renderStatusCmd)

end Informalize.Tooling
