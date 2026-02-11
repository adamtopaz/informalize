import Lean
import Informalize.Extension

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
  s!"  {entryLabel entry} : {typeSnippet} - \"{descSnippet}\" @ {sourcePointer entry}"

private def renderSection (title : String) (entries : InformalEntries) : Array String :=
  if entries.isEmpty then
    #[s!"{title} (0):", "  (none)"]
  else
    #[s!"{title} ({entries.size}):"] ++ entries.map renderEntry

@[command_elab informalStatusCmd] def elabInformalStatusCmd : CommandElab := fun _stx => do
  let all ← liftCoreM Informalize.allEntries
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

  logInfo ("\n".intercalate lines.toList)

end Informalize.Tooling
