import Lean
import Informalize.Extension

open Lean Elab Command

namespace Informalize.Tooling

syntax (name := informalDepsCmd) "#informal_deps" : command

private def mergeUniqueNames (base extra : Array Name) : Array Name := Id.run do
  let mut seen : NameSet := {}
  let mut result : Array Name := #[]
  for name in base do
    if !seen.contains name then
      seen := seen.insert name
      result := result.push name
  for name in extra do
    if !seen.contains name then
      seen := seen.insert name
      result := result.push name
  return result

private def refsForDecl (declRefs : Array (Name × Array Name)) (declName : Name) : Array Name :=
  match declRefs.find? (·.1 == declName) with
  | some (_, refs) => refs
  | none => #[]

private def insertDeclRefs
    (declRefs : Array (Name × Array Name))
    (declName : Name)
    (refs : Array Name) : Array (Name × Array Name) := Id.run do
  let mut found := false
  let mut result : Array (Name × Array Name) := #[]
  for (name, currentRefs) in declRefs do
    if name == declName then
      found := true
      result := result.push (name, mergeUniqueNames currentRefs refs)
    else
      result := result.push (name, currentRefs)
  if !found then
    result := result.push (declName, refs)
  return result

private def collectInformalDeclRefs (entries : InformalEntries) : Array (Name × Array Name) := Id.run do
  let mut declRefs : Array (Name × Array Name) := #[]
  for entry in entries do
    if entry.status == .informal then
      if let some declName := entry.declName then
        declRefs := insertDeclRefs declRefs declName entry.referencedConstants
  return declRefs

private def usedConstants (constInfo : ConstantInfo) : Array Name :=
  let fromType := constInfo.type.getUsedConstants
  let fromValue :=
    match constInfo.value? with
    | some value => value.getUsedConstants
    | none => #[]
  mergeUniqueNames fromType fromValue

private structure ReachState where
  visited : NameSet := {}
  ordered : Array Name := #[]

private partial def collectReachableConstants
    (env : Environment)
    (constName : Name)
    (state : ReachState) : ReachState :=
  if state.visited.contains constName then
    state
  else
    let state := {
      visited := state.visited.insert constName
      ordered := state.ordered.push constName
    }
    match env.find? constName with
    | none => state
    | some constInfo =>
      (usedConstants constInfo).foldl (init := state) fun state depName =>
        collectReachableConstants env depName state

private def dependencyTargets
    (env : Environment)
    (informalDecls : NameSet)
    (srcDecl : Name)
    (refs : Array Name) : Array Name := Id.run do
  let mut seen : NameSet := {}
  let mut targets : Array Name := #[]
  for refName in refs do
    let reachable := (collectReachableConstants env refName {}).ordered
    for target in reachable do
      if target != srcDecl && informalDecls.contains target && !seen.contains target then
        seen := seen.insert target
        targets := targets.push target
  return targets.qsort Name.quickLt

private def buildGraph
    (env : Environment)
    (declRefs : Array (Name × Array Name)) : Array (Name × Array Name) :=
  let declNames := (declRefs.map (·.1)).qsort Name.quickLt
  let informalDecls := declNames.foldl (init := ({} : NameSet)) fun set declName =>
    set.insert declName
  declNames.map fun declName =>
    let refs := refsForDecl declRefs declName
    (declName, dependencyTargets env informalDecls declName refs)

def dependencyGraph : CoreM (Array (Name × Array Name)) := do
  let entries ← Informalize.allEntries
  let env ← getEnv
  let declRefs := collectInformalDeclRefs entries
  return buildGraph env declRefs

private def leavesFromGraph (graph : Array (Name × Array Name)) : Array Name :=
  graph.foldl (init := #[]) fun acc (declName, deps) =>
    if deps.isEmpty then acc.push declName else acc

def dependencyLeaves : CoreM (Array Name) := do
  let graph ← dependencyGraph
  return leavesFromGraph graph

private def renderGraphLine (declName : Name) (deps : Array Name) : String :=
  if deps.isEmpty then
    s!"  {declName} -> (none)"
  else
    s!"  {declName} -> {", ".intercalate (deps.toList.map toString)}"

def renderDependencyGraph : CoreM String := do
  let graph ← dependencyGraph

  if graph.isEmpty then
    return "Informal dependency graph:\n  (no informal declarations found)"
  else
    let lines := graph.map (fun (declName, deps) => renderGraphLine declName deps)
    let leaves := leavesFromGraph graph
    let leafLine :=
      if leaves.isEmpty then
        "Leaves (no informal dependencies): (none)"
      else
        s!"Leaves (no informal dependencies): {", ".intercalate (leaves.toList.map toString)}"
    let output :=
      #["Informal dependency graph:"] ++ lines ++ #["", leafLine]
    return "\n".intercalate output.toList

@[command_elab informalDepsCmd] def elabInformalDepsCmd : CommandElab := fun _stx => do
  logInfo (← liftCoreM renderDependencyGraph)

end Informalize.Tooling
