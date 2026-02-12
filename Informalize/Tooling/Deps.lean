module

public import Lean
public import Informalize.Extension

public section

open Lean Elab Command

namespace Informalize.Tooling

syntax (name := informalDepsCmd) "#informal_deps" : command

private def collectInformalDeclNames (entries : InformalEntries) : Array Name := Id.run do
  let mut seen : NameSet := {}
  let mut declNames : Array Name := #[]
  for entry in entries do
    if entry.status == .informal then
      if let some declName := entry.declName then
        if !seen.contains declName then
          seen := seen.insert declName
          declNames := declNames.push declName
  return declNames.qsort Name.quickLt

private def usedConstants (constInfo : ConstantInfo) : Array Name :=
  Id.run do
    let mut refs : Array Name := #[]
    for constName in constInfo.getUsedConstantsAsSet do
      refs := refs.push constName
    return refs.qsort Name.quickLt

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
    (declNames : Array Name) : Array (Name × Array Name) :=
  let informalDecls := declNames.foldl (init := ({} : NameSet)) fun set declName =>
    set.insert declName
  declNames.map fun declName =>
    let refs :=
      match env.find? declName with
      | some constInfo => usedConstants constInfo
      | none => #[]
    (declName, dependencyTargets env informalDecls declName refs)

def dependencyGraph : CoreM (Array (Name × Array Name)) := do
  let entries ← Informalize.allEntries
  let env ← getEnv
  let declNames := collectInformalDeclNames entries
  return buildGraph env declNames

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

@[command_elab informalDepsCmd] meta unsafe def elabInformalDepsCmd : CommandElab := fun _stx => do
  let renderDepsCmd ← liftCoreM <| Lean.evalConst (CoreM String) ``Informalize.Tooling.renderDependencyGraph
  logInfo (← liftCoreM renderDepsCmd)

end Informalize.Tooling
