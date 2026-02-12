module

public import Lean
public import Informalize.Axiom
public import Informalize.Extension
public meta import Informalize.Extension

public section

open Lean Elab Term Tactic Meta

namespace Informalize

syntax (name := informalTerm) "informal " interpolatedStr(term) : term
syntax (name := informalTermFrom) "informal " interpolatedStr(term) " from " str : term
syntax (name := formalizedTerm) "formalized " interpolatedStr(term) " as " term : term
syntax (name := formalizedTermFrom) "formalized " interpolatedStr(term) " from " str " as " term : term

syntax (name := informalTactic) "informal " interpolatedStr(term) : tactic
syntax (name := informalTacticFrom) "informal " interpolatedStr(term) " from " str : tactic
syntax (name := formalizedTactic) "formalized " interpolatedStr(term) " as " tacticSeq : tactic
syntax (name := formalizedTacticFrom) "formalized " interpolatedStr(term) " from " str " as " tacticSeq : tactic

structure DescriptionData where
  renderedDescription : String
  interpolationExprs : Array Expr
  referencedConstants : Array Name

meta def collectConstantsFromExpr (expr : Expr) (seen : NameSet) (constants : Array Name) : NameSet × Array Name :=
  match expr with
  | .forallE _ domain body _ =>
    let (seen, constants) := collectConstantsFromExpr domain seen constants
    collectConstantsFromExpr body seen constants
  | .lam _ domain body _ =>
    let (seen, constants) := collectConstantsFromExpr domain seen constants
    collectConstantsFromExpr body seen constants
  | .letE _ type value body _ =>
    let (seen, constants) := collectConstantsFromExpr type seen constants
    let (seen, constants) := collectConstantsFromExpr value seen constants
    collectConstantsFromExpr body seen constants
  | .app fn arg =>
    let (seen, constants) := collectConstantsFromExpr fn seen constants
    collectConstantsFromExpr arg seen constants
  | .mdata _ body =>
    collectConstantsFromExpr body seen constants
  | .proj _ _ body =>
    collectConstantsFromExpr body seen constants
  | .const constName _ =>
    if seen.contains constName then
      (seen, constants)
    else
      (seen.insert constName, constants.push constName)
  | _ =>
    (seen, constants)

meta def collectConstants (exprs : Array Expr) : Array Name := Id.run do
  let mut seen : NameSet := {}
  let mut constants : Array Name := #[]
  for expr in exprs do
    let (seen', constants') := collectConstantsFromExpr expr seen constants
    seen := seen'
    constants := constants'
  return constants

meta def elabDescriptionData (descriptionStx : Syntax) : TermElabM DescriptionData := do
  let mut renderedDescription := ""
  let mut interpolationExprs : Array Expr := #[]
  for chunk in descriptionStx.getArgs do
    match chunk.isInterpolatedStrLit? with
    | some litChunk =>
      renderedDescription := renderedDescription ++ litChunk
    | none =>
      let interpExpr ← withRef chunk <| elabTerm chunk none
      let interpExpr ← instantiateMVars interpExpr
      interpolationExprs := interpolationExprs.push interpExpr
      let ppInterpExpr ← Meta.ppExpr interpExpr
      renderedDescription := renderedDescription ++ toString ppInterpExpr
  let referencedConstants := collectConstants interpolationExprs
  return {
    renderedDescription
    interpolationExprs
    referencedConstants
  }

meta def collectCapturedFVarIds (interpolationExprs : Array Expr) (expectedType : Expr) : Array FVarId := Id.run do
  let mut fvarState : CollectFVars.State := {}
  for expr in interpolationExprs do
    fvarState := collectFVars fvarState expr
  fvarState := collectFVars fvarState expectedType
  fvarState.fvarIds

meta def mkSourceRef : TermElabM SourceRef := do
  let moduleName ← getMainModule
  let fileName ← getFileName
  let fileMap ← getFileMap
  let ref ← getRef
  let (line, column, endLine, endColumn) :=
    match ref.getPos?, ref.getTailPos? with
    | some startPos, some endPos =>
      let startPosition := fileMap.toPosition startPos
      let endPosition := fileMap.toPosition endPos
      (startPosition.line, startPosition.column, endPosition.line, endPosition.column)
    | _, _ =>
      (0, 0, 0, 0)
  return {
    moduleName
    fileName
    line
    column
    endLine
    endColumn
  }

meta def mkParams (fvarIds : Array FVarId) : TermElabM (Array (Name × String)) := do
  let mut params : Array (Name × String) := #[]
  for fvarId in fvarIds do
    let localDecl ← fvarId.getDecl
    let type ← instantiateMVars localDecl.type
    let ppType ← Meta.ppExpr type
    params := params.push (localDecl.userName, toString ppType)
  return params

meta def mkUniqueTag : TermElabM Name := do
  let ref ← getRef
  if let (some startSPos, some endSPos) := (ref.getPos?, ref.getTailPos?) then
    let fileMap ← getFileMap
    SorryLabelView.encode {
      module? := some {
        module := (← getMainModule)
        range := {
          pos := fileMap.toPosition startSPos
          endPos := fileMap.toPosition endSPos
          charUtf16 := (fileMap.utf8PosToLspPos startSPos).character
          endCharUtf16 := (fileMap.utf8PosToLspPos endSPos).character
        }
      }
    }
  else
    SorryLabelView.encode {}

meta def mkInformalExpr (expectedType : Expr) (capturedFVarIds : Array FVarId) : TermElabM Expr := do
  let capturedFVars := capturedFVarIds.map mkFVar
  let alpha ← mkForallFVars capturedFVars expectedType
  let tag ← mkUniqueTag
  let taggedAlpha := mkForall `tag .default (mkConst ``Lean.Name) alpha
  let level ← Meta.getLevel taggedAlpha
  let informalConst := Lean.mkConst ``Informalize.Informal [level]
  let informalWithTag := mkApp2 informalConst taggedAlpha (toExpr tag)
  return mkAppN informalWithTag capturedFVars

meta def mkEntry
    (status : InformalStatus)
    (descriptionData : DescriptionData)
    (docRef? : Option DocRef)
    (expectedType : Expr)
    (capturedFVarIds : Array FVarId) : TermElabM InformalEntry := do
  let expectedType ← instantiateMVars expectedType
  let ppExpectedType ← Meta.ppExpr expectedType
  let params ← mkParams capturedFVarIds
  let sourceInfo ← mkSourceRef
  let declName ← getDeclName?
  let referencedConstants := Id.run do
    let mut seen : NameSet := {}
    let mut constants : Array Name := #[]
    for constName in descriptionData.referencedConstants do
      if !seen.contains constName then
        seen := seen.insert constName
        constants := constants.push constName
    let (_, constantsOut) := collectConstantsFromExpr expectedType seen constants
    return constantsOut
  return {
    declName
    description := descriptionData.renderedDescription
    docRef?
    params
    expectedType := toString ppExpectedType
    referencedConstants
    sourceInfo
    status
  }

meta def parseDocRefStx (docRefStx : Syntax) : TermElabM DocRef := do
  let some raw := docRefStx.isStrLit?
    | throwErrorAt docRefStx "expected string literal path[#id]"
  match parseDocRefRaw raw with
  | Except.ok docRef =>
    pure docRef
  | Except.error err =>
    throwErrorAt docRefStx s!"invalid doc reference '{raw}': {err}"

meta def parseDocRefStxInTactic (docRefStx : Syntax) : TacticM DocRef := do
  let some raw := docRefStx.isStrLit?
    | throwErrorAt docRefStx "expected string literal path[#id]"
  match parseDocRefRaw raw with
  | Except.ok docRef =>
    pure docRef
  | Except.error err =>
    throwErrorAt docRefStx s!"invalid doc reference '{raw}': {err}"

meta def runInformalElab
    (descriptionStx : Syntax)
    (docRef? : Option DocRef)
    (expectedType? : Option Expr) : TermElabM Expr := do
  let descriptionData ← elabDescriptionData descriptionStx
  let expectedType ←
    match expectedType? with
    | some expectedType =>
      instantiateMVars expectedType
    | none => do
      let probeExpectedType ← mkFreshTypeMVar
      let probeCapturedFVarIds := collectCapturedFVarIds descriptionData.interpolationExprs probeExpectedType
      let probeExpr ← mkInformalExpr probeExpectedType probeCapturedFVarIds
      Term.synthesizeSyntheticMVarsNoPostponing
      let probeExpr ← instantiateMVars probeExpr
      instantiateMVars (← Meta.inferType probeExpr)
  let capturedFVarIds := collectCapturedFVarIds descriptionData.interpolationExprs expectedType
  let expr ← mkInformalExpr expectedType capturedFVarIds
  let entry ← mkEntry .informal descriptionData docRef? expectedType capturedFVarIds
  addInformalEntry entry
  return annotateExprWithEntry entry expr

meta def runFormalizedTermElab
    (descriptionStx : Syntax)
    (docRef? : Option DocRef)
    (bodyStx : Syntax)
    (expectedType? : Option Expr) : TermElabM Expr := do
  let bodyExpr ← elabTerm bodyStx expectedType?
  Term.synthesizeSyntheticMVarsNoPostponing
  let bodyExpr ← instantiateMVars bodyExpr
  let expectedType ←
    match expectedType? with
    | some expectedType => instantiateMVars expectedType
    | none => instantiateMVars (← Meta.inferType bodyExpr)
  let descriptionData ← elabDescriptionData descriptionStx
  let capturedFVarIds := collectCapturedFVarIds descriptionData.interpolationExprs expectedType
  let entry ← mkEntry .formalized descriptionData docRef? expectedType capturedFVarIds
  addInformalEntry entry
  return annotateExprWithEntry entry bodyExpr

@[term_elab informalTerm] meta def elabInformalTerm : TermElab := fun stx expectedType? => do
  runInformalElab stx[1] none expectedType?

@[term_elab informalTermFrom] meta def elabInformalTermFrom : TermElab := fun stx expectedType? => do
  let docRef ← parseDocRefStx stx[3]
  runInformalElab stx[1] (some docRef) expectedType?

@[term_elab formalizedTerm] meta def elabFormalizedTerm : TermElab := fun stx expectedType? => do
  runFormalizedTermElab stx[1] none stx[3] expectedType?

@[term_elab formalizedTermFrom] meta def elabFormalizedTermFrom : TermElab := fun stx expectedType? => do
  let docRef ← parseDocRefStx stx[3]
  runFormalizedTermElab stx[1] (some docRef) stx[5] expectedType?

@[tactic informalTactic] meta def evalInformalTactic : Tactic := fun stx => do
  withMainContext do
    let goalType ← getMainTarget
    let expr ← runInformalElab stx[1] none (some goalType)
    closeMainGoal `informal expr

@[tactic informalTacticFrom] meta def evalInformalTacticFrom : Tactic := fun stx => do
  withMainContext do
    let goalType ← getMainTarget
    let docRef ← parseDocRefStxInTactic stx[3]
    let expr ← runInformalElab stx[1] (some docRef) (some goalType)
    closeMainGoal `informal expr

@[tactic formalizedTactic] meta def evalFormalizedTactic : Tactic := fun stx => do
  withMainContext do
    let goalType ← getMainTarget
    let body : TSyntax `Lean.Parser.Tactic.tacticSeq := ⟨stx[3]⟩
    let bodyTerm ← `(by $body)
    let expr ← runFormalizedTermElab stx[1] none bodyTerm (some goalType)
    closeMainGoal `formalized expr

@[tactic formalizedTacticFrom] meta def evalFormalizedTacticFrom : Tactic := fun stx => do
  withMainContext do
    let goalType ← getMainTarget
    let docRef ← parseDocRefStxInTactic stx[3]
    let body : TSyntax `Lean.Parser.Tactic.tacticSeq := ⟨stx[5]⟩
    let bodyTerm ← `(by $body)
    let expr ← runFormalizedTermElab stx[1] (some docRef) bodyTerm (some goalType)
    closeMainGoal `formalized expr

end Informalize
