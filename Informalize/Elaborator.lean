module

public import Lean
public import Informalize.Axiom
public import Informalize.Extension
public meta import Informalize.Extension
public meta import Init.Data.String.Legacy

public section

open Lean Elab Term Meta

namespace Informalize

syntax (name := informalTermWithLoc) "informal[" ident "]" (ppSpace term:max)* : term
syntax (name := informalTermNoLoc) "informal" (ppSpace term:max)* : term

private structure MarkdownHeading where
  level : Nat
  title : String
  line : Nat
  deriving Repr

private meta def countLeadingHashes : String -> Nat
  | s =>
    let rec go : List Char -> Nat
      | '#' :: rest => go rest + 1
      | _ => 0
    go s.toList

private meta def parseHeading? (line : String) (lineNo : Nat) : Option MarkdownHeading :=
  let trimmed := line.trimAscii.toString
  let level := countLeadingHashes trimmed
  if level == 0 then
    none
  else
    let title := (String.ofList (trimmed.toList.drop level)).trimAscii.toString
    if title.isEmpty then
      none
    else
      some { level, title, line := lineNo }

private meta def collectHeadings (content : String) : Array MarkdownHeading := Id.run do
  let lines := (content.splitOn "\n").toArray
  let mut headings : Array MarkdownHeading := #[]
  for idx in [0:lines.size] do
    match lines[idx]? with
    | some line =>
      if let some heading := parseHeading? line (idx + 1) then
        headings := headings.push heading
    | none =>
      pure ()
  return headings

private meta def renderId (components : Array String) : String :=
  ".".intercalate components.toList

private meta def mkNameFromComponents (components : Array String) : Name :=
  components.foldl (init := .anonymous) fun acc component =>
    .str acc component

private meta def nameComponents (name : Name) : Except String (Array String) := do
  let rec go : Name -> Except String (List String)
    | .anonymous =>
      pure []
    | .str parent component => do
      let parts ← go parent
      pure (parts ++ [component])
    | .num _ _ =>
      throw "numeric components are not supported in informal ids"
  return (← go name).toArray

private meta def parseIdComponents
    (idStx : TSyntax `ident) : TermElabM (String × Array String × String × Name) := do
  let components ←
    match nameComponents idStx.getId with
    | .ok components =>
      pure components
    | .error err =>
      throwErrorAt idStx err
  if components.size < 2 then
    throwErrorAt idStx s!"informal id `{renderId components}` must have at least two components (`File.Section`)"
  let some fileStem := components[0]?
    | throwErrorAt idStx s!"invalid informal id `{renderId components}`"
  let headingPath := Id.run do
    let mut path : Array String := #[]
    for idx in [1:components.size] do
      match components[idx]? with
      | some component =>
        path := path.push component
      | none =>
        pure ()
    return path
  return (fileStem, headingPath, renderId components, mkNameFromComponents components)

private meta def findHeadingIdx?
    (headings : Array MarkdownHeading)
    (title : String) : Option Nat := Id.run do
  let mut idx? : Option Nat := none
  for idx in [0:headings.size] do
    match headings[idx]? with
    | some heading =>
      if idx?.isNone && heading.title == title then
        idx? := some idx
    | none =>
      pure ()
  return idx?

private meta def findChildHeadingIdx?
    (headings : Array MarkdownHeading)
    (startIdx : Nat)
    (parentLevel : Nat)
    (targetTitle : String) : Option Nat := Id.run do
  let mut idx? : Option Nat := none
  let mut stop := false
  for idx in [startIdx:headings.size] do
    if !stop then
      match headings[idx]? with
      | some heading =>
        if heading.level <= parentLevel then
          stop := true
        else if heading.title == targetTitle then
          idx? := some idx
          stop := true
      | none =>
        pure ()
  return idx?

private meta def resolveHeadingPath
    (headings : Array MarkdownHeading)
    (titles : Array String) : Except String MarkdownHeading := do
  if titles.isEmpty then
    throw "expected at least one heading component"
  let some rootTitle := titles[0]?
    | throw "expected at least one heading component"
  let some rootIdx := findHeadingIdx? headings rootTitle
    | throw s!"missing section `{rootTitle}`"
  let some rootHeading := headings[rootIdx]?
    | throw s!"missing section `{rootTitle}`"
  let mut currentIdx := rootIdx
  let mut currentHeading := rootHeading
  for idx in [1:titles.size] do
    let some title := titles[idx]?
      | throw "invalid heading component"
    let some childIdx :=
      findChildHeadingIdx? headings (currentIdx + 1) currentHeading.level title
      | throw s!"missing subsection `{title}` under `{currentHeading.title}`"
    let some childHeading := headings[childIdx]?
      | throw s!"missing subsection `{title}` under `{currentHeading.title}`"
    currentIdx := childIdx
    currentHeading := childHeading
  return currentHeading

private meta def resolveInformalId (idStx : TSyntax `ident) : TermElabM Name := do
  let (fileStem, headingPath, renderedId, locationName) ← parseIdComponents idStx
  let filePath := s!"informal/{fileStem}.md"
  let pathExists ← (System.FilePath.pathExists filePath : IO Bool)
  if !pathExists then
    throwErrorAt idStx s!"informal id `{renderedId}` points to missing file `{filePath}`"
  let content ←
    try
      (IO.FS.readFile filePath : IO String)
    catch _ =>
      throwErrorAt idStx s!"unable to read `{filePath}` for informal id `{renderedId}`"
  let headings := collectHeadings content
  if headings.isEmpty then
    throwErrorAt idStx s!"informal id `{renderedId}` points to `{filePath}`, but the file has no markdown headings"
  match resolveHeadingPath headings headingPath with
  | .ok _ =>
    pure locationName
  | .error err =>
    throwErrorAt idStx s!"informal id `{renderedId}` is invalid in `{filePath}`: {err}"

private meta def mkUniqueTag : TermElabM Name := do
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

private meta def nameContainsComponent (name : Name) (target : String) : Bool :=
  match name with
  | .anonymous =>
    false
  | .str parent component =>
    component == target || nameContainsComponent parent target
  | .num parent _ =>
    nameContainsComponent parent target

private meta def isCommandPseudoDeclName (declName : Name) : Bool :=
  declName == `_check ||
    declName == `_reduce ||
    declName == `_synth_cmd ||
    nameContainsComponent declName "_eval"

private meta def mkInformalExpr (expectedType : Expr) (argExprs : Array Expr) : TermElabM Expr := do
  let expectedType ← instantiateMVars expectedType
  let argExprs ← argExprs.mapM instantiateMVars
  let argTypes ← argExprs.mapM fun argExpr => do
    instantiateMVars (← Meta.inferType argExpr)
  let alpha := argTypes.foldr (init := expectedType) fun argType body =>
    mkForall `arg .default argType body
  let tag ← mkUniqueTag
  let level ← Meta.getLevel alpha
  let informalConst := Lean.mkConst ``Informalize.Informal [level]
  let informalExpr := mkApp2 informalConst (toExpr tag) alpha
  return mkAppN informalExpr argExprs

private meta def runInformalElab
    (location? : Option (TSyntax `ident))
    (args : Array (TSyntax `term))
    (expectedType? : Option Expr) : TermElabM Expr := do
  let some declName := (← getDeclName?)
    | throwError "`informal` may only be used inside declaration values or proofs"
  if isCommandPseudoDeclName declName then
    throwError "`informal` may only be used inside declaration values or proofs"
  let locationId? ←
    match location? with
    | some location =>
      pure (some (← resolveInformalId location))
    | none =>
      pure none
  let argExprs ← args.mapM fun arg =>
    withRef arg <| elabTerm arg none
  let expectedType ←
    match expectedType? with
    | some expectedType =>
      instantiateMVars expectedType
    | none =>
      mkFreshTypeMVar
  let expr ← mkInformalExpr expectedType argExprs
  Term.synthesizeSyntheticMVarsNoPostponing
  let expr ← instantiateMVars expr
  addInformalOccurrence declName locationId?
  return expr

@[term_elab informalTermWithLoc] meta def elabInformalTermWithLoc : TermElab := fun stx expectedType? => do
  match stx with
  | `(informal[$id:ident] $[$args:term]*) =>
    runInformalElab (some id) args expectedType?
  | _ =>
    throwUnsupportedSyntax

@[term_elab informalTermNoLoc] meta def elabInformalTermNoLoc : TermElab := fun stx expectedType? => do
  match stx with
  | `(informal $[$args:term]*) =>
    runInformalElab none args expectedType?
  | _ =>
    throwUnsupportedSyntax

end Informalize
