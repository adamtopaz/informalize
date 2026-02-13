import Informalize

open Lean Elab Command

namespace Tests.Unit.Extension

noncomputable def extBare : Nat :=
  informal

noncomputable def extLocOnce : Nat :=
  informal[Foo.bar]

noncomputable def extLocDup : Nat :=
  informal[Foo.bar] + informal[Foo.bar]

noncomputable def extLocMulti : Nat :=
  informal[Foo.bar] + informal[Foo.bar.baz]

noncomputable def extTypeAndValue : informal[Foo.bar] :=
  informal

private def entryFor?
    (entries : Array Informalize.InformalDeclEntry)
    (declName : Name) : Option Informalize.InformalDeclEntry :=
  entries.find? (·.declName == declName)

private def assertContainsLocation
    (entry : Informalize.InformalDeclEntry)
    (location : String) : CommandElabM Unit := do
  unless entry.locations.contains location do
    throwError s!"expected declaration `{entry.declName}` to contain location `{location}`"

run_cmd do
  let entries ← liftCoreM Informalize.allInformalDeclEntries

  let some bare := entryFor? entries ``Tests.Unit.Extension.extBare
    | throwError "missing extension entry for `extBare`"
  unless bare.locations.isEmpty do
    throwError "expected `extBare` to have an empty markdown location set"

  let some locOnce := entryFor? entries ``Tests.Unit.Extension.extLocOnce
    | throwError "missing extension entry for `extLocOnce`"
  unless locOnce.locations.size == 1 do
    throwError "expected `extLocOnce` to have exactly one markdown location"
  assertContainsLocation locOnce "Foo.bar"

  let some locDup := entryFor? entries ``Tests.Unit.Extension.extLocDup
    | throwError "missing extension entry for `extLocDup`"
  unless locDup.locations.size == 1 do
    throwError "expected `extLocDup` location set to be deduplicated"
  assertContainsLocation locDup "Foo.bar"

  let some locMulti := entryFor? entries ``Tests.Unit.Extension.extLocMulti
    | throwError "missing extension entry for `extLocMulti`"
  unless locMulti.locations.size == 2 do
    throwError "expected `extLocMulti` to have two markdown locations"
  assertContainsLocation locMulti "Foo.bar"
  assertContainsLocation locMulti "Foo.bar.baz"

  let some typeAndValue := entryFor? entries ``Tests.Unit.Extension.extTypeAndValue
    | throwError "missing extension entry for `extTypeAndValue`"
  unless typeAndValue.locations.size == 1 do
    throwError "expected `extTypeAndValue` to have one markdown location"
  assertContainsLocation typeAndValue "Foo.bar"

run_cmd do
  let some entry ← liftCoreM <| Informalize.informalDeclEntry? ``Tests.Unit.Extension.extLocOnce
    | throwError "expected per-declaration extension query for `extLocOnce`"
  unless entry.locations == #["Foo.bar"] do
    throwError "unexpected per-declaration location set for `extLocOnce`"

end Tests.Unit.Extension
