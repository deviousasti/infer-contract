module App

open Browser.Dom

let jsonEditor = Ace.createEditor "json-editor" "json"

let fsharpEditor =
    Ace.createEditor "fsharp-editor" "fsharp"

let updateUi _ =
    jsonEditor.getValue ()
    |> InferType.infer
    |> InferType.generateSource 
    |> fsharpEditor.setValue

jsonEditor.on ("change", updateUi)
window.addEventListener ("load", updateUi)
