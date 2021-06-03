module Ace

open Fable.Core.JsInterop

type 'a Callback = 'a -> unit

type Editor =
    abstract getValue : unit -> string
    abstract setValue : string -> unit
    abstract on : event : string * callback: Callback<_> -> unit
    abstract session : Editor

type Ace =
    abstract edit : string * 'config -> Editor

let ace : Ace = importAll "ace-builds/src-min-noconflict/ace" 
importAll "ace-builds/src-min-noconflict/mode-json"
importAll "ace-builds/src-min-noconflict/mode-fsharp"

let createEditor elementId mode =
    ace.edit (elementId, {| mode = $"ace/mode/{mode}"; selectionStyle = "text"; useWorker = false|})
    