[<AutoOpen>]
module Infra

open System.Text.RegularExpressions


let tuple f a b = f (a,b)
let iif condition value = if condition then Some value else None
let Matches pattern str = iif (Regex.IsMatch(str, pattern)) str

