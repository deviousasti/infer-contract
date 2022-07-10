module InferType

open Fable.SimpleJson

type FsType =
    | FUndecided
    | FBool
    | FInt
    | FFloat
    | FDecimal
    | FString
    | FDateTime
    | FArray of FsType
    | FOption of FsType
    | FMap of FsType
    | FUmx of FsType * unit:string 
    | FAnonymous of TypeDefintion
    | FNamedType of string * TypeDefintion

and Trivia =
    | NoTrivia
    | Inline of string
    | Multiline of string list

and TypeDefintion = Map<string, Trivia * FsType>

let Undefined = NoTrivia, FUndecided

module FsType =

    let coalesceTrivia triv1 triv2 =
        let triv1, triv2 = if triv1 < triv2 then triv1, triv2 else triv2, triv1
        match triv1, triv2 with
        | _ when triv1 = triv2 -> triv1
        | NoTrivia, t
        | t, NoTrivia -> t
        | Inline inl1, Inline inl2 -> Inline $"{inl1}, {inl2}"
        | Multiline lst1, Multiline lst2 -> Multiline(lst1 @ lst2)
        | Multiline lst, Inline inl
        | Inline inl, Multiline lst -> Multiline(inl :: lst)

    let rec coalesceType type1 type2 =
        let unwrap (FOption (FOption _  as t) | t) = t

        let rec comb t1 t2 =
            match t1, t2 with
            | FUndecided, t -> t
            | FOption t1, t2 -> unwrap <| FOption (comb t1 t2)
            | FMap t1, FMap t2 -> FMap (comb t1 t2)
            | FArray t1, FArray t2 -> FArray (comb t1 t2)
            | FAnonymous map1, FAnonymous map2 ->
                 FAnonymous (combMap map2 map1)
            | t1, t2 when t2 > t1 -> t2
            | higher, _ -> higher
            
        and combMap map1 map2 = 
            let keys = Map.toSeq >> Seq.map fst
            let allKeys = (keys map1, keys map2) ||> Seq.append
            seq { 
            for key in allKeys do
                let find = Map.tryFind key
                match find map1, find map2 with
                | Some (triv, t1), None 
                | None, Some (triv, t1) ->
                    key, (triv, unwrap (FOption t1))
                | Some def1, Some def2 ->
                    key, coalesce def1 def2
                | None, None -> invalidOp "This case is illogical"
            }
            |> Map.ofSeq           

        comb type1 type2

    and coalesce (triv1, def1) (triv2, def2) =
        coalesceTrivia triv1 triv2, coalesceType def1 def2

let (|Trivia|) = function NoTrivia -> "" | Inline inl -> inl | Multiline lines -> String.concat "\n" lines
let (|UpperCase|_|) (str: string) =
    iif (str = str.ToUpperInvariant()) str

let (|JDateTime|_|) =
    Matches "[0-9]{4}-[0-9]{2}-[0-9]{2}T([0-9]{2}:){2}[0-9]{2}[+|-][0-9]{2}:[0-9]{2}"
let (|JUrl|_|) = Matches "http[s]?://"
let (|JInt|_|) num = iif (truncate num = num) (int num)
let (|JFloat|_|) num = iif ((string num).Contains "e") num
let (|JLiteral|_|) = function JString _ | JBool _ | JNumber _ -> Some () | _ -> None
let (|JBag|_|) map =
    iif (Map.forall (tuple (function UpperCase _, JLiteral -> true | _ -> false)) map) map

let makeType (ignoreTrivia: bool) (json: Json) =    
    let rec construct json =
        match json with
        | JObject (JBag map) ->
            let (trivia, sub) =
                map
                |> Map.fold (fun def key value -> 
                    let (Trivia trivia, sub) = construct value
                    let def' = Inline $"{key}: {trivia}", sub
                    FsType.coalesce def def') Undefined

            trivia, FMap sub
        | JObject jobj ->
            let sub = jobj |> Map.map (fun _ v -> construct v)
            NoTrivia, FAnonymous sub
        | JString "" -> NoTrivia, FOption FString
        | JString (JDateTime dt) -> Inline dt, FDateTime
        | JString (JUrl dt) -> Inline dt, FUmx (FString, "url")
        | JString str -> Inline str, FString
        | JBool b -> Inline(string b), FBool
        | JNumber (JInt num) -> Inline(string num), FInt
        | JNumber (JFloat num) -> Inline(string num), FFloat
        | JNumber num -> Inline(num.ToString("N2")), FDecimal
        | JArray [] -> NoTrivia, FArray FUndecided
        | JArray arr ->
            let (_, sub) =
                arr
                |> List.map construct
                |> List.fold FsType.coalesce Undefined

            NoTrivia, FArray sub
        | JNull -> Inline "null", FOption FUndecided
        |> fun (trivia, fstype) ->
            if ignoreTrivia then NoTrivia, fstype
            else trivia, fstype
    
    match construct json with
    | _, FArray (FAnonymous def) -> FNamedType("Message", def)
    | _, e -> e
    |> List.singleton

let reservedKeywords =
    // https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/keyword-reference
    // Result of:
    // Array.from(document.querySelectorAll("td > code")).map(a => `"${a.innerText}"`).join("; ")
    set ["abstract"; "and"; "as"; "assert"; "base"; "begin"; "class"; "default"; "delegate"; "do"; "done"; "downcast"; "downto"; "for"; "elif"; "else if"; "else"; "end"; "begin"; "exception"; "extern"; "false"; "finally"; "try"; "fixed"; "for"; "fun"; "function"; "fun"; "match"; "global"; "if"; "in"; "inherit"; "inline"; "interface"; "internal"; "lazy"; "let"; "let!"; "match"; "match!"; "member"; "module"; "mutable"; "namespace"; "new"; "not"; "not struct"; "null"; "of"; "open"; "or"; "or"; "||"; "override"; "private"; "public"; "rec"; "return"; "return!"; "select"; "static"; "struct"; "then"; "to"; "for"; "true"; "try"; "with"; "finally"; "type"; "upcast"; "use"; "let"; "Dispose"; "use!"; "let!"; "Dispose"; "val"; "void"; "void"; "when"; "while"; "with"; "match"; "yield"; "yield!"; "const"; "const"; "method"; "member"; "constructor"; "new"; "atomic"; "eager"; "let eager"; "let lazy"; "object"; "recursive"; "rec"; "functor"; "measure"; "volatile"; ]

let rec makeSource level typeDef =
    let indentAt lvl item = String.replicate lvl "  " + item

    let enclose sopen sclose items =
        seq {
            yield sopen //|> indentAt level

            for item in items do
                yield item |> indentAt (level + 1)

            yield sclose |> indentAt level
        }
        |> String.concat "\n"

    let rec makeProp level (name, (trivia, sub)) =
        let inner = makeSource level sub
        let name = 
            if reservedKeywords |> Set.contains name || name.Contains(" ")
            then $"``{name}``" 
            else name
        match trivia with
        | NoTrivia -> [ $"{name} : {inner}" ]
        | Inline c -> [ $"{name} : {inner} // {c}" ]
        | Multiline lines ->
            lines
            |> List.map (fun l -> $"// {l}")
            |> List.append [ $"{name} : {inner}" ]

    let makeProps map =
        map |> Map.toSeq |> Seq.collect (makeProp (level + 1))

    let make = makeSource level

    match typeDef with
    | FNamedType (name, typeDef) ->
        makeProps typeDef
        |> enclose $"type {name} = {{" $"}} // {name}"
    | FAnonymous typeDef -> makeProps typeDef |> enclose "{|" "|}"
    | FMap sub -> $"Map <string, {make sub}>"
    | FString -> nameof string
    | FInt -> nameof int
    | FFloat -> nameof float
    | FDecimal -> nameof decimal
    | FArray sub -> $"{make sub} []"
    | FOption sub -> $"{make sub} option"
    | FDateTime -> nameof System.DateTimeOffset
    | FBool -> nameof bool
    | FUmx (sub, param) -> $"{make sub}<{param}>"
    | FUndecided -> "obj"

let infer (text: string) =
    match SimpleJson.tryParse text with
    | Some (JObject _ as json) -> 
        Ok ("Single message contract", makeType false (JArray [json]))
    | Some (JArray items as json) -> 
        let message = 
            if items |> List.map (makeType true) |> List.distinct |> List.length = 1 
            then "Multiple examples with same contract"
            else "Multiple examples with unified contract"

        Ok (message, makeType false json)
    | None -> 
        try
            Fable.Core.JS.JSON.parse text |> ignore
            Error "Could not parse json"
        with ex ->
            Error ("Parse error: \n" + ex.Message)            
    | _ -> Error "Unsupported json construct"

let generateSource =
    function
    | Ok (header, def) -> 
        let source = def |> Seq.map (makeSource 0) |> String.concat "\nand\n"
        $"// {header}\n{source}"
    | Error error -> $"(* {error} *)"
