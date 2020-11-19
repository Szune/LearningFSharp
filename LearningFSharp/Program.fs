// Learn more about F# at http://fsharp.org

open System

type ArgNameValue = {Name:string;Value:string}
type ArgName = {Name:string;}
type ArgState =
    | ArgName of ArgName
    | ArgNameValue of ArgNameValue
    | Input of String
    | Nothing
    
let ParseArgs (argv:String list) =
    let rec innerParse parsedArgs current (remaining:String list) =
        match remaining with
        | [] ->
                if current <> Nothing then
                    current :: parsedArgs |> List.rev
                else
                    parsedArgs
        | arg::tail -> // no need to use "rest |> List.head" and "rest |> List.tail" :D
               if arg.StartsWith("-") then
                   match current with
                   | Nothing -> innerParse [] (ArgName{Name=arg}) tail
                   | Input(_) -> innerParse (current :: parsedArgs) (ArgName{Name=arg}) tail
                   | ArgName(_) -> innerParse (current :: parsedArgs) (ArgName{Name=arg}) tail
                   | ArgNameValue(_) -> innerParse (current :: parsedArgs) (ArgName{Name=arg}) tail
               else
                   match current with
                   | Nothing -> innerParse [] (Input arg) tail
                   | Input(v) -> innerParse [] (Input (v + " " + arg)) tail
                   | ArgName({Name=v}) -> innerParse parsedArgs (ArgNameValue{Name=v;Value=arg}) tail
                   | ArgNameValue(v) -> innerParse parsedArgs (ArgNameValue{v with Value = v.Value + " " + arg}) tail
    innerParse [] Nothing argv

[<EntryPoint>]
let main argv =
    argv |> printfn "Incoming args: %A"
    argv |> Array.toList |> ParseArgs |> printfn "Parsed args: %A"
    0 // return an integer exit code
