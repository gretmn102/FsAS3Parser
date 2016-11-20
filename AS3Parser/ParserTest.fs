module ParserTest

#if INTERACTIVE
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"
#endif

#if INTERACTIVE
#load "AST.fs"
#endif

#if INTERACTIVE
#load "AS3Parser.fs"
#endif

open FParsec
open AS3Parser
open AST.StringTools

let testR p str =
    match run p str with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg
let testD p str =
    let afterAST = testR p str
    let after = string afterAST
    let beforeAST = testR p after 
    let before = beforeAST |> string
    let strEq = after = before
    let astEq = afterAST = beforeAST
    if strEq && astEq then before 
    else failwithf "strEq = %b\nastEq = %b\nafterStr = %s\nbeforeStr = %s\nafterAST = %A\nbeforeAST = %A" strEq astEq after before afterAST beforeAST
assert
    testD infix2.expr "g + a*(b(10, e*f)-c())" = "g + a * (b(10, e * f) - c())"
assert
    testD infix2.expr "(2 + 2) * 2" = "(2 + 2) * 2"
assert
    testD infix2.expr "param1.charCodeAt(param1.length - 2) == param2 && param1.charCodeAt(2) == param3" = 
        "param1.charCodeAt(param1.length - 2) == param2 && param1.charCodeAt(2) == param3"
assert
    testD infix2.expr "param1.substr(0, -3)" = "param1.substr(0, -3)"
assert
    testD infix2.expr "param1.substr(0, --x)" = "param1.substr(0, --x)"
//assert
//    testR infix2.expr "++a + ++b + c++"
//    testD infix2.expr "++a + ++b + c++" = "++a + ++b + c++"

let varsDefS = "var _loc_4:* = 0;
            var _loc_5:* = null;
            var _loc_6:* = undefined; }"
//test (ws >>. pVarDef) varsDefS

test states varsDefS

let ifS =  """if (param1.charCodeAt(param1.length - 2) == param2 && param1.charCodeAt(2) == param3)
            { }"""
test pIf ifS

let f x =
    let p =
        let p = char_ws '|' >>. identifier
        ws >>. (many p)

    testR p x |> join " | OpB " |> (bet "| OpB " " -> ") |> show//|> bet "[" "]" |> show
//"""    | Times | Divide | Mod
//    | Plus | Minus
//    | GG | LL | GGG // << >> >>>
//    | Lt | Gt | Le | Ge // < > <= >= instanceof
//    | Ee | Ne | Eee | Nee // == != === !==
//    | BAnd // Bitwise &
//    | BXor // Bitwise ^ (xor)
//    | BOr // Bitwise |
//    | And // &&
//    | Or // ||""" |> split '\n' |> Array.map (f) |> List.ofArray |> List.rev |> join "\n"

