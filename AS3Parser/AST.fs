module AST

module StringTools = 
    let join s (xs:seq<string>) = System.String.Join(s, xs)
    let split sep (str:string) = str.Split([|sep|])
    let brake s = "[" + s + "]"
    let paren s = "(" + s + ")"
    type String = char list
    type ShowS = String -> String
    
    let showChar c = (fun cs -> (c::cs)):ShowS
    let showString (xs:string) = (fun ys -> List.append (List.ofSeq xs) ys):ShowS
    let (nl:ShowS) = showString System.Environment.NewLine
    let showParen b = (fun p -> if b then showChar '(' << p << showChar ')' else p):(ShowS -> ShowS)
    let show (x:ShowS) = System.String.Join("", x [])
    let shows x = (showString (x.ToString())):ShowS
    let bet opened closed p = (showString opened << shows p << showString closed):ShowS
    let between (opened:ShowS) (closed:ShowS) p = (opened << shows p << closed):ShowS

module Reflect =
    open Quotations.DerivedPatterns
    open Quotations.Patterns
    open Microsoft.FSharp.Reflection    
    let unionCaseToList (func:Quotations.Expr<'a -> 'b>) =
        let rec f acc = function
            | IfThenElse(u, v, else') ->
                match u with
                | UnionCaseTest(_, e) -> 
                    let r = FSharpValue.MakeUnion(e, [||]) :?> 'a
                    let v = match v with Value (x, _) -> x :?> 'b | _ -> failwith ""
                    f <| (r,v)::acc <| else'
                    //Some((r, v), else')
                | x -> failwithf "fail at %A" x
            | Value (x, _) ->
                let v = x :?> 'b
                let all = 
                    FSharpType.GetUnionCases <| typeof<'a>
                    |> Array.map (fun e -> FSharpValue.MakeUnion(e, [||]) :?> 'a)
                    |> Set.ofArray
                let xs = Set.ofList <| List.map fst acc
                let xs = all - xs
                match Set.count xs with 
                | x when x = 1 -> (Seq.head xs,v)::(List.rev acc)
                | x -> failwithf "not realized %d of %A" <| x - 1 <| xs
            | _ -> List.rev acc
//        assert
//            match <@ fromOpMatch @> with
//            | Lambda (_, Call(_, MethodWithReflectedDefinition(Lambda(_, expr)), _)) -> 
//                match expr with
//                | IfThenElse(u, v, else') ->
//                    match u with
//                    | UnionCaseTest(_, e) -> e
//            true
        match func with
        | Lambda (_, Call(_, MethodWithReflectedDefinition(Lambda(_, expr)), _)) ->
            f [] expr
            //Seq.unfold f expr |> List.ofSeq
        | x -> failwithf "%A" x
    let createUnionCases (inst:'a) =
        FSharpType.GetUnionCases <| inst.GetType() //typeof<Ops>
        |> Array.map (fun e -> FSharpValue.MakeUnion(e, [||]) :?> 'a) |> List.ofArray

open StringTools
type Access = 
    | Public | Private
    override x.ToString() = match x with Public -> "public" | Private -> "private"
type Type = 
    Str | Bool | Int | UndefType | Void | SType of string
    override x.ToString() =
        match x with
        | Str -> "String" | Int -> "int" | UndefType -> "*"
        | Void -> failwith "retest void" | SType x -> x | Bool -> "Boolean"
        
type ValT = 
    | String' of string
    | Float' of float
    | Bool' of bool
    | Int' of int
    override x.ToString() =
        let f' p x = if p then showParen true (shows x) else shows x
        let f = function
            | Int' x -> f' (x < 0) x
            | Float' x -> f' (x < 0.) x
            | String' x -> showChar '\"' << shows x << showChar '\"'
            | Bool' x -> shows x
        show <| f x

type Val = 
    Null | Undefined | ValT of ValT
    override x.ToString() =
        let f = function 
            | Null -> showString "null" | Undefined -> showString "undefined"
            | ValT x -> shows x
        f x |> show
            
type DefVar = 
    { NameVar: string; Type:Type; Value:Val }
    override x.ToString() = 
        between (bet "var " ":" x.NameVar) <| showString " = " <| x.Type << shows x.Value
        //shows "var " << shows x.NameVar << showChar ':' << shows x.Type << shows " = " << shows x.Value
        |> show

type Ops =
    | Or // ||
    | And // &&
    | BOr // Bitwise |
    | BXor // Bitwise ^ (xor)
    | BAnd // Bitwise &
    | Ee | Ne | Eee | Nee // == != === !==
    | Lt | Gt | Le | Ge // < > <= >= instanceof
    | GG | LL | GGG // << >> >>>
    | Plus | Minus
    | Times | Divide | Mod
    | Dot
    override x.ToString () = failwith "Ops.ToStrion() not supp"


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Ops =
    [<ReflectedDefinition>]
    let toString = function
        | Times -> "*"
        | Divide -> "/"
        | Mod -> "%"
        | Plus -> "+"
        | Minus -> "-"
        | GG -> ">>"
        | LL -> "<<"
        | GGG -> ">>>"
        | Lt -> "<"
        | Gt -> ">"
        | Le -> "<="
        | Ge -> ">="
        | Ee -> "=="
        | Ne -> "!="
        | Eee -> "==="
        | Nee -> "!=="
        | BAnd -> "&"
        | BXor -> "^"
        | BOr -> "|"
        | And -> "&&"
        | Or -> "||"
        | Dot -> "."
    let ops = Reflect.unionCaseToList <@toString@>
    (*
    let toString = 
        let m = Map.ofList ops
        fun x -> match Map.tryFind x m with Some x -> x | None -> failwithf "not found %A" x *)
    let fromString = 
        let m = Map.ofList <| List.map (fun (a,b) -> b,a) ops
        fun x -> match Map.tryFind x m with Some x -> x | None -> failwithf "not found %A" x

//"""Times	*
//Divide	/
//Mod	%
//Plus	+
//Minus	-
//GG	>>
//LL	<<
//GGG	>>>
//Lt	<
//Gt	>
//Le	<=
//Ge	>=
//Ee	==
//Ne	!=
//Eee	===
//Nee	!==
//BAnd	&
//BXor	^
//BOr	|
//And	&&
//Or	||""" |> split '\n' |> Array.map (split '\t' >> function [|t; op|] -> sprintf "| %s -> \"%s\"" t op | x -> failwithf "fail at %A" x) |> join "\n"

type PrefOp =
    Neg | IncPref | DecPref | ConsB
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PrefOp =
    [<ReflectedDefinition>]
    let toString = function | IncPref -> "++" | DecPref -> "--" | Neg -> "-" | ConsB -> "~"
    let ops = Reflect.unionCaseToList <@toString@>
    let fromString = 
        let m = Map.ofList <| List.map (fun (a,b) -> b,a) ops
        fun x -> match Map.tryFind x m with Some x -> x | None -> failwithf "not found %A" x
type PostOp =
    IncPost | DecPost
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module PostOp =
    [<ReflectedDefinition>]
    let toString = function | IncPost -> "++" | DecPost -> "--"
    let ops = Reflect.unionCaseToList <@toString@>
    let fromString = 
        let m = Map.ofList <| List.map (fun (a,b) -> b,a) ops
        fun x -> match Map.tryFind x m with Some x -> x | None -> failwithf "not found %A" x

//type UnarOp = 
//    Neg | IncPost | IncPref | DecPost | DecPref | ConsB
//    override x.ToString() = failwith "unarOp.ToString() not suppored"
//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//[<RequireQualifiedAccess>]
//module UnarOp =
//    [<ReflectedDefinition>]
//    let toString = function | IncPref | IncPost -> "++" | DecPost | DecPref -> "--" | Neg -> "-" | ConsB -> "~"
//    //let ops = Reflect.unionCaseToList <@toString@>
//    let fromString _ = failwith "при вызове \"++\" что возвращать PPPost или PPPref?"


module Precedences = 
    type T = OpB of Ops | PostB of PostOp | PrefB of PrefOp
    
    let prec = function
        | OpB Or -> 1
        | OpB And -> 2
        | OpB BOr -> 3
        | OpB BXor -> 4
        | OpB BAnd -> 5
        | OpB Ee | OpB Ne | OpB Eee | OpB Nee -> 6
        | OpB Lt | OpB Gt | OpB Le | OpB Ge -> 7
        | OpB GG | OpB LL | OpB GGG -> 8
        | OpB Plus | OpB Minus -> 9
        | OpB Times | OpB Divide | OpB Mod -> 10
        | PrefB IncPref | PrefB DecPref | PrefB Neg | PrefB ConsB -> 11 // ++x --x + - ~ ! delete typeof void
        | PostB IncPost | PostB DecPost -> 12
        | OpB Dot -> 13

type Expr = 
    | PrefExpr of PrefOp * Expr
    | PostExpr of PostOp * Expr
//    | GetProperty of string * string * Expr
//    | GetMethod of string * string * Expr
    | Expr of Ops * Expr * Expr
    | FuncCallE of string * Expr list
    | Var of string
    | ValE of ValT
    override x.ToString() = 
        let rec f = function
        | ValE v -> shows v
        | Var v -> shows v
        //| FuncCallE("idx", (Var x)::t) -> x + (List.map expr t |> join ", " |> brake)
        | FuncCallE(x, xs) -> showString x << (List.map string xs |> join ", " |> shows |> showParen true)
       (* | UnarExpr(op, e) -> 
            match op with
            | Neg -> shows op << (shows e |> showParen true)
            | IncPref | DecPref -> shows op << shows e
            | IncPost | DecPost -> shows e << shows op *)
        //| PrefExpr(Neg, e) -> shows Neg << (shows e |> showParen true)
        | PrefExpr(op, e) -> showString (PrefOp.toString op) << shows e
        | PostExpr(op, e) -> shows e << showString (PostOp.toString op)
        | Expr(Dot, e1, e2) -> f e1 << showChar '.' << f e2
        | Expr(op, e1, e2) -> 
            let prec = Precedences.OpB >> Precedences.prec
            let f x = 
                match x with
                //| ValE _ | Var _ | FuncCallE _ -> f x | _ -> showParen true <| f x
                | Expr(op', _, _) -> showParen (prec op > prec op') <| f x
                | x -> f x
            f e1 << showChar ' ' << showString (Ops.toString op) << showChar ' ' << f e2 //(showParen true <| shows e2)
        f x |> show

type FuncBody = 
    | Assert of string * Expr
    | DefVar of DefVar
    | FuncCall of string * Expr list
    | Return of Expr
    | If of Expr * FuncBody list * FuncBody list
    | While of Expr * FuncBody list
    (*override x.ToString() =
        let rec print = function
        | If(cond, body, bodyElse) -> 
            let f = function [] -> "{ }" | [x] -> showChar '\n' print x
            string cond
        print x *)
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FuncBody = ()
    (*let rec toString = function
        | If(cond, body, bodyElse) ->
            Expr.to *)
type Func = { Access:Access; Name:string; Args:DefVar list; Return:string; Body:FuncBody list }