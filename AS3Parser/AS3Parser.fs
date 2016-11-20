module AS3Parser
open System
open FParsec
open AST

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


let ws = spaces
let str s = pstring s
let str_ws s = pstring s .>> ws
let char_ws c = pchar c .>> ws
let bet opened closed = between <| char_ws opened <| pchar closed
let bet_ws opened closed p = bet opened closed p .>> ws
let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws // skips trailing whitespace
//let ptype = str "String" <|> str "int" <|> str

let varTypeDef =
    (char_ws '*' >>% UndefType) <|> (identifier |>> SType) .>> ws

let pFunDef pFunStates = 
    let var = 
        pipe2 (identifier .>> char_ws ':') varTypeDef
              (fun name t -> { NameVar = name; Type = t; Value = Null })
    //test var "some_var_2 : sOmeType"
    let vars = sepBy var (str_ws ",")
    //test vars "some_var_2 : sOmeType, ks: sd"
    let paccess = (str "public" >>% Public) <|> (str "private" >>% Private) .>> ws
    paccess >>= fun access -> 
    str_ws "function" >>. identifier >>= fun name ->
    bet_ws '(' ')' vars >>= fun vars ->
    char_ws ':' >>. identifier >>= fun returnT ->
    bet_ws '{' '}' pFunStates >>= fun body ->
    preturn { Access = access; Name = name; Args = vars; Return = returnT; Body = body }
let sFunDef = 
    """public function tr( param1:String, param2:int, param3:int) : String
        {
    
    }"""
test <| pFunDef (ws >>. preturn []) <| sFunDef

module infix2 =
    (*type Expr = 
        | InfixOpExpr of string * Expr * Expr
        | Number of int
        | Var of string
        | Call of string * Expr list *)

    let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
    let expr = opp.ExpressionParser
    let term = 
        let varOrCall = 
            let pcall name = 
                bet_ws '(' ')' (sepBy expr (pchar ',' >>. ws)) >>= fun args ->
                preturn <| FuncCallE(name, args)
            identifier >>= fun name -> pcall name <|> (preturn <| Var name)
        (pint32 .>> ws |>> (Int' >> ValE)) <|> varOrCall <|> bet_ws '(' ')' expr

        //(identifier |>> Var)
    opp.TermParser <- term

    type A = Associativity
    
    let addInf opT = 
        let op = Ops.toString opT
        let prec = Precedences.prec <| Precedences.OpB opT
        opp.AddOperator(InfixOperator(op, ws, prec, A.Left, fun x y -> Expr(opT, x, y)))
    Reflect.createUnionCases Ne |> List.iter addInf
    
    let addPref unT = 
        let op = PrefOp.toString unT
        let prec = Precedences.prec <| Precedences.PrefB unT
        opp.AddOperator(PrefixOperator(op, ws, prec, false, fun x -> PrefExpr(unT, x)))
    
    ["~"; "++"; "--"; "-"] |> List.iter (PrefOp.fromString >> addPref)

    let addPost unT = 
        let op = PostOp.toString unT
        let prec = Precedences.prec <| Precedences.PostB unT
        opp.AddOperator(PostfixOperator(op, ws, prec, false, fun x -> PostExpr(unT, x)))

    ["++"; "--"] |> List.iter (PostOp.fromString >> addPost)


let pVarDef = 
    let varVal = 
        let f s t = str_ws s >>% t
        f "null" Null <|> f "undefined" Undefined <|> (pint32 |>> (Int' >> ValT))

    str_ws "var" >>. identifier >>= fun name ->
    char_ws ':' >>. varTypeDef >>= fun t ->
    bet_ws '=' ';' varVal |>> fun expr ->
    { NameVar = name
      Type = t
      Value = expr } |> DefVar

let state = pVarDef
let states = many state

let pIf = 
    let st = bet_ws '{' '}' states <|> (state |>> fun x -> [x])
    str_ws "if" >>. bet_ws '(' ')' infix2.expr >>= fun cond ->
    st >>= fun body ->
    ((str_ws "else" >>. st) |>> fun bodyElse -> If(cond, body, bodyElse)) <|> (preturn <| If(cond, body, []))
    