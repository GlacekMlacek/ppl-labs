// ============================================================================
// 02 - Extend the type checker with let bindings and functions
// ============================================================================

type Type =
  | Number
  | String
  // NOTE: 'Function(t1, t2)' is the type of a function from t1 to type t2.
  // For example, a function that takes a number and returns a string
  // has a type Function(Number, String).
  | Function of Type * Type

type Expression =
  | StringConst of string
  | NumberConst of int
  | Binary of string * Expression * Expression
  | Variable of string
  | If of Expression * Expression * Expression
  | Let of string * Expression * Expression
  // NOTE: Lambda carries a type annotation for its argument - when 
  // writing 'fun x -> ...' the programmer must say what type 'x' has. 
  | Lambda of string * Type * Expression
  | Application of Expression * Expression

type TypingContext = Map<string, Type>

// ----------------------------------------------------------------------------
// Type checker
// ----------------------------------------------------------------------------

let rec typeCheck (ctx:TypingContext) expr =
  match expr with
  | StringConst _ -> String
  | NumberConst _ -> Number
  | Binary(op, l, r) ->
      let ops = set ["*"; "/"; "+"; "-"]
      match (typeCheck ctx l), (typeCheck ctx r) with
      | (Number, Number) -> if ops.Contains op then Number else failwith "Unsupported operation"
      | _ -> failwith "Invalid bin args"
  | Variable v ->
      if ctx.ContainsKey v then ctx[v] else failwith "var not found"
  | If(e1, e2, e3) ->
      match typeCheck ctx e1 with
      | Number ->
          let t1 = typeCheck ctx e2
          let t2 = typeCheck ctx e3
          if t1 = t2 then t1 else failwith "If doesnt have the same type"
      | _ -> failwith "If condition is not int"
  | Lambda(v, t, e) ->
      Function(t, typeCheck (Map.add v t ctx) e)
  | Application(e1, e2) ->
      match typeCheck ctx e1 with
      | Function(t1, t2) -> if (typeCheck ctx e2) = t1 then t2 else failwith "arg types not matching"
      | _ -> failwith "was expecting a function type"
  | Let(v, e1, e2) -> typeCheck ctx (Lambda(v, typeCheck ctx e1, e2))

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let vars = Map.ofList ["num", Number]

// Correctly typed: let x = 10+20 in x => Number
let ef1 =
  Let("x", Binary("+", NumberConst 10, NumberConst 20),
    Variable("x"))

typeCheck Map.empty ef1 |> printfn "ef1 %A"

// Type error: 'x' is not in scope in the binding expression
let ef2 =
  Let("x", Variable("x"),
    Binary("+", NumberConst 10, NumberConst 20))

// typeCheck Map.empty ef2 |> printfn "ef2 %A" // should fail

// Correctly typed: fun (x:Number) -> x+20 => Function(Number, Number)
let ef3 =
  Lambda("x", Number,
    Binary("+", Variable "x", NumberConst 20))

typeCheck Map.empty ef3 |> printfn "ef3 %A"

// Type error: '+' applied to a String argument (x has type String)
let ef4 =
  Lambda("x", String,
    Binary("+", Variable "x", NumberConst 20))

// typeCheck Map.empty ef4 |> printfn "ef4 %A" // should fail

// Correctly typed: (fun (x:Number) -> x+10) 32 => Number
let ef5 =
  Application(
    Lambda("x", Number, Binary("+", Variable "x", NumberConst 10)),
    NumberConst(32) )

typeCheck Map.empty ef5 |> printfn "ef5 %A"

// Type error: function expects Number but called with String
let ef6 =
  Application(
    Lambda("x", Number, Binary("+", Variable "x", NumberConst 10)),
    StringConst("32") )

// typeCheck Map.empty ef6 |> printfn "ef6 %A" // should fail

// Type error: 32 is not a function
let ef7 =
  Application(
    NumberConst(32),
    Lambda("x", Number, Binary("+", Variable "x", NumberConst 10)))

// typeCheck Map.empty ef7 |> printfn "ef7 %A" // should fail

