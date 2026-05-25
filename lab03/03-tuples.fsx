// ============================================================================
// 03 - Extend the type checker with tuple types
// ============================================================================

type Type =
  | Number
  | String
  | Function of Type * Type
  // NOTE: 'Tuple(t1, t2)' is the type of a pair where the first element
  // has type t1 and the second element has type t2.
  | Tuple of Type * Type

type Expression =
  | StringConst of string
  | NumberConst of int
  | Binary of string * Expression * Expression
  | Variable of string
  | If of Expression * Expression * Expression
  | Let of string * Expression * Expression
  | Lambda of string * Type * Expression
  | Application of Expression * Expression
  // NOTE: Added MakeTuple (constructor) and GetTuple (getter)
  | MakeTuple of Expression * Expression
  | GetTuple of bool * Expression

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
  | MakeTuple(e1, e2) -> Tuple(typeCheck ctx e1, typeCheck ctx e2)
  | GetTuple(b, e) ->
      match typeCheck ctx e with
      | Tuple(t1, t2) -> if b then t1 else t2
      | _ -> failwith "was expecting a tuple"

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let vars = Map.ofList ["num", Number]

// Correctly typed: ("hello world", num+10) => Tuple(String, Number)
let et1 =
  MakeTuple(StringConst("hello world"),
    Binary("+", Variable "num", NumberConst 10))

typeCheck vars et1 |> printfn "et1 %A"

// Correctly typed: let t = ("hello world", num+10) in t#2 + 1 => Number
let et2 =
  Let("t",
    MakeTuple(StringConst("hello world"),
      Binary("+", Variable "num", NumberConst 10)),
    Binary("+", GetTuple(false, Variable("t")), NumberConst 1) )

typeCheck vars et2 |> printfn "et2 %A"

// Type error: + applied to string and a number
let et3 =
  Let("t",
    MakeTuple(StringConst("hello world"),
      Binary("+", Variable "num", NumberConst 10)),
    Binary("+", GetTuple(true, Variable("t")), NumberConst 1) )

// typeCheck vars et3 |> printfn "et3 %A" // should fail

// Type error: 't' is bound to a Number, not a tuple
let et4 =
  Let("t", Binary("+", Variable "num", NumberConst 10),
    GetTuple(false, Variable("t")) )

// typeCheck vars et4 |> printfn "et4 %A" // should fail

