// ============================================================================
// 01 - A simple type checker for expressions with strings and numbers
// ============================================================================

// Types of expressions in our language - we have numbers and strings
type Type =
  | Number
  | String

// Expressions (similar as in lab02) - constants, operators, variables and if
// (The type checker will figure out the type of each expression.)
type Expression =
  | StringConst of string
  | NumberConst of int
  | Binary of string * Expression * Expression
  | Variable of string
  | If of Expression * Expression * Expression

// A typing context maps variable names to their types
// (note, VariableContext in lab02 mapped strings to values!)
type TypingContext = Map<string, Type>

// ----------------------------------------------------------------------------
// Type checker - a recursive function taking a typing context and expression;
// returns the Type of the expression, or fails with a type error.
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
      // TODO: Type-check binary expressions. The supported operators are
      // "*", "/", "+", "-". Both arguments must be Number and the result is
      // Number. Fail with an error for unknown operators or for non-number
      // arguments. (Hint: use set ["*"; "/"; "+"; "-"] to define the set.)

  | Variable v ->
      if ctx.ContainsKey v then ctx[v] else failwith "var not found"
      // TODO: Look up the variable type in the context using ctx.ContainsKey
      // and ctx[v]. If the variable is not in the context, fail with an error.

  | If(e1, e2, e3) ->
      match typeCheck ctx e1 with
      | Number ->
          let t1 = typeCheck ctx e2
          let t2 = typeCheck ctx e3
          if t1 = t2 then t1 else failwith "If doesnt have the same type"
      | _ -> failwith "If condition is not int"
      // TODO: Type-check the condition 'e1' and both branches 'e2', 'e3'.
      // * The condition must be Number
      // * Both branches must have the same type
      // * The overall type is the type of the branches

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let vars = Map.ofList ["num", Number]

// Type error: condition is a String, not a Number
let e1 =
  If(StringConst("oops"),
    Binary("+", NumberConst 40, NumberConst 2),
    Variable("num"))

// typeCheck vars e1 |> printfn "e1 %A" // should fail

// Type error: '+' applied to a String argument
let e2 =
  If(NumberConst 0,
    Binary("+", StringConst "40", NumberConst 2),
    Variable("num"))

// typeCheck vars e2 |> printfn "e2 %A" // should fail

// Type error: variable 'nummmm' is unbound
let e3 =
  If(NumberConst 0,
    Binary("+", NumberConst 40, NumberConst 2),
    Variable("nummmm"))

// typeCheck vars e3 |> printfn "e3 %A" // should fail

// Correctly typed: if 0 then 40+2 else num => result is Number
let e4 =
  If(NumberConst 0,
    Binary("+", NumberConst 40, NumberConst 2),
    Variable("num"))

typeCheck vars e4 |> printfn "e4 %A"

