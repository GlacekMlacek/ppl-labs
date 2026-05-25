// ============================================================================
// 07 - Add a simple data type - tuples
// ============================================================================

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  // NOTE: A tuple value consisting of two other values.
  // (Think about why we have 'Value' here but 'Expression'
  // in the case of 'ValClosure' above!)
  | ValTuple of Value * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Log of string * Expression
  // NOTE: 'Tuple' represents two-element tuple constructor
  // and 'TupleGet' the destructor (accessing a value)
  // Use 'true' for #1 element, 'false' for #2. This is not
  // particularly descriptive, but it works OK enough.
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

and VariableContext = 
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "unsupported val type in binary"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)
  | Unary(op, e) ->
      let v = evaluate ctx e
      match v with
        | ValNum n ->
          match op with
            | "-" -> ValNum(-n)
            | _ -> failwith "unsuppoered unary operator"
        | _ -> failwith "Uunsopported val type in unary"
  | If(pred, t, f) ->
      let p = evaluate ctx pred
      match p with
          | ValNum(0) -> evaluate ctx f
          | ValNum(_) -> evaluate ctx t
          | _ -> failwith "unsupported val type in if"
  | Log(msg, e) -> 
      let res = evaluate ctx e
      printfn "%s: %A" msg res
      res
  | Let(v, earg, ebody) ->
      let arg = evaluate ctx earg
      let nctx = Map.add v arg ctx
      evaluate nctx ebody
  | Lambda(v, e) ->
      ValClosure(v, e, ctx)
  | Application(e1, e2) ->
      let ee1 = evaluate ctx e1
      match ee1 with
        | ValClosure(name, lbody, lctx) -> evaluate (Map.add name (evaluate ctx e2) lctx) (Let(name, e2, lbody))
        | _ -> failwith "was expecting a closure"
  | Tuple(e1, e2) ->
      ValTuple(evaluate ctx e1, evaluate ctx e2)

  | TupleGet(b, e) ->
      let ee = evaluate ctx e
      match b, ee with
          | true, ValTuple(t, _) -> t
          | false, ValTuple(_, f) -> f
          | _, _ -> failwith "expecting tuple"

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - simple tuple example (using the e#1, e#2 notation for field access)
//   (2*21, 123)#1
//   (2*21, 123)#2
let ed1 = 
  TupleGet(true, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed1 |> printfn "ed1 %A\n"

let ed2 = 
  TupleGet(false, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed2 |> printfn "ed2 %A\n"

// Data types - trying to get a first element of a value
// that is not a tuple (This makes no sense and should fail)
//   (42)#1
let ed3 = 
  TupleGet(true, Constant(42))
// evaluate Map.empty ed3 |> printfn "ed3 %A\n" // should fail

