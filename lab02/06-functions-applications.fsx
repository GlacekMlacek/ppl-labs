// ============================================================================
// 06 - Functions and application - now with proper lexical scoping
// ============================================================================

type Value = 
  | ValNum of int 
  // NOTE: The right way to handle lexical scoping is to remember the
  // variable context as it was available when the function was defined.
  // We do this by adding VariableContext to our closure value.
  // (Compilers for C# and similar languages with lambdas need to
  // capture variables when you define function in the same way!)
  | ValClosure of string * Expression * VariableContext

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Log of string * Expression
  | Let of string * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression

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

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Basic function declaration (should return closure)
//   (fun x -> x * 2) 
let ef1 = 
  Lambda("x", Binary("*", Variable("x"), Constant(2)))
evaluate Map.empty ef1 |> printfn "ef1 %A\n"

// Basic function calls (should return number)
//   (fun x -> x * 2) 21
let ef2 = 
  Application(
    Lambda("x", Binary("*", Variable("x"), Constant(2))),
    Constant(21)
  )
evaluate Map.empty ef2 |> printfn "ef2 %A\n"

// This did not work with dynamic scoping, but it works now.
// The variable 'n' is captured when creating a closure and
// so you should get 42.
//
//   let f = 
//     (let n = 21 in (fun x -> n*x)) 
//   f 2
//
let efunarg =
  Let("f", 
    Let("n", Constant 21, 
      Lambda("x", Binary("*", Variable "n", Variable "x"))),
    Application(Variable "f", Constant 2)
  )

evaluate Map.empty efunarg |> printfn "efunarg %A\n"

// On the other hand, the following no longer works with lexical
// scoping, because 'n' is not defined when we create the closure!
//
//   let f = (fun x -> n*x)
//   let n = 21
//   f 2
//
let edyn =
  Let("f", Lambda("x", Binary("*", Variable "n", Variable "x")),
    Let("n", Constant 21, 
      Application(Variable "f", Constant 2)))

// evaluate Map.empty edyn |> printfn "edyn %A\n"

