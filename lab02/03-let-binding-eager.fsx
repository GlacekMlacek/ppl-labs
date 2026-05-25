// ============================================================================
// 03 - Adding let binding with eager evaluation
// ============================================================================

type Value = 
  | ValNum of int 

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression

  // NOTE: Added a definition for let binding. Let(v, earg, ebody) means:
  // 
  //  (let v = earg in ebody)
  //
  | Let of string * Expression * Expression

  // NOTE: Log is a helper for printing information about evaluation
  // When you have Log(msg, e), the evaluator should evaluate 'e', then print
  // 'msg' alongside with the result and return the result. It is equivalent
  // to writing something like:
  // 
  //   (let res = 1+2 in printfn "evaluated: %A"; res)
  //
  | Log of string * Expression

type VariableContext = 
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

  | If(pred, t, f) ->
      let p = evaluate ctx pred
      match p with
          | ValNum(0) -> evaluate ctx f
          | ValNum(_) -> evaluate ctx t
  
  | Log(msg, e) -> 
      let res = evaluate ctx e
      printfn "%s: %A" msg res
      res

  | Let(v, earg, ebody) ->
      let arg = evaluate ctx earg
      let nctx = Map.add v arg ctx
      evaluate nctx ebody

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Simple let: let x = 3*7 in x+x => 42
let eletx = 
  Let("x", Binary("*", Constant(3), Constant(7)),
    Binary("+", Variable("x"), Variable("x")))

evaluate Map.empty eletx |> printfn "eletx %A"

// Testing the 'log' function: + evaluates before *
let elog = 
  Log("evaluating *",
    Binary("*",
      Log("evaluating +", Binary("+", Constant(1), Constant(2))),
      Constant(10) ))

evaluate Map.empty elog |> printfn "elog %A"

// Conditional expression - should only evaluate one branch
let eif1 = 
  If(Constant(1), 
    Log("true branch", Constant(42)), 
    Log("false branch", Constant(0))
  )
evaluate Map.empty eif1 |> printfn "eif1 %A"  // Evaluates only true branch!

// Eager evaluation - this should print 'evaluating *' only once!
let elog1 = 
  Let("x", Log("evaluating *", Binary("*", Constant(3), Constant(7))),
    Binary("+", Variable("x"), Variable("x")))

evaluate Map.empty elog1 |> printfn "elog1 %A"

