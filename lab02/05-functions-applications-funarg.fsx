// ============================================================================
// 05 - Functions and application - but with the funarg problem!
// ============================================================================

type Value = 
  | ValNum of int 
  // NOTE: A function value such as '(fun x -> ...)' is a value - this means
  // that we can pass them around. We added 'ValClosure' here to represent
  // functions at runtime. The closure stores the variable name of the lambda
  // and the body of the lambda. (We will need to modify this later to support 
  // lexical scoping.)
  | ValClosure of string * Expression

// NOTE: 'ValClosure' above needs to refer to 'Expression'. 
// To make such recursive references, we define 
// the types using 'type .. and .. and' from now on!
and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Log of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added application 'e1 e2' and lambda 'fun v -> e'
  | Application of Expression * Expression
  | Lambda of string * Expression

// ============================================================================
// NOTE: We are going to use eager evaluation in our interpreter, 
// so you should continue from STEP 03 and ignore the changes from STEP 04
// ============================================================================

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
      ValClosure(v, e)
  | Application(e1, e2) ->
      let ee1 = evaluate ctx e1
      match ee1 with
        | ValClosure(name, lbody) -> evaluate ctx (Let(name, e2, lbody))
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

// Wrong function call (the first argument is not a function)
//   21 (fun x -> x * 2)
let ef3 = 
  Application(
    Constant(21),
    Lambda("x", Binary("*", Variable("x"), Constant(2)))
  )
// evaluate Map.empty ef3 |> printfn "ef3 %A\n" // should fail

// Wrong binary operator (it is now possible to apply '+'
// to functions; this makes no sense and should fail!)
//   21 + (fun x -> x * 2)
let ef4 = 
  Binary("+",
    Constant(21),
    Lambda("x", Binary("*", Variable("x"), Constant(2)))  
  )
// evaluate Map.empty ef4 |> printfn "ef4 %A\n" // should fail

// The FUNARG problem - our variables are dynamically scoped. When
// we call a function, we call it with current variables as they
// are in context when making the call. In the following, 'n' is
// available when the function 'f' is defined, but not when we call it!
// This means that the example crashes!
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

// evaluate Map.empty efunarg |> printfn "efunarg %A\n" // should fail

// On the other hand, the following works with dyanmic scoping!
// The variable 'n' is not defined when we create the lambda, but
// it is in scope when we call it - we get 42!
//
//   let f = (fun x -> n*x)
//   let n = 21
//   f 2
//
let edyn =
  Let("f", Lambda("x", Binary("*", Variable "n", Variable "x")),
    Let("n", Constant 21, 
      Application(Variable "f", Constant 2)))

evaluate Map.empty edyn |> printfn "edyn %A\n"

