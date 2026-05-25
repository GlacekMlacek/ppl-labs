// ============================================================================
// STEP #4 (BONUS) - Representing numbers in lambda calculus
// ============================================================================

type Term = 
  | Variable of string
  | Lambda of string * Term
  | Application of Term * Term


// ============================================================================
// Functions implemented in previous step
// ============================================================================

let rec format term = 
  match term with 
  | Lambda(x, t) -> $"(\\{x}.{format t})"
  | Application(t1, t2) -> $"({format t1} {format t2})"
  | Variable x -> x

let tryFormat optTerm = 
  match optTerm with 
  | Some term -> format term
  | None -> ""
  
let rec substitute (var:string) (subst:Term) (term:Term) : Term = 
  match term with
  | Variable(v) -> if v = var then subst else term
  | Lambda(v, t) -> if v = var then term else Lambda(v, substitute var subst t)
  | Application(t1, t2) -> Application(substitute var subst t1, substitute var subst t2)

// ============================================================================
// Reduction strategies implemented in previous steps
// ============================================================================

let reduceRedexCBN (term:Term) : option<Term> = 
  match term with
  | Application(Lambda(v, lbody), t) -> Some(substitute v t lbody)
  | _ -> None

let rec reduceCBN (term:Term) : option<Term> = 
  match reduceRedexCBN term with 
  | Some reduced -> Some reduced
  | None -> 
      match term with
      | Application(t1, t2) -> 
          match reduceCBN t1 with
          | Some rt1 -> Some(Application(rt1, t2))
          | None ->
            match reduceCBN t2 with
            | Some rt2 -> Some(Application(t1, rt2))
            | None -> None
      | _ -> None

let rec reduceAllCBN term = 
  match reduceCBN term with 
  | Some term -> reduceAllCBN term
  | None -> term

let rec reduceCBV (term:Term) : option<Term> = 
  match term with
  | Application(Lambda(v, lbody), t) -> 
      match reduceCBV t with
      | Some rt -> Some(Application(Lambda(v, lbody), rt))
      | None -> Some(substitute v t lbody)
  | Application(t1, t2) ->
      match reduceCBV t1 with
      | Some rt1 -> Some(Application(rt1, t2))
      | None ->
          match reduceCBV t2 with
          | Some rt2 -> Some(Application(t1, rt2))
          | None -> None
  | _ -> None

let rec reduceAllCBV term = 
  match reduceCBV term with
  | Some term -> reduceAllCBV term
  | None -> term

// ============================================================================
// You can represent numbers and arithmetic in lambda calculus!
// ============================================================================

// Church numerals encode numbers as lambdas that apply a given function
// to an argument repeatedly - n-times application represents number 'n'

let rec ntimesf n = 
  if n = 0 then Variable "x"
  else Application(Variable "f", ntimesf (n-1))

let num n = Lambda("f", Lambda("x", ntimesf n))

format (num 0) = "(\\f.(\\x.x))" |> printfn "%A" 
format (num 1) = "(\\f.(\\x.(f x)))" |> printfn "%A" 
format (num 2) = "(\\f.(\\x.(f (f x))))" |> printfn "%A" 

// We can apply Church numeral to variables 's' and 'z' to get a more readable output
let normalize n = 
  let t = Application(Application(n, 
    Variable "s"), Variable "z")
  reduceAllCBN t

format (normalize (num 0)) = "z" |> printfn "%A" 
format (normalize (num 1)) = "(s z)" |> printfn "%A" 
format (normalize (num 2)) = "(s (s z))" |> printfn "%A" 


// TASK #1: Can you implement the 'successor' function? See the definition:
// https://en.wikipedia.org/wiki/Church_encoding#Calculation_with_Church_numerals

let succ = Lambda("n", Lambda("f", Lambda("x", Application(Variable "f", Application(Application(Variable "n", Variable "f"), Variable "x")))))



// TEST: The successor of the successor of 0 should be 2!

let t2a = Application(succ, Application(succ, num 0)) 
let t2b = num 2

format (normalize t2a) = "(s (s z))" |> printfn "%A" 
format (normalize t2b) = "(s (s z))" |> printfn "%A" 

