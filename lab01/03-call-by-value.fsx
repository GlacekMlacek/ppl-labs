// ============================================================================
// STEP #3 - Implementing the call-by-value reduction strategy
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
// Call-by-name reduction
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

// ============================================================================
// Call-by-value reduction
// ============================================================================

// In call-by-value reduction, we want to reduce everything in the argument
// of the redex before doing the substitution. This means that if we have
// 
// (\x.x) ((\y.y) z)
//
// The argument first reduces to 'z' and then the whole term reduces to 'z'.
// (In call-by-name, the term first reduces to '((\y.y) z)' and then to 'z'.)

// TASK #1: Implement call-by-value reduction
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
  // * If the term is 'Application(Lambda(v, t1), t2)' then
  //   first try reducing the argument 't2' recursively
  //   - If this succeeds, return the reduced Application(Lambda(v, t1), t2reduced)
  //   - If this does not succeed, t2 is a value and we can do substitution
  // * If the term is 'Application(t1, t2)', try reducing 
  //   't1' and then 't2' recursively (in the same way as in CBN)
  // * If the term is anything else then it cannot be reduced


// TASK #2: Run implement recursive reduction 
// (this is the same as reduceAllCBN)

let rec reduceAllCBV term = 
    match reduceCBV term with
    | Some term -> reduceAllCBV term
    | None -> term


// TESTS: The following are copied from step2. The 
// two reduction strategies behave the same for them!

let tcbn1 = 
  Application(Application(Lambda("x", Lambda("y", 
    Variable("x"))), Variable("z1")), Variable("z2"))
let tcbn2 = 
  Application(Application(Lambda("x", Lambda("y", 
    Variable("y"))), Variable("z1")), Variable("z2"))

// TEST: Reduce redex that is nested in a term: (((\x.(\y.x)) z1) z2) ~~> ((\y.z1) z2)
tryFormat (reduceCBV tcbn1) = "((\\y.z1) z2)" |> printfn "tcbn1 %A"
// TEST: Reduce redex that is nested in a term: (((\x.(\y.y)) z1) z2) ~~> ((\y.y) z2)
tryFormat (reduceCBV tcbn2) = "((\\y.y) z2)" |> printfn "tcbn2 %A"


// TEST: More interesting case where CBN and CBN differ: (\x.x) ((\y.y) z)
let tx = Lambda("x", Variable("x"))
let ty = Lambda("y", Variable("y"))
let t1 = Application(tx, Application(ty, Variable("z")))

// The two strategies proceed in different ways
tryFormat (reduceCBV t1) = "((\\x.x) z)" |> printfn "t1cbv %A"
tryFormat (reduceCBN t1) = "((\\y.y) z)" |> printfn "t1cbn %A"

// But if we reduce them fully, the result is the same
format (reduceAllCBN t1) = "z" |> printfn "t1cbvall %A"
format (reduceAllCBV t1) = "z" |> printfn "t1cbnall %A"

// ============================================================================
// Bonus demo - difference between CBN and CBV
// ============================================================================

// Recall that we previously created a term 
// '(\x.xx) (\x.xx)' which can be reduced and reduces to itself!

let txx = Lambda("x",Application(Variable "x", Variable "x"))
let tinf = Application(txx, txx)

format tinf = "((\\x.(x x)) (\\x.(x x)))"
// tryFormat (reduceCBN tinf) = format tinf |> printfn "tinfCBN %A"
// tryFormat (reduceCBV tinf) = format tinf |> printfn "tinfCBV %A"

// If you try calling 'reduceAllCBN' or 'reduceAllCBV' on 'tinf'
// you get an infinite loop (in both cases)

// But what if we use 'tinf' as an argument to a lambda 
// function that does not use its argument? 
let tnop = Application(Lambda("x", Variable("z")), tinf)

// CBN will do the substitution first and so we get 'z'
reduceAllCBN tnop |> printfn "tnopCBNall %A"

// But CBV will run into an infinite loop!
// reduceAllCBV tnop |> printfn "tnopCBVall %A"
