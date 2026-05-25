// ============================================================================
// STEP #2 - Implementing the call-by-name reduction strategy
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

// Helper function (done for you) which formats values 'option<Term>'
// (format a term, but returns empty string if the argument is 'None')
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

// In all our evaluation strategies, we are looking for a redex in the form
// '(\x. t1) t2' - we have an application where the first term is a lambda.

// First, we will implement call-by-name reduction. This is a strategy where
// we do not evaluate the argument before reducting the redex - so we replace
// '(\x. t1) t2' with 't1' where we substitute 't2' for 'x'.

// TASK #1: Implement a function that tests if a term is a redex (application 
// of a lambda) - if so, it returns 'Some' with the reduction result; if 
// the term is anything else, it returns 'None'
let reduceRedexCBN (term:Term) : option<Term> = 
  match term with
  | Application(Lambda(v, lbody), t) -> Some(substitute v t lbody)
  | _ -> None


// TEST - this reduces: (\x.x) z ~> z
let tr1 = Application(Lambda("x", Variable("x")), Variable("z")) 
tryFormat (reduceRedexCBN tr1) = "z" |> printfn "%A"

// TEST - this reduces: (\x.x x) z ~> z z
let tr2 = Application(Lambda("x", Application(Variable("x"), Variable("x"))), Variable("z")) 
tryFormat (reduceRedexCBN tr2) = "(z z)" |> printfn "%A"

// TEST - other things do not reduce
tryFormat (reduceRedexCBN (Variable("x"))) = "" |> printfn "%A"
tryFormat (reduceRedexCBN (Application(Variable("x"), Variable("z")))) = "" |> printfn "%A"

// TEST - fun fact! (\x.x x) (\x.x x) reduces to itself!
let trl1 = Lambda("x", Application(Variable("x"), Variable("x")))
let trl2 = Application(trl1, trl1)

tryFormat (reduceRedexCBN trl2) = "((\\x.(x x)) (\\x.(x x)))" |> printfn "%A"
tryFormat (reduceRedexCBN trl2) = format trl2 |> printfn "%A"

printfn ""

// TASK #2: Now we want to implement a function that applies 'reduceRedexCBN'
// to the term, but also to sub-terms that appear inside other applications.
// So if we have a redex '(\x.\y.x) z' inside some bigger term - for example
// '((\x.\y.x) z) z' the nested term gets reduced first!

let rec reduceCBN (term:Term) : option<Term> = 
  // If the 'term' is a redex and can be reduced, we reduce and return it!
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
      // If the term is 'Variable' or 'Lambda' then we cannot do anything and just return 'None'
      // If the term is 'Application(t1, t2)' then we try to apply 'reduce' to 
      // the two sub-terms - t1 and t2. 
      //
      // * You need to call 'reduce t1' and pattern match on the result 
      //   - if this is 'Some(t1reduced)' then return 'Some(Application(t1reduced, t2))'
      // * Otherwise, if 'reduce t1' returns None (term t1 cannot be reduced) try reducing t2:
      //   - if this is 'Some(t2reduced)' then return 'Some(Application(t1, t2reduced))'


let tcbn1 = 
  Application(Application(Lambda("x", Lambda("y", 
    Variable("x"))), Variable("z1")), Variable("z2"))
let tcbn2 = 
  Application(Application(Lambda("x", Lambda("y", 
    Variable("y"))), Variable("z1")), Variable("z2"))

// TEST: Reduce redex that is nested in a term: (((\x.(\y.x)) z1) z2) ~~> ((\y.z1) z2)
tryFormat (reduceCBN tcbn1) = "((\\y.z1) z2)" |> printfn "%A"
// TEST: Reduce redex that is nested in a term: (((\x.(\y.y)) z1) z2) ~~> ((\y.y) z2)
tryFormat (reduceCBN tcbn2) = "((\\y.y) z2)" |> printfn "%A"


// DEMO: A helper function that runs 'reduceCBN' recursively
// as long as it can find and reduce some redex in the term 
let rec reduceAllCBN term = 
  match reduceCBN term with 
  | Some term -> reduceAllCBN term
  | None -> term

// TEST: Fully reduce a term: (((\x.(\y.x)) z1) z2) ~~>* z1
format (reduceAllCBN tcbn1) = "z1" |> printfn "%A"
// TEST: Fully reduce a term: (((\x.(\y.y)) z1) z2) ~~>* z2
format (reduceAllCBN tcbn2) = "z2" |> printfn "%A"

