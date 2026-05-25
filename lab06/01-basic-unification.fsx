// ----------------------------------------------------------------------------
// 01 - Implementing basic unication of terms
// ----------------------------------------------------------------------------

// A term is a recursive type, which can be either an atom (a known fact 
// like 'socrates'), a variable (to which we want to asign a term by 
// unification), or a predicate (with predicate name and a list of arguments). 
type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

// A clause is for example 'mortal(X) :- human(X)' or 'human(socrates)'. It
// consists of a head and body (head :- body). Body is a sequence of terms.
type Clause =
  { Head : Term
    Body : Term list }

// A program is just a list of clauses
type Program = Clause list

// A substitution assigns terms to variable names
type Substitution = Map<string, Term>

// Create a clause that states a fact
let fact p = { Head = p; Body = [] }

// Create a clause that defines a rule  
let rule p b = { Head = p; Body = b }

// Helper function (implemented for you!) - this appends two substitutions
// by iterating over items from 'sub1' and adding them to 'sub2'
let appendSubstitutions sub1 sub2 = 
  Map.fold (fun sub2 key value -> Map.add key value sub2) sub1 sub2

// ----------------------------------------------------------------------------
// Unification of terms and lists
// ----------------------------------------------------------------------------

let rec unifyLists l1 l2 : option<Substitution> = 
  match l1, l2 with 
  | [], [] -> 
      // TODO: Succeeds, but returns an empty substitution
      Some(Substitution [])
  | h1::t1, h2::t2 -> 
      // TODO: Unify 'h1' with 'h2' using 'unify' and
      // 't1' with 't2' using 'unifyLists'. If both 
      // succeed, return the generated joint substitution!
      // (For now, you can use the above 'appendSubstitutions' helper)
      match unify h1 h2, unifyLists t1 t2 with
      | Some h, Some t -> Some(appendSubstitutions h t)
      | _ -> None
  | _ -> None
    // TODO: Lists cannot be unified 

and unify t1 t2 : option<Substitution> = 
  match t1, t2 with
  | Atom x, Atom y -> if x = y then Some Map.empty else None
  | Predicate(x, ts1), Predicate(y, ts2) -> if x = y then unifyLists ts1 ts2 else None
  | Variable x, t
  | t, Variable x -> Some(Map.ofList [(x, t)])
  | _ -> None
      // TODO: Add all the necessary cases here!
      // * For matching atoms, return empty substitution (Map.empty)
      // * For matching predicates, return the result of 'unifyLists'
      // * For variable and any term, return a new substitution (Map.ofList)
      // * For anything else, return None (failed to unify) 

// ----------------------------------------------------------------------------
// Basic unification tests 
// ----------------------------------------------------------------------------

// Example: human(socrates) ~ human(X) 
// Returns: [X -> socrates]
unify
  (Predicate("human", [Atom("socrates")]))
  (Predicate("human", [Variable("X")])) |> printfn "x->socr 1: %A"

// Example: human(odysseus) ~ human(penelope) 
// Returns: None (fail)
unify
  (Predicate("human", [Atom("odysseus")]))
  (Predicate("human", [Atom("penelope")]))  |> printfn "fail 2: %A"

// Example: human(socrates) ~ mortal(X) 
// Returns: None (fail)
unify
  (Predicate("human", [Atom("socrates")]))
  (Predicate("mortal", [Variable("X")])) |> printfn "fail 3: %A"

// Example: parent(charles, harry) ~ parent(charles, X)
// Returns: [X -> harry]
unify
  (Predicate("parent", [Atom("charles"); Atom("harry")]))
  (Predicate("parent", [Atom("charles"); Variable("X")])) |> printfn "x->harry 4: %A"

// Example: parent(X, harry) ~ parent(charles, Y)
// Returns: [X -> charles; Y -> harry]
unify
  (Predicate("parent", [Variable("X"); Atom("harry")]))
  (Predicate("parent", [Atom("charles"); Variable("Y")])) |> printfn "x->charles,y->harry 5: %A"

// Example: succ(succ(succ(zero))) ~ succ(X)
// Returns: [X -> succ(succ(zero))]
unify
  (Predicate("succ", [Predicate("succ", [Predicate("succ", [Atom("zero")])])]))
  (Predicate("succ", [Variable("X")])) |> printfn "x->suc(suc(zero)) 6: %A"

// Example: succ(succ(zero)) ~ succ(zero)
// Returns: None (fail)
unify
  (Predicate("succ", [Predicate("succ", [Atom("zero")])]))
  (Predicate("succ", [Atom("zero")])) |> printfn "fail 7: %A"

