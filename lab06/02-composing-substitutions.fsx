// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list
type Substitution = Map<string, Term>

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

let appendSubstitutions sub1 sub2 = 
  Map.fold (fun sub2 key value -> Map.add key value sub2) sub1 sub2

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Substitution) term : Term = 
  match term with
  | Atom _ -> term
  | Variable v -> //subst.[v]
    match Map.tryFind v subst with
    | Some t -> t
    | None -> term
  | Predicate(s, tl) -> Predicate(s, List.map (fun t -> substitute subst t) tl)
  // TODO: Replace variables in 'term' for which there is a
  // replacement specified by 'subst.[var]' with the replacement.
  // You can assume the terms in 'subst' do not contain
  // any of the variables that we want to replace.


let substituteSubst (newSubst:Substitution) (subst:Substitution) = 
  // TODO: Apply the substitution 'newSubst' to all the terms 
  // in the existing substitiution 'subst' (Hint: use Map.map).
  Map.map (fun _ t -> substitute newSubst t) subst


let substituteTerms (subst:Substitution) (terms:list<Term>) = 
  // TODO: Apply substitution 'subst' to all the terms in 'terms'
  List.map (fun t -> substitute subst t) terms


let rec unifyLists l1 l2 = 
  // TODO: Modify the implementation to use 'substituteTerms' and 'substituteSubst'.
  //
  // Let's say that your code calls 'unify h1 h2' to get a substitution 's1'
  // and then it calls 'unifyLists t1 t2' to get a substitution 's2' and then it
  // returns a concatentated list 'appendSubstitutions s1 s2'. Modify the code so that:
  //
  // (1) The substitution 's1' is aplied to 't1' and 't2' before calling 'unifyLists'
  // (2) The substitution 's2' is applied to all terms in substitution 's1' before returning
  match l1, l2 with 
  | [], [] -> 
      Some Map.empty
  | h1::t1, h2::t2 ->
      match unify h1 h2 with
      | Some s1 ->
          let st1 = substituteTerms s1 t1
          let st2 = substituteTerms s1 t2
          match unifyLists st1 st2 with
          | Some s2 -> Some(substituteSubst s2 s1)
          | None -> None
      | _ -> None
  | _ -> None

and unify t1 t2 = 
  match t1, t2 with
  | Atom x, Atom y -> if x = y then Some Map.empty else None
  | Predicate(x, ts1), Predicate(y, ts2) -> if x = y then unifyLists ts1 ts2 else None
  | Variable x, t
  | t, Variable x -> Some(Map.ofList [(x, t)])
  | _ -> None

// ----------------------------------------------------------------------------
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
unify
  (Predicate("loves", [Atom("narcissus"); Atom("narcissus")]))
  (Predicate("loves", [Variable("X"); Variable("X")])) |> printfn "x->narc: %A"

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
unify
  (Predicate("loves", [Atom("odysseus"); Atom("penelope")]))
  (Predicate("loves", [Variable("X"); Variable("X")])) |> printfn "none: %A"

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
unify
  (Predicate("add", [Atom("zero"); Predicate("succ", [Atom("zero")])]))
  (Predicate("add", [Variable("Y"); Predicate("succ", [Variable("Y")])])) |> printfn "y->zero: %A"

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
unify
  (Predicate("loves", [Variable("X"); Atom("narcissus")]))
  (Predicate("loves", [Variable("Y"); Variable("X")])) |> printfn "x->narc,y->narc: %A"

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
unify
  (Predicate("add", 
      [ Predicate("succ", [Variable("X")]); 
        Variable("X") ]))
  (Predicate("add", 
      [ Variable("Y"); 
        Predicate("succ", [Variable("Z")]) ])) |> printfn "x->suc(Z),y->suc(suc(Z)): %A"

