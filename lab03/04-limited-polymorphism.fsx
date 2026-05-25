// ============================================================================
// 04 - Adding limited polymorphism via unification
// ============================================================================

type Expression =
  | StringConst of string
  | NumberConst of int
  | Binary of string * Expression * Expression
  | Variable of string
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Type * Expression
  | Let of string * Expression * Expression
  | MakeTuple of Expression * Expression
  | GetTuple of bool * Expression

and Type =
  | Number
  | String
  | Function of Type * Type
  | Tuple of Type * Type
  // NOTE: We add 'TypeVariable' to represent an unknown type to be determined
  // by unification. For example, the identity function can be given type
  // Function(TypeVariable "a", TypeVariable "a"), meaning it works for any 'a'.
  | TypeVariable of string

type TypingContext = Map<string, Type>

// ----------------------------------------------------------------------------
// Unification
// ----------------------------------------------------------------------------


// TODO: Implement 'unify'. It takes a list of type constraints (pairs of
// types that must be equal) and tries to find an assignment of type variables
// to concrete types. It returns 'Some assignment' for success or 'None' for
// failure. Note that only the FIRST type in each pair may contain
// TypeVariables - the second is always a concrete type.
let rec unify (constraints:list<Type * Type>) : option<Map<_, _>> =
  match constraints with 
  // EXAMPLE: If the first constraint unifies number & number, 
  // we remove it and solve the remaining constraints...
  | (Number, Number)::constraints -> unify constraints
  | (String, String)::constraints -> unify constraints
  | (Function(ta1, ta2), Function(tb1, tb2))::constraints -> unify ((ta1, tb1)::((ta2, tb2)::constraints))
  | (Tuple(ta1, ta2), Tuple(tb1, tb2))::constraints -> unify ((ta1, tb1)::((ta2, tb2)::constraints))
  | (TypeVariable v, t)::constraints ->
      match unify constraints with
      | Some ctx ->
          match ctx.TryFind v with
          | Some t1 -> if t = t1 then Some(ctx) else None
          | None -> Some(Map.add v t ctx)
      | None -> None
  | [] -> Some Map.empty
  | _ -> None
  
  // TODO: Add handling for the following cases:
  // //  * (String, String) - types match, continue with the rest
  // //  * (Function(ta1,ta2), Function(tb1,tb2)) and similarly for tuples
  //    -> Add (ta1,tb1) and (ta2,tb2) to the remaining constraints
  //  * (TypeVariable v, t) - First solve the remaining constraints. 
  //    -> If that worked, add mapping from 'v' to 't' to the returned assignment
  //    -> If the assignment already contains type that's not 't', that is an error
  //  * [] - all constraints solved, return Some Map.empty
  //  * anything else - types are incompatible, return None


// Success: Some [] - Number matches Number, String matches String
unify [(Function(Number, String), Function(Number, String))] |> printfn "1s %A"

// Success: Some ["a", Function(Number, String)]
unify [(TypeVariable "a", Function(Number, String))] |> printfn "2s %A"

// Success: Some ["a", Number; "b", String]
unify [(Tuple(TypeVariable "a", TypeVariable "b"), Tuple(Number, String))] |> printfn "3s %A"

// None: String does not match Number
unify [(Function(String, Number), Function(Number, String))] |> printfn "4n %A"

// None: Function does not match Tuple
unify [(Function(Number, String), Tuple(Number, String))] |> printfn "5n %A"

// None: cannot assign two different types to 'a'
unify [(Tuple(TypeVariable "a", TypeVariable "a"), Tuple(Number, String))] |> printfn "6n %A"


// ----------------------------------------------------------------------------
// Substitution
// ----------------------------------------------------------------------------

// TODO: Implement 'substitute'. Given a substitution map and a type, replace
// every TypeVariable in the type with its assigned type from the map.
// Leave Number and String unchanged. Recurse into Function and Tuple.
// You can assume all TypeVariables in the type are present in the map.

let rec substitute (subst:Map<string, Type>) typ =
    match typ with
    | Number -> Number
    | String -> String
    | TypeVariable v ->
        match subst.TryFind v with
        | Some t -> t
        | None -> failwith ("variable not found: " + v)
    | Function(t1, t2) -> Function((substitute subst t1), (substitute subst t2))
    | Tuple(t1, t2) -> Tuple((substitute subst t1), (substitute subst t2))
    // | _ -> failwith "subst not implemented for this"

let ab = Map.ofList ["a", Number; "b", String]

// Substitute 'a' -> Number in TypeVariable "a" => Number
substitute ab (TypeVariable "a") |> printfn "7-number %A"

// Substitute 'a' -> Number in Function(TypeVariable "a", String) 
// => Function(Number, String)
substitute ab (Function(TypeVariable "a", String)) |> printfn "8-fun(num,str) %A"

// Substitute a and b in Tuple(TypeVariable "a", TypeVariable "b")
// => Tuple(Number, String)
substitute ab (Tuple(TypeVariable "a", TypeVariable "b")) |> printfn "9-tuple(num,str) %A"


// ----------------------------------------------------------------------------
// Type checker
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
  | Variable v ->
      if ctx.ContainsKey v then ctx[v] else failwith "var not found"
  | If(e1, e2, e3) ->
      match typeCheck ctx e1 with
      | Number ->
          let t1 = typeCheck ctx e2
          let t2 = typeCheck ctx e3
          if t1 = t2 then t1 else failwith "If doesnt have the same type"
      | _ -> failwith "If condition is not int"
  | Lambda(v, t, e) ->
      Function(t, typeCheck (Map.add v t ctx) e)
  | Let(v, e1, e2) -> typeCheck ctx (Lambda(v, typeCheck ctx e1, e2))
  | MakeTuple(e1, e2) -> Tuple(typeCheck ctx e1, typeCheck ctx e2)
  | GetTuple(b, e) ->
      match typeCheck ctx e with
      | Tuple(t1, t2) -> if b then t1 else t2
      | _ -> failwith "was expecting a tuple"

  | Application(e1, e2) ->
      match typeCheck ctx e1 with
      | Function(t1a, t2) ->
          match unify [(t1a, typeCheck ctx e2)] with
          | Some subst -> substitute subst t2
          | None -> failwith "type mismatch"
      | _ -> failwith "was expecting a function"
      // TODO: Implement function application with unification.
      //
      // Unlike in step 2, functions can now be polymorphic - e.g. an identity
      // function has type Function(TypeVariable "a", TypeVariable "a").
      // Note our unification is one-sided and can have type variables on the left.
      //
      // Steps:
      //  1. Type-check e1 and match the type against Function(t1a, t2). Fail otherwise.
      //  2. Type-check e2 to get the concrete argument type t1b.
      //  3. Call 'unify [(t1a, t1b)]' - this matches the (possibly polymorphic)
      //     expected argument type against the concrete actual argument type.
      //  4. If unification returns Some subst, call 'substitute subst t2' to
      //     instantiate any type variables in the return type. Return the result.
      //  5. If unification returns None, fail with a type mismatch error.


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Context with some useful polymorphic operations
let ops = Map.ofList [
  "id",   Function(TypeVariable "a", TypeVariable "a")
  "dup",  Function(TypeVariable "a", Tuple(TypeVariable "a", TypeVariable "a"))
  "swap", Function(Tuple(TypeVariable "a", TypeVariable "b"),
            Tuple(TypeVariable "b", TypeVariable "a")) ]

// id 42 => Number  (a is instantiated to Number)
let e0 = Application(Variable("id"), NumberConst(42))

typeCheck ops e0 |> printfn "\n10 %A"

// swap 42 => type error: 42 is a Number, not a Tuple
let e1 = Application(Variable("swap"), NumberConst(42))

// typeCheck ops e1 |> printfn "11 %A" // should fail

// dup "hello!" => Tuple(String, String), then get first element => String
let e2 = GetTuple(true, Application(Variable("dup"), StringConst "hello!"))

typeCheck ops e2 |> printfn "12 %A"

// fun (tup : Number * String) -> (swap tup)#1 => Function(Tuple(Number,String), String)
let e3 =
  Lambda("tup", Tuple(Number, String),
    GetTuple(true, Application(Variable("swap"), Variable "tup")))

typeCheck ops e3 |> printfn "13 %A"

// (fun (f : Number -> Number) -> f 10) (fun x -> x+1) => Number
let elf = Lambda("f", Function(Number, Number),
  Application(Variable "f", NumberConst 10))

let elf1 = Application(elf,
  Lambda("x", Number, Binary("+", Variable "x", NumberConst 1)))

typeCheck ops elf1 |> printfn "14 %A"

// (fun (f : Number -> Number) -> f 10) id
// In a more sophisticated system, we could do this! But because our 
// unification is one-sided (only allows type variables on the left), this
// currently fails. (It would have to unify 'Number -> Number' with 'a -> a' ).
let elf2 = Application(elf, Variable "id")

// typeCheck ops elf2 |> printfn "15 %A" // should fail

