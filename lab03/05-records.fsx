// ============================================================================
// 05 - Adding records with structural subtyping
// ============================================================================

// NOTE: Records are a new kind of value. A record type lists the field
// names and their types. Unlike tuples, fields are accessed by name.
//
// We also want a limited form of subtyping: a function expecting a record with
// field {age:Number} can accept any record that has at least that field
// (e.g. {name:String, age:Number} is fine). This is checked in the unification.

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
  // NOTE: MakeRecord constructs a record from a map of field names to
  // expressions; GetRecord accesses a named field of a record expression.
  | MakeRecord of Map<string, Expression>
  | GetRecord of Expression * string

and Type =
  | Number
  | String
  | Function of Type * Type
  | Tuple of Type * Type
  | TypeVariable of string
  // NOTE: Record type is a map from field names to their types. For example: 
  // {name:String, age:Number} is Record(Map.ofList ["name",String; "age",Number])
  | Record of Map<string, Type>

type TypingContext = Map<string, Type>

// ----------------------------------------------------------------------------
// Useful F# functions
// ----------------------------------------------------------------------------

// For checking record types, you may need functions for working with sets, e.g.:
Set.isSubset (set ["a";"b"]) (set ["a";"b";"c"])  // true
Set.isSubset (set ["a";"d"]) (set ["a";"b";"c"])  // false

// You can create a set from a collection of values, such as Keys of a map:
let m1 = Map.ofList ["x", Number; "y", String]
let s1 = set m1.Keys 

// When you have a map, you can transform its values using 'Map.map", e.g.:
Map.map (fun k v -> Tuple(v, v)) m1

// If you need to turn any collection to a list, you can use 'List.ofSeq':
List.ofSeq s1
List.ofSeq m1.Keys

// You can concatenate two lists using the '@' operator:
let l1 = [1;2;3]
let l2 = [4;5;6]
let l = l1 @ l2

// You can iterate over lists using 'List.map', e.g.: this fetches the 
// value for each key, which is useless, but shows the syntax!
List.map (fun k -> m1[k]) (List.ofSeq m1.Keys)

// ----------------------------------------------------------------------------
// Unification
// ----------------------------------------------------------------------------

// NOTE: Extend 'unify' with a case for records. This adds limited subtyping, i.e.:
// (Record r1, Record r2) can be unified when r2 has at least all
// the fields that r1 requires (r1's keys are a subset of r2's keys). 
// Fields present in r1 must have types that unify pairwise.

let rec unify constraints : option<Map<_, _>> =
  match constraints with
  | (Record r1, Record r2) :: constraints ->
      if Set.isSubset (set r1.Keys) (set r2.Keys) then
          unify ((List.map (fun k -> (r1[k], r2[k])) (List.ofSeq r1.Keys)) @ constraints)
      else None
      // TODO: Check that every field name in r1 also appears in r2 using
      // Set.isSubset and on Keys. If not, return None.
      //
      // Otherwise build a list of pairwise field-type constraints for each
      // key in r1: (r1[k], r2[k]). Append these to 'constraints' (use @)
      // and call unify on the combined list.
      //
      // (Hint: use List.ofSeq r1.Keys to iterate over the keys of r1.)
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

// Success: r2 has all fields of r1 (and more) - Some ["a", Number]
unify [Record(Map.ofList ["thing", TypeVariable "a"]),
       Record(Map.ofList ["thing", Number; "more", String])] |> printfn "a-some %A"

// None: r1 requires "more" but r2 does not have it
unify [Record(Map.ofList ["thing", Number; "more", String]),
       Record(Map.ofList ["thing", Number])] |> printfn "b-none %A"


// ----------------------------------------------------------------------------
// Substitution
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Type>) typ =
  match typ with
  | Record r -> Record (Map.map (fun key v -> (substitute subst v)) r)
      // TODO: Apply substitute to every field type in r using Map.map.
      // (Hint: Map.map takes a function (key -> value -> result))
      
    | Number -> Number
    | String -> String
    | TypeVariable v ->
        match subst.TryFind v with
        | Some t -> t
        | None -> failwith ("variable not found: " + v)
    | Function(t1, t2) -> Function((substitute subst t1), (substitute subst t2))
    | Tuple(t1, t2) -> Tuple((substitute subst t1), (substitute subst t2))

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

  | MakeRecord(fields) -> Record (Map.map (fun k v -> typeCheck ctx v) fields)
      // TODO: Type-check every field expression and collect the results into
      // a map of field types. Return Record of that map.
  | GetRecord(e, field) ->
      match typeCheck ctx e with
      | Record(f) -> 
          match f.TryFind field with
          | Some(t) -> t
          | None -> failwith "field is not inside"
      | _ -> failwith "must be a record"
      // TODO: Type-check e - it must be a Record type. Check that 'field'
      // is present in the record's field map (use .ContainsKey). Return the
      // type of that field. Fail if it is not there.


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Success: {name:"Yoda", age:700}.age + 1 => Number
let e1 =
  Binary("+",
    GetRecord(
      MakeRecord(Map.ofList ["name", StringConst "Yoda"; "age", NumberConst 700]),
      "age"),
    NumberConst 1)

typeCheck Map.empty e1 |> printfn "1 %A"

// Fail: {name:"Yoda"}.age => type error: field "age" does not exist
let e2 =
  GetRecord(
    MakeRecord(Map.ofList ["name", StringConst "Yoda"]),
    "age")

// typeCheck Map.empty e2 |> printfn "2 %A" // should fail

// Success: (fun (x:{age:Number}) -> x.age + 1) {name:"Yoda", age:700} => Number
// The function requires only 'age'; the argument has 'name' too - that is fine.
let e3 =
  Application(
    Lambda("x", Record(Map.ofList ["age", Number]),
      Binary("+",
        GetRecord(Variable "x", "age"),
        NumberConst 1)),
    MakeRecord(Map.ofList ["name", StringConst "Yoda"; "age", NumberConst 700]))

typeCheck Map.empty e3 |> printfn "3 %A"

// let r = (fun (x:{age:Number}) -> x) {name:"Yoda", age:700}
// in r.name
//
// Fail: This fails even though the argument had a "name" field! The lambda's
// return type is Record({"age":Number}) - it only promises to return a record
// with "age". The extra "name" field is forgotten. 
let e4 =
  Let("r",
    Application(
      Lambda("x", Record(Map.ofList ["age", Number]),
        Variable "x"),
      MakeRecord(Map.ofList ["name", StringConst "Yoda"; "age", NumberConst 700])),
    GetRecord(Variable "r", "name"))

typeCheck Map.empty e4 |> printfn "4 %A" // should fail

