#nowarn "40"
// ----------------------------------------------------------------------------
// 03 - Passing arguments to methods using activation records
// ----------------------------------------------------------------------------

// To support method arguments, we now replace the object (receiver) sent
// to the methods with an *activation record* - a temporary object that
// stores the receiver and also the arguments as slots. The definition
// is unchanged - but the input 'Objekt' in 'Code' will look different.

type Slot =
  { Name : string
    Value : Objekt }

and Objekt =
  { Slots : list<Slot>
    Special : option<Special> }

and Special =
  | String of string
  | Code of (Objekt -> Objekt)

// #load "objekt-visualizer.fs"

// ----------------------------------------------------------------------------
// Helper functions for constructing objects
// ----------------------------------------------------------------------------

let makeObject (slots : list<string * Objekt>) : Objekt =
  let slots = slots |> List.map (fun (k, v) -> { Name = k; Value = v })
  { Slots = slots; Special = None }

let makeMethod (f : Objekt -> Objekt) : Objekt =
  { Slots = []; Special = Some(Code f) }

let printString (obj : Objekt) : unit =
  match obj.Special with
  | Some(String s) -> printfn "%s" s
  | _ -> failwith "not a string"

// ----------------------------------------------------------------------------
// Prototype-based slot lookup
// ----------------------------------------------------------------------------

let getParents (obj : Objekt) : Objekt list =
  List.choose (fun s -> if s.Name.EndsWith("*") then Some s.Value else None) obj.Slots

let rec findSlots (name : string) (obj : Objekt) : Slot list =
  match List.tryFind (fun s -> s.Name = name) obj.Slots with
  | Some s -> [s]
  | _ -> List.collect (fun o -> findSlots name o) (getParents obj)

let send (name : string) (args : list<string * Objekt>) (obj : Objekt) : Objekt =
  // TODO: Add support for method arguments via an activation record.
  // When 'findSlots' finds a 'Code' with some function, you will need to:
  //
  // First, create an activation record using 'makeObject'. The slots of the
  // activation record should be all the arguments ('args') and an 
  // extra parent slot 'target*' that points to the instance ('obj').
  // This way, the method code can access both the arguments and the slots
  // of the receiver using ordinary 'send'!
  //   
  // Second, call 'f activation' and return the result. The remaining cases 
  // (plain value, not found, ambiguous) are the same as in step 2.
  match findSlots name obj with
  | [s]  -> 
      match s.Value.Special with // f obj
      | Some(Code f) -> f (makeObject (List.append ["target*", obj] args))
      | _ -> s.Value
  | [] -> failwith "missing slot"
  | _ -> failwith "multiple slots"

// ----------------------------------------------------------------------------
// Primitive string objects with a prototype carrying string methods
// ----------------------------------------------------------------------------

// All string values now inherit from string prototype that 
// defines useful functions for working with strings.

let rec stringPrototype : Objekt = makeObject [
  "append", makeMethod (fun activation ->
    // TODO: Implement string concatenation.
    // Access the receiver by sending 'target*' to 'activation'
    // Access the argument by sending 'other' to 'activation' 
    // Match both as Some(String s) and return 'makeString (s1 + s2)'
    //
    // NOTE: You can call 'ObjektVis.print' to visualize the activation record.
    // This way, you can see if you constructed it correctly!
    //
    let receiver = send "target*" [] activation
    let args = send "other" [] activation
    match receiver.Special, args.Special with
    | Some(String s1), Some(String s2) -> makeString (s1 + s2)
    | _ -> failwith "append error"
    )
]

and makeString (s : string) : Objekt =
  { Slots = [ { Name = "prototype*"; Value = stringPrototype } ]
    Special = Some(String s) }

// ----------------------------------------------------------------------------
// DEMO: Sending a method with an argument
// ----------------------------------------------------------------------------

// Each 'send "append"' call passes "other" as a named argument. Inside the method,
// the activation object has 'target*' pointing to the left-hand string and 'other'
// holding the right-hand string - both accessible via ordinary slot lookup.
let hello = makeString "Hello"
let space = makeString " "
let world = makeString "world"

hello
|> send "append" ["other", space]
|> send "append" ["other", world]
|> printString
