// ----------------------------------------------------------------------------
// 02 - Adding methods to prototype-based objects
// ----------------------------------------------------------------------------

// Objects can now contain methods. A method is an object that has 
// 'Special' set to 'Some(Code(fn))'. When sending a message to an object,
// we will look if the found slot is a method. If so, we invoke the function
// rather than just returning the value from the slot.

type Slot =
  { Name : string
    Value : Objekt }

and Objekt =
  { Slots : list<Slot>
    Special : option<Special> }

and Special =
  | String of string
  // NEW: A method is stored as a function from the receiver to result.
  // The function receives the object on which the message was sent ('self').
  | Code of (Objekt -> Objekt)

// #load "objekt-visualizer.fs"

// ----------------------------------------------------------------------------
// Helper functions for constructing objects
// ----------------------------------------------------------------------------

let makeObject (slots : list<string * Objekt>) : Objekt =
  let slots = slots |> List.map (fun (k, v) -> { Name = k; Value = v })
  { Slots = slots; Special = None }

// NEW: Wraps a function as an Objekt representing method. The function will 
// receive the receiver object when the method is invoked via 'send'.
let makeMethod (f : Objekt -> Objekt) : Objekt =
  { Slots = []; Special = Some(Code f) }

// Prints the string value of an object, failing if it is not a string.
// Unlike 'printValue' from earlier steps, this is used for string results
// where we know the type and want clean output without a label.
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

let send (name : string) (obj : Objekt) : Objekt =
  // TODO: Extend 'send' from step 1 to handle method invocation.
  // When 'findSlots' returns a slot whose value has Special = Some(Code f),
  // call 'f obj' - passing the receiver as the argument - and return the result.
  // The rest is the same as in step #1
  match findSlots name obj with
  | [s]  -> 
      match s.Value.Special with
      | Some(Code f) -> f obj
      | _ -> s.Value
  | [] -> failwith "missing slot"
  | _ -> failwith "multiple slots"

// ----------------------------------------------------------------------------
// Primitive string objects
// ----------------------------------------------------------------------------

let makeString (s : string) : Objekt =
  { Slots = []; Special = Some(String s) }

// ----------------------------------------------------------------------------
// TESTS: Methods, printable prototype & message sends
// ----------------------------------------------------------------------------

// A minimal object with a single method slot. Sending "greet" invokes the
// method and returns a string object - testing the basic send.
let greeter = makeObject [
  "greet", makeMethod (fun obj ->
    makeString "Hello world!"
  )
]
greeter |> send "greet"


// Methods that perform side effects (like printing) need to return something;
// 'empty' serves as a conventional "void" return value.
let empty : Objekt = makeObject []


// TODO: Define 'printable' - a prototype object with a 'print' method.
// The method should use 'send' to look up the 'name' slot on the receiver
// and pass the result to 'printString'. Return 'empty' as the result.
let printable : Objekt =
  makeObject [ "print", makeMethod (fun obj -> printString (send "name" obj); empty) ]


let alice = makeObject [
  "book", makeString "Alice in Wonderland"
]
let forgetful = makeObject [
  "book", makeString "Mog the Forgetful Cat"
]
let cat = makeObject [
  "sound", makeString "Meow"
]
let aristocrat = makeObject [
  "sound", makeString "Oh dear!"
]

// TODO: Make all the cats printable by adding the 'printable*' parent slot!
// This is an example of a mixin: 'printable' contributes behaviour orthogonal
// to the animal/fictional hierarchy, mixed in by adding another parent slot.
let cheshire = makeObject [
  "animal*", cat
  "fictional*", alice
  "printable*", printable
  "name", makeString "Cheshire cat"
  "sound", makeString "We are all mad!"
]
let mog = makeObject [
  "animal*", cat
  "fictional*", forgetful
  "printable*", printable
  "name", makeString "Mog"
]
let larry = makeObject [
  "animal*", cat
  "aristocrat*", aristocrat
  "printable*", printable
  "name", makeString "Larry"
]

// TESTS: Each cat should print its name via the inherited 'print' method.
cheshire |> send "print"
mog |> send "print"
larry |> send "print"
