// ----------------------------------------------------------------------------
// 05 - Class-based object system
// ----------------------------------------------------------------------------

// In class-based OO, objects are *instances* of classes. The class defines
// available methods; the instance stores its own state (instance variables).
//
// We use the same 'Objekt' and 'Slot' types, but with a specific representation:
// * Every object has a 'class*' slot pointing to its class. 
// * Every class has an optional 'super*' slot pointing to its superclass.
// * Every class also has 'name*' to store its name.
//
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

let makeObject slots =
  let slots = slots |> List.map (fun (k, v) -> { Name = k; Value = v })
  { Slots = slots; Special = None }

let makeMethod f =
  { Slots = []; Special = Some(Code f) }

// Unwraps the F# string from a string Objekt - useful for printing results.
let getStringValue obj =
  match obj with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string"

// ----------------------------------------------------------------------------
// Classes, instances, and strings
// ----------------------------------------------------------------------------

// 'Class' is the class of all class objects - every class is an instance of it.
// 'Str' is the class of string objects (we'll add methods later).
//
// NOTE: These should really be proper classes created using 'makeClass', but
// that creates an impossible recursive reference. We'll fix this later.
let Class : Objekt = makeObject []
let Str : Objekt = makeObject []

// Creates an instance of 'cls' with the given instance variable slots.
// The 'class*' slot links the instance to its class for method lookup.
let makeInstance cls slots =
  makeObject (("class*", cls) :: slots)

// Wraps an F# string as an Objekt belonging to the 'Str' class.
let makeString s =
  { Slots = [{ Name = "class*"; Value = Str }]
    Special = Some(String s) }

// Creates a class with no superclass. This is used to create root 'Object'.
let makeRootClass name methods =
  let methods = methods |> List.map (fun (m, f) -> m, makeMethod f)
  makeInstance Class (["name*", makeString name] @ methods)

// Creates a class that inherits from 'super'. 
let makeClass name super methods =
  let methods = methods |> List.map (fun (m, f) -> m, makeMethod f)
  makeInstance Class (["name*", makeString name; "super*", super] @ methods)

// 'Object' is the root of the class hierarchy - all classes inherit from it.
let Object = makeRootClass "Object" []

// ----------------------------------------------------------------------------
// Class-based method lookup
// ----------------------------------------------------------------------------

let tryFindSlot (name : string) (obj : Objekt) : Objekt option =
  // TODO: Search for a slot named 'name' directly on 'obj', with no inheritance
  // (this is data lookup). Return Some(value) if found, None otherwise.
  // (Hint: use List.tryPick on obj.Slots)
  List.tryPick (fun s -> if s.Name = name then Some s.Value else None) obj.Slots

let tryFindSuper (cls : Objekt) : Objekt option =
  // TODO: Return the superclass of 'cls', or None if it has no superclass.
  // The superclass is stored in the 'super*' slot.
  tryFindSlot "super*" cls

let findClass (obj : Objekt) : Objekt =
  // TODO: Return the class of 'obj'.
  // The class is stored in the 'class*' slot; fail if it is absent.
  match tryFindSlot "class*" obj with
  | Some o -> o
  | None -> failwith "not a class"

let rec lookupMethod (name : string) (cls : Objekt) : (Objekt -> Objekt) option =
  // TODO: Search for a method named 'name' starting at class 'cls'.
  // First look in 'cls' itself using 'tryFindSlot'. If a slot is found and its
  // value is a Code method, return Some f. If found but not a Code, fail.
  // If not found in 'cls', recurse into the superclass via 'tryFindSuper'.
  // Return None if the entire hierarchy is exhausted without finding it.
  match tryFindSlot name cls with
  | Some({ Special = Some s }) ->
      match s with
      | Code f -> Some f
      | _ -> failwith "not a method"
  | None ->
      match tryFindSuper cls with
      | Some sup -> lookupMethod name sup
      | None -> failwith "doesnt contain method"

let send (name : string) (obj : Objekt) : Objekt =
  // TODO: Send message 'name' to 'obj'.
  // First, find the class of 'obj', look up the method in the class hierarchy.
  // Then, call it with 'obj' as the receiver. Fail with "message not understood"
  // if no method is found (Again, we are starting with a version with no arguments.)
  match lookupMethod name (findClass obj) with
  | Some f -> f obj
  | None -> failwith "message not understood"

// ----------------------------------------------------------------------------
// DEMO: Class hierarchy with inherited and overridden methods
// ----------------------------------------------------------------------------

// 'Cat' defines two methods: 'pet' (inherited unchanged by all subclasses)
// and 'speak' (overridden by CheshireCat). 
let Cat : Objekt = makeClass "Cat" Object [
  "pet",   (fun _ -> makeString "Purr!")
  "speak", (fun _ -> makeString "Meow!")
]

// 'CheshireCat' overrides 'speak' but not 'pet'. 
let CheshireCat : Objekt = makeClass "CheshireCat" Cat [
  "speak", (fun _ -> makeString "We are all mad!")
]

let mog = makeInstance Cat []
let cheshire = makeInstance CheshireCat []

// ObjektVis.print mog
// ObjektVis.print cheshire

// TESTS: 'pet' is inherited by both; 'cheshire' speaks differently!
printfn "%A" (mog |> send "pet" |> getStringValue)  // "Purr!"
printfn "%A" (cheshire |> send "pet" |> getStringValue)  // "Purr!"

printfn "%A" (mog  |> send "speak" |> getStringValue)  // "Meow!"
printfn "%A" (cheshire |> send "speak" |> getStringValue)  // "We are all mad!"
