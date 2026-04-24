// ----------------------------------------------------------------------------
// 01 - Prototype-based object-oriented system with slots
// ----------------------------------------------------------------------------

// In prototype-based OO, everything is an object. An object is a collection
// of named slots that reference other objects. In the first step, we have
// ordinary objects and strings (we will add code later):
//
// * Ordinary object has slots that store other values
// * String value stores the string using the 'Special' field
//
// Objects can inherit from multiple other objects. This is done simply
// by using a special name - if the slot name ends with "*", it is a parent!
type Slot =
  { Name : string
    Value : Objekt }

and Objekt =
  { Slots : list<Slot>
    Special : option<Special> }

and Special =
  | String of string

#load "objekt-visualizer.fs"

// ----------------------------------------------------------------------------
// Helper functions for constructing objects
// ----------------------------------------------------------------------------

// Convenience constructor - takes (name, value) pairs and wraps them as Slots.
let makeObject (slots : list<string * Objekt>) : Objekt =
  let slots = slots |> List.map (fun (k, v) -> { Name = k; Value = v })
  { Slots = slots; Special = None }

// ----------------------------------------------------------------------------
// Prototype-based slot lookup
// ----------------------------------------------------------------------------

let getParents (obj : Objekt) : Objekt list =
  // TODO: Return the values of all parent slots.
  // Parent slots are identified by their name ending with '*'.
  // Use List.choose to filter and unwrap in one step or List.filter & List.map
  failwith "todo"

let rec findSlots (name : string) (obj : Objekt) : Slot list =
  // TODO: Search for a slot by name. If the slot is not found directly
  // in the object, follow the prototype chain using getParents.
  // Returns a list because the same name might be reachable via multiple
  // parent paths; the caller treats multiple results as an error.
  failwith "todo"

let send (name : string) (obj : Objekt) : Objekt =
  // TODO: Look up a slot by name and return its value.
  // Exactly one result is expected: zero means the slot is missing,
  // more than one means the name is ambiguous across parent paths.
  failwith "todo"

// ----------------------------------------------------------------------------
// Primitive string objects
// ----------------------------------------------------------------------------

// Helper that turns F# string into an Objekt representing strings
let makeString (s : string) : Objekt =
  { Slots = []; Special = Some(String s) }

// ----------------------------------------------------------------------------
// DEMO: Prototype objects and multiple inheritance
// ----------------------------------------------------------------------------

// Fictional characters will inherit from these objects 
// that define what book the character comes from.
let alice = makeObject [
  "book", makeString "Alice in Wonderland"
]
let forgetful = makeObject [
  "book", makeString "Mog the Forgetful Cat"
]

// These two parent objects define two parents that make some sound.
// Cats "Meow", while aristocrats say "Oh dear!"
let cat = makeObject [
  "sound", makeString "Meow"
]
let aristocrat = makeObject [
  "sound", makeString "Oh dear!"
]


// Mog is a fictional character (inherits book) and a cat (inherits sound)
let mog = makeObject [
  "animal*", cat
  "fictional*", forgetful
  "name", makeString "Mog"
]
// Larry is an aristocrat (inherits sound) and also cat (inherits sound again)
let larry = makeObject [
  "animal*", cat
  "aristocrat*", aristocrat
  "name", makeString "Larry"
]
// Cheshire cat is a cat and fictional character; it defines its own sound.
let cheshire = makeObject [
  "animal*", cat
  "fictional*", alice
  "name", makeString "Cheshire cat"
  "sound", makeString "We are all mad!"
]

// You can visualize the objects using console!
ObjektVis.print larry
ObjektVis.print cheshire
ObjektVis.print mog


// TESTS: Testing 'findSlots' function

cheshire |> findSlots "sound" // We are all mad!
mog |> findSlots "sound"      // Meow
larry |> findSlots "sound"    // !! Ambiguous !!

cheshire |> findSlots "book" // Alice in Wonderland
mog |> findSlots "book"      // Mog the Forgetful Cat
larry |> findSlots "book"    // !! Empty list !!


// TESTS: Testing message sends

// The following are all fine
cheshire |> send "sound"
cheshire |> send "book"
mog |> send "sound" 
mog |> send "book"

// Error - ambiguous slots (sound is found via both animal* and aristocrat*)
larry |> send "sound"
// Error - no slot found (larry has no fictional* parent)
larry |> send "book"
