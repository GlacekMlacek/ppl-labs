// ----------------------------------------------------------------------------
// 06 - BONUS - Adding method arguments and minimal reflection
// ----------------------------------------------------------------------------

// This step extends the class-based system to support:
//
// * Method arguments: This time, we pass arguments as an extra object
// * Reflection: objects can look at their class and find its name

type Slot =
  { Name : string
    Value : Objekt }

and Objekt =
  { // NOTE: To workaround the recursive references, we now make Slots
    // mutable. This allows us to create the object and populate it later.
    mutable Slots : list<Slot>
    Special : option<Special> }

and Special =
  | String of string
  // NEW: Methods now take two Objekt arguments - the receiver ('inst')
  // and an argument list object ('args') - instead of just the receiver.
  | Code of (Objekt -> Objekt -> Objekt)

// #load "objekt-visualizer.fs"

// ----------------------------------------------------------------------------
// Helper functions for constructing objects
// ----------------------------------------------------------------------------

let makeObject (slots : list<string * Objekt>) : Objekt =
  let slots = slots |> List.map (fun (k, v) -> { Name = k; Value = v })
  { Slots = slots; Special = None }

let makeMethod (f : Objekt -> Objekt -> Objekt) : Objekt =
  { Slots = []; Special = Some(Code f) }

let getStringValue (obj : Objekt) : string =
  match obj with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string"

// ----------------------------------------------------------------------------
// Classes, instances, and strings
// ----------------------------------------------------------------------------

// 'Class' and 'Str' start as empty objects; 'makeClass' needs 'Class' to 
// already exist, so we create placeholders and fill in their slots later
let Class = makeObject []
let Str = makeObject []

let makeInstance cls slots =
  makeObject (("class*", cls) :: slots)

let makeString s =
  { Slots = [{ Name = "class*"; Value = Str }]
    Special = Some(String s) }

let makeRootClass name methods =
  let methods = methods |> List.map (fun (m, f) -> m, makeMethod f)
  makeInstance Class (["name*", makeString name] @ methods)

let makeClass name super methods =
  let methods = methods |> List.map (fun (m, f) -> m, makeMethod f)
  makeInstance Class (["name*", makeString name; "super*", super] @ methods)

// ----------------------------------------------------------------------------
// Class-based method lookup  (copy from step 5)
// ----------------------------------------------------------------------------

let tryFindSlot (name : string) (obj : Objekt) : Objekt option =
  List.tryPick (fun s -> if s.Name = name then Some s.Value else None) obj.Slots

let tryFindSuper (cls : Objekt) : Objekt option =
  tryFindSlot "super*" cls

let findClass (obj : Objekt) : Objekt =
  match tryFindSlot "class*" obj with
  | Some o -> o
  | None -> failwith "not a class"

let rec lookupMethod (name : string) (cls : Objekt) : (Objekt -> Objekt -> Objekt) option =
  match tryFindSlot name cls with
  | Some({ Special = Some s }) ->
      match s with
      | Code f -> Some f
      | _ -> failwith "not a method"
  | None ->
      match tryFindSuper cls with
      | Some sup -> lookupMethod name sup
      | None -> failwith "doesnt contain method"

// ----------------------------------------------------------------------------
// Argument lists and send
// ----------------------------------------------------------------------------

// 'ArgsList' is the base class for all argument list objects. Each call to
// 'send' with named arguments dynamically creates a subclass of 'ArgsList'
// with one getter method per argument, then instantiates it.
let ArgsList : Objekt = makeClass "ArgsList" Class []


// TODO: 'makeArgsList' creates a new argument list object. We want
// to be able to access arguments using e.g. `send "other" []` rather than
// by accessing slots. For this to work, we need to dynamically create
// a new subclass that inherits from ArgsList, containing the getters.
let makeArgsList (args : list<string * Objekt>) : Objekt =
  let getters = 
    // For each of the 'arg', we want to create a getter of the same
    // name, which uses 'tryFindSlot' to fetch the slot value.
    List.map (fun (s, o) -> (s, (fun inst -> fun fargs -> tryFindSlot s o |> Option.get))) args
  let specialArgsList = makeClass "__ArgsList" ArgsList getters
  // Now we create instance of the new class, containing the arg values
  makeInstance specialArgsList args


// TODO: 'send' now passes a second argument - the ArgsList object - to the
// method function, matching the new 'Objekt -> Objekt -> Objekt' signature.
// The error message also now includes the class name for easier debugging.
let send (name : string) (args : list<string * Objekt>) (obj : Objekt) : Objekt =
  // let cls = match tryFindSlot "name*" obj with
  //           | Some({Special = Some(String s)}) -> s
  //           | _ -> failwith "class doesnt have a name"
  match lookupMethod name (findClass obj) with
  | Some f -> f obj (makeArgsList args)
  | None -> failwith "message not understood, class: " // + cls

// ----------------------------------------------------------------------------
// Bootstrapping: Object, Class, and Str
// ----------------------------------------------------------------------------

// Now that 'send' is available, we can define 'Object' with real methods.
// 'getClass' returns the class of the receiver via direct slot access.
// 'Object' is the root class - it has no superclass.
let Object : Objekt = makeRootClass "Object" [
  "getClass", (fun inst _ ->
    inst |> tryFindSlot "class*" |> Option.get)
]

// 'Class' needs a 'getName' method but could not be created with 'makeClass'
// earlier. We create a properly-formed class object 'Class'' now and copy
// its slots into the 'Class' placeholder to retroactively give it methods.
let Class' : Objekt = makeRootClass "Class" [
  "getName", (fun inst _ ->
    inst |> tryFindSlot "name*" |> Option.get)
]
Class.Slots <- Class'.Slots

// Same bootstrapping for 'Str': create 'Str'' as a proper class inheriting
// from 'Object', then copy its slots into the 'Str' placeholder.
let Str' : Objekt = makeClass "String" Object [
  "append", (fun inst args ->
    // TODO: 'append' concatenates two strings.
    // Access the argument by sending "other" to 'args'.
    // Match both 'inst' and 'other' as Some(String s) and return makeString (s1 + s2).
    let receiver = send "target*" [] inst
    let arg = send "other" [] args
    match receiver.Special, arg.Special with
    | Some(String s1), Some(String s2) -> makeString (s1 + s2)
    | _ -> failwith "append error"
    )
]
Str.Slots <- Str'.Slots

// ----------------------------------------------------------------------------
// DEMO: Cat hierarchy with self-sends and reflection
// ----------------------------------------------------------------------------

// 'describe' calls 'speak' and 'pet' on the receiver (self-sends), then
// uses reflection ('getClass' + 'getName') to include the class name in output.
// The local '++' operator is shorthand for chained string append calls.
let Cat : Objekt = makeClass "Cat" Object [
  "pet",   (fun _ _ -> makeString "Purr!")
  "speak", (fun _ _ -> makeString "Meow!")
  "describe", (fun inst _ ->
    let spk = inst |> send "speak" []
    let pet = inst |> send "pet" []
    let cls = inst |> send "getClass" [] |> send "getName" []
    let (++) s1 s2 = s1 |> send "append" ["other", s2]
    makeString "I'm a "
    ++ cls
    ++ makeString " and I "
    ++ pet
    ++ makeString " when pet, and I say "
    ++ spk)
]

let CheshireCat : Objekt = makeClass "CheshireCat" Cat [
  "speak", (fun _ _ -> makeString "We are all mad!")
]

let mog = makeInstance Cat []
let cheshire = makeInstance CheshireCat []

ObjektVis.print mog
ObjektVis.print cheshire

// This is more manageable output!
ObjektVis.printLimit 2 mog
ObjektVis.printLimit 2 cheshire

// TESTS: basic method calls
mog |> send "pet" [] |> getStringValue  // "Purr!"
cheshire |> send "pet" [] |> getStringValue  // "Purr!"
mog |> send "speak" [] |> getStringValue  // "Meow!"
cheshire |> send "speak" [] |> getStringValue  // "We are all mad!"

// TESTS: Test the 'describe' method
cheshire |> send "describe" [] |> getStringValue
mog |> send "describe" [] |> getStringValue
