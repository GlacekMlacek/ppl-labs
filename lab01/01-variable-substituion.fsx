// ============================================================================
// STEP #1 - Implementing variable substitution and getting free variables
// ============================================================================

// This is our representation of lambda terms. A term can be:
type Term = 
  // An access to a named variable - lambda term "x" would be Variable("x")
  | Variable of string

  // A lambda function consisting of a variable name and body (another term)
  // For example '(\x.x)' would be: Lambda("x", Variable("x"))
  // Note that the variable is just a string but the body is another term!
  | Lambda of string * Term

  // A function application - the first term is the function, 
  // the second argument. For example '(\x.x) y' would be:
  //   Application(Lambda("x", Variable("x")), Variable("y"))
  | Application of Term * Term

// ============================================================================
// Some sample terms we will use later for testing
// ============================================================================

let t1 = Variable("x")
let t2 = Lambda("x", Variable("x"))
let t3 = Application(Lambda("x", Variable("x")), Variable("y"))

// ============================================================================
// Basic pattern matching on lambda terms
// ============================================================================

// TASK #1: First, let's write a function that returns string
// with basic information about the top-level structure that
// appears in the lambda term (this looks at the term itself
// only and does not need recursion)

let getTopLevelStructure t =
  match t with
  | Variable(v) -> $"variable: {v}" 
  | Lambda(v, _) -> $"lambda: {v}"
  | Application(_, _) -> "application"

getTopLevelStructure t1 = "variable: x" |> printfn "%A"
getTopLevelStructure t2 = "lambda: x" |> printfn "%A"
getTopLevelStructure t3 = "application" |> printfn "%A"

getTopLevelStructure (Variable("zzz")) = "variable: zzz" |> printfn "%A"
getTopLevelStructure (Lambda("zzz", Variable("x"))) = "lambda: zzz" |> printfn "%A"

printfn ""

// ============================================================================
// Basic recursive term processing
// ============================================================================

// TASK #2: Now, let's try writing a recursive function that works with lambda
// terms. The following should return a string with the names of all variables
// that are accessed in the term (in the order in which they appear, without
// deduplication) - just concatenate all variable names.

// DMEO: You can concatenate strings in F# with '+'
let hello = "hello"
let world = "world"
let helloworld = hello + " " + world + "!"

// NOTE: This is a recursive function marked as 'rec'!
let rec allAccessedVariables t = 
  match t with 
  | Variable(v) -> v
  | Lambda(v, t) -> allAccessedVariables t
  | Application(t1, t2) -> allAccessedVariables t1 + "," + allAccessedVariables t2


allAccessedVariables t1 = "x" |> printfn "t1 %A"
allAccessedVariables t2 = "x" |> printfn "t2 %A"
allAccessedVariables t3 = "x,y" |> printfn "t3 %A"

let txyz = Application(Application(Variable("x"), Variable("y")), Variable("z"))
let txxx = Application(Application(Variable("x"), Variable("x")), Variable("x"))
allAccessedVariables txyz = "x,y,z" |> printfn "txyz %A"
allAccessedVariables txxx = "x,x,x" |> printfn "txxx %A"

printfn ""

// ============================================================================
// Pretty printing lambda terms
// ============================================================================

// NOTE: This one has been done for you :-)
// In practice, this would be nicer using string interpolation, 
// but the example shows how to write pattern matching with multiple
// lines in the body and 'let' binding to define local variables.
// You will need this in the next step!

let rec format term = 
  match term with 
  | Lambda(x, t) -> 
      let body = format t
      "(\\" + x + "." + body + ")"
  | Application(t1, t2) -> 
      let s1 = format t1
      let s2 = format t2
      "(" + s1 + " " + s2 + ")"
  | Variable x -> x

format t1 = "x" |> printfn "%A"
format t2 = "(\\x.x)" |> printfn "%A"
format t3 = "((\\x.x) y)" |> printfn "%A"

printfn ""

// ============================================================================
// Defining variable substitution
// ============================================================================

// TASK #3: Implement variable substitution!
//
// The function 'substitute var subst term' should substitute (replace) the 
// variable named 'var' with the term 'subst' inside the original term 'term'.
//
// NOTE: To help you implement the function, the following header uses type
// annotations to tell the F# compiler what types to expect. This means 
// you'll get better error messages!
let rec substitute (var:string) (subst:Term) (term:Term) : Term = 
  match term with
  | Variable(v) -> if v = var then subst else term
  | Lambda(v, t) -> if v = var then term else Lambda(v, substitute var subst t)
  | Application(t1, t2) -> Application(substitute var subst t1, substitute var subst t2)
  // * If the term is 'Variable' then you need 'if' to decide between two cases:
  //   - the variable is 'var' - return the substitution
  //   - the variable is something else - return it unmodified
  // * If the term is 'Lambda' then we again need two cases:
  //   - the lambda defines variable that is the same as 'var' - return it unmodified
  //   - the lambda defines some other variable - do substitution in the body
  // * If the term is 'Application', call substitution recursively on both terms
  


// TEST: The term is just a variable and it gets substituted 
let ts1 = Variable("x")
let sub = Lambda("z", Variable("z"))
format (substitute "x" sub ts1) = "(\\z.z)" |> printfn "ts1 %A"

// TEST: The term contains the variable twice - both get substituted
let ts2 = Application(Variable("x"), Variable("x"))
format (substitute "x" sub ts2) = "((\\z.z) (\\z.z))" |> printfn "%A"

// TEST: Variable 'y' is free and so it gets substituted for 'z'
let ts3 = Lambda("x", Variable("y"))
format (substitute "y" (Variable("z")) ts3) = "(\\x.z)" |> printfn "%A"

// TEST: Variable 'x' is bound by the lambda - no substitution happens!
let ts4 = Lambda("x", Variable("x"))
format (substitute "x" (Variable("z")) ts4) = "(\\x.x)" |> printfn "ts4 %A"

printfn ""

// ============================================================================
// Getting free variables of a term
// ============================================================================

// A free variable is a variable that is not bound by any lambda - i.e.,
// it is not reference to a variable defined by any lambda in the term.

// TASK #4: Implement a recursive function that returns 
// the set of free variables in a lambda term!

// The easiest way to do this is using operations for working with sets.
// The following shows all the syntax you will need.

let s1 = set [ 1; 2; 3 ] // Create a set with 1,2,3
let s2 = set [ 3; 4; 5 ] // Create a set with 3,4,5
let su = s1 + s2  // Union of sets becomes 1,2,3,4,5
let sm = su - set [ 1; 3; 5 ] // Remove elements from the set


let rec freeVariables (term:Term) : Set<string> = 
  match term with
  | Variable(v) -> set [v]
  | Lambda(v, t) -> freeVariables t - set [v]
  | Application(t1, t2) -> freeVariables t1 + freeVariables t2
  // * If the term is 'Variable' the variable is free - return it as a singleton set
  // * If the term is 'Application', get the free variables of the two subtersm and
  //   return a union of the two sets using +
  // * If the term is 'Lambda', get the free variables of the body - this may include
  //   the variable defined by the lambda, so we need to remove this using '-'!


let tf1 = Application(Variable("x"), Variable("y"))
freeVariables tf1 = set ["x"; "y"] |> printfn "tf1 %A"

let tf2 = Lambda("x", Application(Variable("z"), Variable("z")))
freeVariables tf2 = set ["z"] |> printfn "%A"

let tf3 = Lambda("x",Variable("x"))
freeVariables tf3 = set [] |> printfn "tf3 %A"

