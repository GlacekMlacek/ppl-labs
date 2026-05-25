// ----------------------------------------------------------------------------
// 02 - Adding conditionals and variables
// ----------------------------------------------------------------------------

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command =
  | Print of Expression
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that 
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State = 
  { Program : list<int * Command>
    // TODO: Add variable context to the program state
    Variables : Map<string, Value>
    CurrentLine : int }


// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let gotoNextLine (state:State) line : State option = 
  match List.tryFind (fun (n, _) -> n > line) state.Program with
  | Some(n, _) -> Some { state with CurrentLine = n }
  | None -> None
  
let getCurrentCommand state : Command =
  snd (List.find (fun (n, _) -> n = state.CurrentLine) state.Program)
  
// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let printValue (value:Value) =
  // TODO: Add support for printing NumberValue and BoolValue
  match value with
  | StringValue s -> printf "%s" s
  | NumberValue n -> printf "%d" n
  | BoolValue b -> printf "%b" b


let rec evalExpression (expr:Expression) state : Value =
  // TODO: Add support for 'Function' and 'Variable'. For now, handle just the two
  // functions we need, i.e. "-" (takes two numbers & returns a number) and "="
  // (takes two values and returns Boolean). Note that you can test if two
  // F# values are the same using '='. It works on values of type 'Value' too.
  //
  // HINT: You will need to pass the program state to 'evalExpression' 
  // in order to be able to handle variables!
  match expr with
  | Const(v) -> v
  | Function("=", [e1; e2]) -> BoolValue(evalExpression e1 state = evalExpression e2 state)
  | Function("-", [e1; e2]) ->
      match evalExpression e1 state, evalExpression e2 state with
      | NumberValue x, NumberValue y -> NumberValue(x - y)
      | _, _ -> failwith "wrong args for '-'"
  | Function(_, _) -> failwith "function not implemented"
  | Variable v ->
      match Map.tryFind v state.Variables with
      | Some v -> v
      | None -> failwith "variable not found"


let rec runCommand cmd state : State option =
  match cmd with
  | Print(expr) ->
      evalExpression expr state |> printValue
      gotoNextLine state state.CurrentLine
  | Goto(target) ->
      Some { state with CurrentLine = target }
  // TODO: Implement assignment and conditional. 
  // Assignment should go to the next line after setting the variable like Print.
  // Conditional should evaluate the expression and, if 'true' call 'runCommand'
  // recursively to run the command. Otherwise, it goes to the next line.
  // | If of Expression * Command
  | Assign(s, e) -> gotoNextLine { state with Variables = Map.add s (evalExpression e state) state.Variables } state.CurrentLine
  | If(e, c) ->
      match evalExpression e state with
      | BoolValue b -> if b then runCommand c state else gotoNextLine state state.CurrentLine
      | _ -> failwith "if was expecting a bool"



let rec runCurrentCommand state = 
  runCommand (getCurrentCommand state) state
let rec runProgram state : unit = 
  match runCurrentCommand state with
  | Some nstate -> runProgram nstate
  | None -> ()

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// TODO: Add empty variables to the initial state!
let makeProgram prog = { Program = prog; Variables = Map.empty; CurrentLine = 10 } 

let testVariables = 
  [ 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    20, Assign("I", Const(NumberValue 1))
    30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    40, Print(Variable "S") 
    50, Print(Variable "I") 
    60, Print(Variable "B") ]

// DEMO: Simpler test program without 'If" (just variables and '=' function) 
runProgram (makeProgram testVariables)

let helloTen = 
  [ 10, Assign("I", Const(NumberValue 10))
    20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    30, Print (Const(StringValue "HELLO WORLD\n")) 
    40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    50, Goto 20
    60, Print (Const(StringValue "")) ]

// NOTE: Prints hello world ten times using conditionals
runProgram (makeProgram helloTen)
