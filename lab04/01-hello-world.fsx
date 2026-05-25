// ----------------------------------------------------------------------------
// 01 - Minimal BASIC interpreter with printing, GOTO and infinite loops!
// ----------------------------------------------------------------------------

// Represents primitive values such as strings, numbers, booleans
type Value =
  | StringValue of string

// Expression can be evaluated to get a value (like ML expressions)
type Expression = 
  | Const of Value

// Commands are imperative actions that transform the program state
type Command =
  // PRINT prints the specified expression - for now, just a constant
  | Print of Expression
  // NOTE: GOTO specified line number. Note that this is an integer, rather
  // than an expression, so you cannot calculate line number dynamically.
  // (But there are tricks to do this by direct memory access on a real C64!)
  | Goto of int

// State consists of the running program (in our simple system, 
// we do not modify the program, but in real BASIC, this is possible)
// and the currently executing line of the program.
type State = 
  { Program : list<int * Command>
    CurrentLine : int }


// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let gotoNextLine (state:State) line : State option = 
  // TODO: Move to the next line after 'line'. Find out what is the next line
  // number in the program (hint: use List.tryFind) and return a new state
  // where 'CurrentLine' is set to the next line. If there is no next line, 
  // return 'None', otherwise return 'Some newState'. You can assume that the 
  // list of program lines is sorted.
  match List.tryFind (fun (n, _) -> n > line) state.Program with
  | Some(n, _) -> Some { Program = state.Program; CurrentLine = n }
  | None -> None

let getCurrentCommand state : Command =
  // TODO: Return the command at the current line (hint: use List.find)
  // You can assume that the state is well-formed, i.e., the line is there.
  snd (List.find (fun (n, _) -> n = state.CurrentLine) state.Program)

// Test cases
let state1 = { Program = [ 10, Print(Const(StringValue "HI")) ]; CurrentLine = 10 }
gotoNextLine state1 0     // Returns state with CurrentLine=10
gotoNextLine state1 10    // Returns None
getCurrentCommand state1  // Returns the Print command


// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let printValue (value:Value) =
  // TODO: Print the value nicely using e.g. printf "%s" for strings
  match value with
  | StringValue(s) -> printf "%s" s


let rec evalExpression (expr:Expression) : Value =
  // TODO: Evaluate an expression - for now, this is trivial because
  // our only expression is a constant which contains a value!
  match expr with
  | Const(v) -> v

let rec runCurrentCommand state = 
  // Note that 'runCommand' takes the command to run (this will be useful in 
  // the next step), but we define 'runCurrentCommand' so that we can easily
  // run the command on the current line.
  runCommand (getCurrentCommand state) state

and runCommand cmd state : State option =
  match cmd with
  | Print(expr) ->
      // TODO: Evaluate the expression and print the resulting value
      // Then return new state with the next line (hint: gotoNextLine)
      evalExpression expr |> printValue
      gotoNextLine state state.CurrentLine

  | Goto(target) ->
      // TODO: Return a new state with the modified CurrentLine
      Some { Program = state.Program; CurrentLine = target }


let rec runProgram state : unit = 
  // NOTE: You can skip this and return to it when needed later!
  // TODO: Run the program. Call 'runCommand' in a loop until
  // the function returns 'None' indicating the end of the program.
  // Then return the unit value - you can write just "()" to return.
  match runCurrentCommand state with
  | Some nstate -> runProgram nstate
  | None -> ()

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce =   
  { CurrentLine = 10
    Program = [ 
      10, Print (Const (StringValue "HELLO WORLD !!!\n")) 
    ] }

// DEMO: Prints HELLO WORLD once and returns None
runCurrentCommand helloOnce

let helloInf = 
  { CurrentLine = 10
    Program = [ 
      10, Print (Const (StringValue "HELLO WORLD\n")) 
      20, Goto 10 ] }

// DEMO: Prints HELLO WORLD and then jumps next. We can 
// run as many iterations of this as we want!
runCurrentCommand helloInf
|> Option.bind runCurrentCommand
|> Option.bind runCurrentCommand
|> Option.bind runCurrentCommand
|> Option.bind runCurrentCommand


// DEMO: Run the program - helloOnce terminates; helloInf
// should run in an infinite loop (Ctrl+C to stop it!)
runProgram helloOnce
runProgram helloInf

