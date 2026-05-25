// ----------------------------------------------------------------------------
// 03 - Implementing FOR loops with a stack
// ----------------------------------------------------------------------------

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command =
  // NOTE: Print takes a bool indicating whether to append a newline after
  // the value. Use 'true' for a line on its own, 'false' to keep printing
  // on the same line (e.g. for dots inside a loop body).
  | Print of Expression * bool
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: For(v, lo, hi) initialises variable 'v' to 'lo' and loops until
  // 'v' exceeds 'hi'. Next(v) increments 'v' and jumps back to the body
  // of the loop, or falls through once the limit is passed.
  | For of string * Expression * Expression
  | Next of string

type State =
  { Program : list<int * Command>
    Variables : Map<string, Value>
    // NOTE: LoopStack tracks active FOR loops as (variable, limit, for-line).
    // The for-line is stored so that NEXT can jump back to the line immediately
    // after FOR, which is where the loop body begins.
    LoopStack : list<string * int * int>
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

let getNumberValue (value:Value) : int =
  // TODO: Helper that extracts numerical value or fails
  match value with
  | NumberValue n -> n
  | _ -> failwith "value is not number"
  
// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let printValue (value:Value) =
  match value with
  | StringValue s -> printf "%s" s
  | NumberValue n -> printf "%d" n
  | BoolValue b -> printf "%b" b
let rec evalExpression state (expr:Expression) : Value =
  match expr with
  | Const v -> v
  | Function("=", [e1; e2]) -> BoolValue(evalExpression state e1 = evalExpression state e2)
  | Function("-", [e1; e2]) ->
      match evalExpression state e1, evalExpression state e2 with
      | NumberValue x, NumberValue y -> NumberValue(x - y)
      | _, _ -> failwith "wrong args for '-'"
  | Function(_, _) -> failwith "function not implemented"
  | Variable v ->
      match Map.tryFind v state.Variables with
      | Some v -> v
      | None -> failwith "variable not found"

let rec runCommand state cmd : State option =
  match cmd with
  | Print(expr, b) ->
      evalExpression state expr |> printValue
      if b then printf "\n"
      gotoNextLine state state.CurrentLine
      // TODO: Modify 'printValue' to use 'printf' (not 'printfn')
      // and print a '\n' character here if required. 
  | Goto target ->
      Some { state with CurrentLine = target }
  | Assign(s, e) -> gotoNextLine { state with Variables = Map.add s (evalExpression state e) state.Variables } state.CurrentLine
  | If(e, c) ->
      match evalExpression state e with
      | BoolValue b -> if b then runCommand state c else gotoNextLine state state.CurrentLine
      | _ -> failwith "if was expecting a bool"

  // TODO: FOR <v> = <e1> TO <e2> sets the loop variable <V> to the lower bound
  // (obtained by evaluating <e1>). We then need to remember that we started
  // looping! To do this, push information about (i) variable name, (ii) upper
  // bound and (iii) the current line) to LoopStack so that NEXT <v> knows 
  // where to jump and when to stop. Then continue into the loop body.
  // (hint: use getNumberValue helper here!)
  // | For of string * Expression * Expression
  // | Next of string
  | For(v, e1, e2) ->
      match evalExpression state e1, evalExpression state e2 with
      | NumberValue lo, NumberValue hi ->
          let nstate = { state with Variables = Map.add v (NumberValue lo) state.Variables; LoopStack = List.append [(v, hi, state.CurrentLine)] state.LoopStack }
          gotoNextLine nstate state.CurrentLine
      | _ -> failwith "forloop init error"

  // TODO: NEXT <v> increments the variable <v> by 1. Then look through the 
  // LoopStack to find the loop for this variable. If we are within bounds,
  // use gotoNextLine to jump to the line just after the loop start. If we 
  // finished looping, remove the LoopStack record (hint: List.filter) and 
  // continue (gotoNextLine).
  | Next v ->
      let vv = getNumberValue (Map.find v state.Variables) + 1
      let nstate = { state with Variables = Map.add v (NumberValue vv) state.Variables }
      let _, limit, jmp = List.find (fun (name, _, _) -> name = v) state.LoopStack
      if vv <= limit then gotoNextLine nstate jmp else gotoNextLine nstate nstate.CurrentLine


let rec runCurrentCommand state = 
  runCommand state (getCurrentCommand state)

let rec runProgram state : unit =
  match runCurrentCommand state with
  | Some nstate -> runProgram nstate
  | None -> ()

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let makeProgram prog = 
  { Program = List.sortBy fst prog; Variables = Map.empty; 
    LoopStack = []; CurrentLine = 10 }


let helloTen =
  [ 10, Assign("I", Const(NumberValue 10))
    20, If(Function("=", [Variable("I"); Const(NumberValue 0)]), Goto(60))
    30, Print(Const(StringValue "HELLO WORLD"), true)
    40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    50, Goto 20
    60, Print(Const(StringValue ""), false) ]

// NOTE: Prints hello world ten times using conditionals
runProgram (makeProgram helloTen)

let helloFor =
  [ 10, For("I", Const(NumberValue 1), Const(NumberValue 10))
    20, Print(Const(StringValue "HELLO WORLD"), true)
    30, Next("I") ]

// NOTE: Same result as helloTen but expressed with FOR/NEXT
runProgram (makeProgram helloFor)

let nestedLoops =
  [ 10, For("I", Const(NumberValue 1), Const(NumberValue 10))
    20, For("J", Const(NumberValue 1), Variable("I"))
    30, Print(Const(StringValue "."), false)
    40, Next("J")
    50, Print(Const(StringValue ""), true)
    60, Next("I") ]

// NOTE: Outer loop I from 1 to 10; inner loop prints I dots, then a newline.
// Result is a triangle: one dot on row 1, two on row 2, ..., ten on row 10.
runProgram (makeProgram nestedLoops)
