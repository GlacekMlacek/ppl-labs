// ----------------------------------------------------------------------------
// 04 - Flat memory model - storing variables in a memory 'array'
// ----------------------------------------------------------------------------

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression =
  | Const of Value
  | Function of string * Expression list
  // We will support only single-character variables and store their
  // value at a memory location determined by their ASCII code
  | Variable of char

type Command =
  | Print of Expression * bool
  | Goto of int
  // Variable name in all of the following also becomes 'char'
  | Assign of char * Expression
  | If of Expression * Command
  | For of char * Expression * Expression
  | Next of char
  // Added two functions for working with the flat memory representation:
  // POKE E1, E2 - sets the value at address 'E1' to the value of 'E2'
  // PEEK X, E - reads the value at address 'E' into a variable named 'X' (like ASSIGN)
  | Poke of Expression * Expression
  | Peek of char * Expression

type State =
  { Program : list<int * Command>
    // Replacing something like "Variables : Map<string, Value>" with 
    // a memory. We will only be able to store numerical values in the memory
    Memory : Map<int, int>
    LoopStack : list<char * int * int>
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

let getNumberValue value = 
  match value with
  | NumberValue n -> n
  | _ -> failwith "value is not number"

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let getVariableValue state (name:char) =
  // TODO: Variables are stored in Memory at the address equal to the ASCII
  // code of the variable name. Look up 'int name' in state.Memory and wrap
  // the result as NumberValue.
  match Map.tryFind (int name) state.Memory with
  | Some n -> n
  | None -> failwith "getVarVal failed"

let setVariableValue state (name:char) value =
  // TODO: Set the variable value in state.Memory. Extract the int from 'value'
  // using getNumberValue and store it in Memory at address 'int name'.
  { state with Memory = Map.add (int name) (getNumberValue value) state.Memory }

let printValue (value:Value) =
  match value with
  | StringValue s -> printf "%s" s
  | NumberValue n -> printf "%d" n
  | BoolValue b -> printf "%b" b

let rec evalExpression state (expr:Expression) : Value =
  match expr with
  | Const v -> v
  | Variable name ->
      // TODO: Use getVariableValue to read the variable's value from Memory.
      NumberValue (getVariableValue state name)
  | Function("=", [e1; e2]) -> BoolValue(evalExpression state e1 = evalExpression state e2)
  | Function("-", [e1; e2]) ->
      match evalExpression state e1, evalExpression state e2 with
      | NumberValue x, NumberValue y -> NumberValue(x - y)
      | _, _ -> failwith "wrong args for '-'"
  | Function(_, _) -> failwith "function not implemented"

let rec runCommand state cmd : State option =
  match cmd with
  | Print(expr, b) ->
      evalExpression state expr |> printValue
      if b then printf "\n"
      gotoNextLine state state.CurrentLine
  | Goto target ->
      Some { state with CurrentLine = target }
  | If(e, c) ->
      match evalExpression state e with
      | BoolValue b -> if b then runCommand state c else gotoNextLine state state.CurrentLine
      | _ -> failwith "if was expecting a bool"
  
  // | Assign(s, e) -> gotoNextLine { state with Variables = Map.add s (evalExpression state e) state.Variables } state.CurrentLine
  | Assign(name, expr) ->
      // TODO: Evaluate 'expr' and store the result using setVariableValue
      gotoNextLine (setVariableValue state name (evalExpression state expr)) state.CurrentLine

  | Poke(addr, expr) ->
      // TODO: Evaluate 'addr' to get the target memory address and 'expr' to
      // get the value. Write the value directly into Memory at that address.
      // Unlike Assign, this bypasses the variable name - any address can be
      // written, including one that happens to be a variable's location!
      gotoNextLine (setVariableValue state (char (getNumberValue (evalExpression state addr))) (evalExpression state expr)) state.CurrentLine
  
  | Peek(name, addr) ->
      // TODO: Evaluate 'addr' to get a memory address, read the int stored
      // there, and store it as the value of variable 'name' (use setVariableValue).
      // This is the read counterpart to Poke.
      runCommand state (Assign(name, Const (evalExpression state addr)))
  
  | For(v, e1, e2) ->
      match evalExpression state e1, evalExpression state e2 with
      | NumberValue lo, NumberValue hi ->
          let nstate = { state with LoopStack = List.append [(v, hi, state.CurrentLine)] state.LoopStack }
          runCommand nstate (Assign(v, Const (NumberValue lo)))
      | _ -> failwith "forloop init error"
  | Next v ->
      let vv = 1 + getVariableValue state v
      let nstate = setVariableValue state v (NumberValue vv)
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
  { Program = List.sortBy fst prog; LoopStack = []; Memory = Map.empty; CurrentLine = 10 }

let testVariables =
  [ 10, Assign('I', Const(NumberValue 1))
    30, Print(Variable 'I', true) ]

// DEMO: Simpler test program with variables
runProgram (makeProgram testVariables)

let helloTen =
  [ 10, Assign('I', Const(NumberValue 10))
    20, If(Function("=", [Variable('I'); Const(NumberValue 1)]), Goto(60))
    30, Print (Const(StringValue "HELLO WORLD"), true)
    40, Assign('I', Function("-", [ Variable('I'); Const(NumberValue 1) ]))
    50, Goto 20
    60, Print (Const(StringValue ""), true) ]

// NOTE: Prints hello world ten times using conditionals
runProgram (makeProgram helloTen)

// DEMO: We can set value in memory at some arbitrary address
// then we can read it into a variable and print the variable value...
let peekPokeDemo =
  [ 10, Poke(Const(NumberValue 100), Const(NumberValue 42))
    20, Peek('X', Const(NumberValue 100))
    30, Print(Variable 'X', true) ]

runProgram (makeProgram peekPokeDemo)

// The following demo shows that we can set variable values using Poke!
// If we know their memory location, we can set them (here we set all three
// using a single for loop).
let pokeVars =
  [ 10, Assign('A', Const(NumberValue 0))
    20, Assign('B', Const(NumberValue 0))
    30, Assign('C', Const(NumberValue 0))
    35, Print(Const(StringValue "hi"), true)
    40, For('I', Const(NumberValue 65), Const(NumberValue 67))
    50, Poke(Variable('I'), Variable('I'))
    60, Next('I')
    70, Print(Variable 'A', true)
    80, Print(Variable 'B', true)
    90, Print(Variable 'C', true) ]

runProgram (makeProgram pokeVars)
