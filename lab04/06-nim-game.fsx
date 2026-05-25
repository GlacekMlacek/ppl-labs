// ----------------------------------------------------------------------------
// 06 - Adding input, implementing NIM game & improving it with GOSUB
// ----------------------------------------------------------------------------
open System

// NOTE: This task has two parts. In the first part, we add INPUT <V> and STOP
// commands to be able to play a little game called NIM (see code at the end).
// In the second part, we can use a better implementation that uses a 
// BASIC subroutine (procedure) call using GOSUB.

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression =
  | Const of Value
  | Function of string * Expression list
  | Variable of char

type Command =
  // Note - I modify PRINT to take a list of expressions (real C64 BASIC allows 
  // this too and it makes the NIM game implementation a bit less horrible)
  | Print of Expression list
  | Goto of int
  | Assign of char * Expression
  | If of Expression * Command
  | For of char * Expression * Expression
  | Next of char
  | Poke of Expression * Expression
  | Peek of char * Expression
  | Clear
  | Update
  
  // INPUT <V> reads a number from the console and stores it as variable 'v'
  // STOP terminates the program without continuing to the next line
  | Input of char
  | Stop
  // GOSUB <N> calls a procedure defined on line <N>. To do this, it pushes
  // the current line onto the call stack and jumps to line 'n'. 
  // RETURN pops the call stack and continues from the line after the GOSUB.
  | GoSub of int
  | Return


type State =
  { Program : list<int * Command>
    Memory : Map<int, int>
    LoopStack : list<char * int * int>
    // Note: CallStack holds return addresses pushed by GOSUB
    CallStack : list<int>
    CurrentLine : int
    Random : System.Random }


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

let getNumberValue value =
  match value with
  | NumberValue n -> n
  | _ -> failwith "value is not number"

let getVariableValue state (name:char) =
  match Map.tryFind (int name) state.Memory with
  | Some n -> n
  | None -> int ' '

let setVariableValue state (name:char) value =
  { state with Memory = Map.add (int name) (getNumberValue value) state.Memory }

let printValue (value:Value) =
  match value with
  | StringValue s -> printf "%s" s
  | NumberValue n -> printf "%d" n
  | BoolValue b -> printf "%b" b

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let binaryBoolOp f args = 
  match args with 
  | [BoolValue a; BoolValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let binaryNumOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> NumberValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr =
  // TODO: Extend evalExpression with the 'MIN(E1, E2)' function, which evaluates
  // both arguments and returns the smaller of the two as a NumberValue.
  // (All other cases are implemented in steps 1-5.)
  match expr with
  | Const v -> v
  | Variable name ->
      NumberValue (getVariableValue state name)
  | Function(op, args) ->
      let eargs = List.map (fun e -> evalExpression state e) args
      match op with
      | "RND" -> NumberValue (state.Random.Next(0, getNumberValue (List.head eargs)))
      | "MIN" -> NumberValue (List.min (List.map (fun e -> getNumberValue e) eargs))
      | "=" -> binaryRelOp (=) eargs
      | ">" -> binaryRelOp (>) eargs
      | "<" -> binaryRelOp (<) eargs
      | "-" -> binaryNumOp (-) eargs
      | "+" -> binaryNumOp (+) eargs
      | "*" -> binaryNumOp (*) eargs
      | "||" -> binaryBoolOp (||) eargs
      | _ -> failwith "unsupported func"

let rec runCommand state cmd : State option =
  match cmd with
  | Goto target ->
      Some { state with CurrentLine = target }
  | If(e, c) ->
      match evalExpression state e with
      | BoolValue b -> if b then runCommand state c else gotoNextLine state state.CurrentLine
      | _ -> failwith "if was expecting a bool"
  | Assign(name, expr) ->
      gotoNextLine (setVariableValue state name (evalExpression state expr)) state.CurrentLine
  | Poke(addr, expr) ->
      gotoNextLine (setVariableValue state (char (getNumberValue (evalExpression state addr))) (evalExpression state expr)) state.CurrentLine
  | Peek(name, addr) ->
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
  | Update ->
      Console.CursorLeft <- 0
      Console.CursorTop <- 0
      for r in 0 .. 19 do
          for c in 0 .. 59 do
              printf "%c" (char (getVariableValue state (char (1024 + r * 60 + c))))
          printf "\n"
      gotoNextLine state state.CurrentLine
  | Clear ->
      let mutable nstate = state
      for i in 1024 .. 1024 + 20 * 60 - 1 do
            nstate <- setVariableValue nstate (char i) (NumberValue (int ' '))
      gotoNextLine nstate nstate.CurrentLine
  | Print exprs ->
      // TODO: Print now takes a list of expressions rather than a single one.
      // Iterate over 'exprs', evaluate each one and print its value using
      // 'printValue'. Then advance to the next line.
      let _ = List.map (fun e -> printValue (evalExpression state e)) exprs
      gotoNextLine state state.CurrentLine
  | Stop ->
      // TODO: STOP terminates the program immediately.
      // Recall that runCommand returns 'State option': return the value that
      // signals "no more lines to run" to make runProgram stop.
      None

  | Input name ->
      // TODO: INPUT <V> reads a number from the user and stores it in variable 'name'.
      // Use Console.ReadLine() and Int32.TryParse to read an integer. If parsing
      // fails (user typed something that isn't a number), ask again - keep looping
      // until you get a valid number. Then store it and advance to the next line.
      match Int32.TryParse (Console.ReadLine()) with
      | true, x -> runCommand state (Assign(name, Const(NumberValue x)))
      | _ -> runCommand state (Input name)
  // NOTE: You can skip GoSub and Return now and run the first version of the game!
  // (Return to these later to run the nicer version of the game...)
  | GoSub target ->
      // TODO: GOSUB <N> calls a subroutine starting at line <N>.
      // Before jumping, save the current line on the Stack so that RETURN can
      // come back here. Then jump to 'target' (hint: delegate to Goto).
      runCommand { state with CallStack = List.append state.CallStack [state.CurrentLine] } (Goto target)

  | Return ->
      // TODO: RETURN comes back from a subroutine called by GOSUB.
      // Pop the top of the Stack to get the line the GOSUB was on, then use
      // gotoNextLine to continue from the line *after* that GOSUB.
      // If the Stack is empty, there is no matching GOSUB - fail with an error.
      if state.CallStack.IsEmpty then failwith "empty callstack"
      let jmp = List.rev state.CallStack |> List.head
      runCommand { state with CallStack = List.rev state.CallStack |> List.tail |> List.rev } (Goto jmp)

let rec runCurrentCommand state = 
  runCommand state (getCurrentCommand state)

let rec runProgram state : unit =
  match runCurrentCommand state with
  | Some nstate -> runProgram nstate
  | None -> ()

// ----------------------------------------------------------------------------
// Test case - NIM game with GOSUB
// ----------------------------------------------------------------------------

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var (n:char) = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let makeProgram prog =
  { Program = List.sortBy fst prog; LoopStack = []; CallStack = [];
    Memory = Map.empty; Random = System.Random(); CurrentLine = 10 }

// NOTE: A simple game you should be able to run now! :-)
// NIM - two players alternate turns (via GoSub) removing 1-5 matches;
// who takes the last match wins the game.

let nimDirect = 
  [ 10, Assign('M', num 20)
    20, Print [ str "THERE ARE "; var 'M'; str " MATCHES LEFT\n" ]
    30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var 'M']; str " MATCHES\n" ]
    40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    50, Input('P')
    60, If((var 'P' .< num 1) .|| (var 'P' .> num 5) .|| (var 'P' .> var 'M'), Goto 40)
    70, Assign('M', var 'M' .- var 'P')
    80, If(var 'M' .= num 0, Goto 200)
    90, Print [ str "THERE ARE "; var 'M'; str " MATCHES LEFT\n" ]
    100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var 'M']; str " MATCHES\n" ]
    110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    120, Input('P')
    130, If((var 'P' .< num 1) .|| (var 'P' .> num 5) .|| (var 'P' .> var 'M'), Goto 110)
    140, Assign('M', var 'M' .- var 'P')
    150, If(var 'M' .= num 0, Goto 220)
    160, Goto 20
    200, Print [str "PLAYER 1 WINS!"]
    210, Stop
    220, Print [str "PLAYER 2 WINS!"]
    230, Stop
  ]

runProgram (makeProgram nimDirect)

// NOTE: NIM - the above version has a lot of repetition for player 1 and 2.
// We can turn this into a subroutine (lines 100-170) that is shared between
// the two players. The program still uses global variables:
//
// 'M' = matches remaining, 'U' = current player number, 'P' = player's pick.
//
let nimGosub = 
  [ 10, Assign('M', num 20)
    
    20, Assign('U', num 1)
    30, GoSub(100)
    40, Assign('U', num 2)
    50, GoSub(100)
    60, Goto(20) 

    100, Print [ str "THERE ARE "; var 'M'; str " MATCHES LEFT\n" ]
    110, Print [ str "PLAYER "; var 'U'; str ": YOU CAN TAKE BETWEEN 1 AND "; 
      Function("MIN", [num 5; var 'M']); str " MATCHES\n" ]
    120, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    130, Input('P')
    140, If((var 'P' .< num 1) .|| (var 'P' .> num 5) .|| (var 'P' .> var 'M'), Goto 120)
    150, Assign('M', var 'M' .- var 'P')
    160, If(var 'M' .= num 0, Goto 200)
    170, Return    
    
    200, Print [str "PLAYER "; var 'U'; str " WINS!"]
  ]


runProgram (makeProgram nimGosub)
