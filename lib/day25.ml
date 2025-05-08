type arg  = Register of char
          | Value of int
type inst = Cpy of arg * arg
          | Jnz of arg * arg
          | Inc of arg
          | Dec of arg
          | Out of arg

type state = {a : int; b : int; c : int; d : int}

let read (s : state) (r : char) = match r with
  | 'a' -> s.a
  | 'b' -> s.b
  | 'c' -> s.c
  | 'd' -> s.d
  |  _  -> raise (Util.Generic "No R!")

let write (s : state) (r : char) (v : int) = match r with
  | 'a' -> {s with a = v}
  | 'b' -> {s with b = v}
  | 'c' -> {s with c = v}
  | 'd' -> {s with d = v}
  |  _  -> raise (Util.Generic "No R!")

let input = CCString.lines @@ Util.slurp "./rsc/day25.txt"

let to_arg (str : string) =
  try Value (Stdlib.int_of_string str)
  with Failure _ -> Register (List.hd @@ CCString.to_list str)

let parse (line : string) = 
  let args = CCString.to_list @@ String.sub line 4 (String.length line - 4)
   in match (String.sub line 0 3) with
      | "cpy" -> let (a,b) = Util.split_on ' ' args
                  in Cpy (to_arg @@ CCString.of_list a, to_arg @@ CCString.of_list b)
      | "jnz" -> let (a,b) = Util.split_on ' ' args
                  in Jnz (to_arg @@ CCString.of_list a, to_arg @@ CCString.of_list b)
      | "inc" -> Inc (to_arg @@ CCString.of_list args)
      | "dec" -> Dec (to_arg @@ CCString.of_list args)
      | "out" -> Out (to_arg @@ CCString.of_list args)
      | _     -> raise (Util.Generic "No I!")

let prog = List.map parse input
let regs = {a = 0; b = 0; c = 0; d = 0}

let rec execute (i_ptr : int) (code : inst list) (mem : state) (clk : int list) = 
  if (i_ptr >= List.length code) || (i_ptr < 0) then false
    else if List.length clk > 100 then true
            else match (List.nth code i_ptr) with
                   | Inc (Register r) ->
                       let mem_ = write mem r @@ (read mem r) + 1 in
                       let i_   = i_ptr + 1 in
                         execute i_ code mem_ clk
                   | Dec (Register r) ->
                       let mem_ = write mem r @@ (read mem r) - 1 in
                       let i_   = i_ptr + 1 in
                         execute i_ code mem_ clk
                   | Cpy (Register a, Register b) ->
                       let mem_ = write mem b @@ (read mem a) in
                       let i_   = i_ptr + 1 in
                         execute i_ code mem_ clk
                   | Cpy (Value a, Register b) ->
                       let mem_ = write mem b a in
                       let i_   = i_ptr + 1 in
                         execute i_ code mem_ clk
                   | Jnz (Register a, Value b) ->
                       if (read mem a) = 0
                          then execute (i_ptr + 1) code mem clk
                          else execute (i_ptr + b) code mem clk
                   | Jnz (Value 0, _)       -> execute (i_ptr + 1) code mem clk
                   | Jnz (Value _, Value b) -> execute (i_ptr + b) code mem clk
                   | Out (Value 1) ->
                       (match clk with |   []    -> execute (i_ptr + 1) code mem (1 :: clk)
                                      | (0 :: _) -> execute (i_ptr + 1) code mem (1 :: clk)
                                      |    _     -> false)
                   | Out (Value 0) ->
                       (match clk with |   []    -> execute (i_ptr + 1) code mem (0 :: clk)
                                      | (1 :: _) -> execute (i_ptr + 1) code mem (0 :: clk)
                                      |    _     -> false)
                   | Out (Value _) -> false
                   | Out (Register r) -> 
                       (match (read mem r) with
                         | 0 -> (match clk with |   []    -> execute (i_ptr + 1) code mem (0 :: clk)
                                                | (1 :: _) -> execute (i_ptr + 1) code mem (0 :: clk)
                                                |    _     -> false)
                         | 1 -> (match clk with |   []    -> execute (i_ptr + 1) code mem (1 :: clk)
                                                | (0 :: _) -> execute (i_ptr + 1) code mem (1 :: clk)
                                                |    _     -> false)
                         | _ -> false)
                   | _ -> raise (Util.Generic "No I!")

let rec experiment (a : int) = match (execute 0 prog {regs with a = a} []) with 
  | true  -> a
  | false -> experiment @@ a + 1

let silver : int  = experiment 0
let gold : string = "Signal transmitted"

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver 
                      ^ "\n\tGold:\t" ^ gold
