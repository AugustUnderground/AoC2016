type arg  = Register of char
          | Value of int
type inst = Cpy of arg * arg
          | Jnz of arg * arg
          | Inc of arg
          | Dec of arg

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

let input = CCString.lines @@ Util.slurp "./rsc/day12.txt"

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
      | _     -> raise (Util.Generic "No I!")

let prog = List.map parse input
let regs = {a = 0; b = 0; c = 0; d = 0}

let rec execute (i_ptr : int) (code : inst list) (mem : state) = 
  if i_ptr >= List.length code
     then mem
     else match (List.nth code i_ptr) with
            | Inc (Register r) ->
                let mem_ = write mem r @@ (read mem r) + 1 in
                let i_   = i_ptr + 1 in
                  execute i_ code mem_
            | Dec (Register r) ->
                let mem_ = write mem r @@ (read mem r) - 1 in
                let i_   = i_ptr + 1 in
                  execute i_ code mem_
            | Cpy (Register a, Register b) ->
                let mem_ = write mem b @@ (read mem a) in
                let i_   = i_ptr + 1 in
                  execute i_ code mem_
            | Cpy (Value a, Register b) ->
                let mem_ = write mem b a in
                let i_   = i_ptr + 1 in
                  execute i_ code mem_
            | Jnz (Register a, Value b) ->
                if (read mem a) = 0
                   then execute (i_ptr + 1) code mem
                   else execute (i_ptr + b) code mem
            | Jnz (Value 0, _)       -> execute (i_ptr + 1) code mem
            | Jnz (Value _, Value b) -> execute (i_ptr + b) code mem
            | _ -> raise (Util.Generic "No I!")

let silver : int  = (execute 0 prog regs).a
let gold : int    = (execute 0 prog {regs with c = 1}).a

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver 
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
