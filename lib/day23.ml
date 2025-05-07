type arg  = Register of char
          | Value of int
type inst = Cpy of arg * arg
          | Jnz of arg * arg
          | Inc of arg
          | Dec of arg
          | Tgl of arg

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

let input = CCString.lines @@ Util.slurp "./rsc/day23.txt"

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
      | "tgl" -> Tgl (to_arg @@ CCString.of_list args)
      | _     -> raise (Util.Generic "No I!")

let prog = List.map parse input
let regs = {a = 0; b = 0; c = 0; d = 0}

let toggle_inst = function | (Inc a)      -> Dec a
                           | (Dec a)      -> Inc a
                           | (Tgl a)      -> Inc a
                           | (Jnz (a, b)) -> Cpy (a, b)
                           | (Cpy (a, b)) -> Jnz (a, b)

let toggle (p : inst list) (i : int) = 
  if i < (List.length p)
     then let i_ = toggle_inst @@ List.nth p i
           in Util.update i_ i p
     else p

let mul_pattern =  [ Cpy (Register 'b', Register 'c') 
                   ; Jnz (Register 'd', Value (-5))
                   ; Dec (Register 'd') ]

let add_pattern =  [ Inc (Register 'a')
                   ; Dec (Register 'c')
                   ; Jnz (Register 'c', Value (-2)) ]

let rec execute (i_ptr : int) (code : inst list) (mem : state) (stack : inst list) = 
  if (i_ptr >= List.length code) || (i_ptr < 0)
    then mem
    else let ins         = List.nth code i_ptr in
         let len         = List.length code in
         let look_ahead  = List.take 3 @@ List.drop i_ptr code in
         let look_behind = List.take 3 @@ ins :: stack in
          match ins with
            | _ when (look_ahead = add_pattern) && ((i_ptr + 3) < len) ->
                let c      = read mem 'c' in
                let a      = read mem 'a' in
                let mem_   = write (write mem 'c' 0) 'a' (a + c) in
                let i_     = i_ptr + 3 in
                  execute i_ code mem_ stack
            | _ when (look_behind = mul_pattern) ->
                let b = read mem 'b' in
                let d = read mem 'd' in
                let mem_ = write (write (write mem 'd' 0) 'c' 0) 'a' (b * (d + 1)) in
                let i_   = i_ptr + 6 in
                  execute i_ code mem_ look_behind
            | Inc (Register r) -> 
                let mem_ = write mem r @@ (read mem r) + 1 in
                let i_   = i_ptr + 1 in
                  execute i_ code mem_ look_behind
            | Dec (Register r) ->
                let mem_ = write mem r @@ (read mem r) - 1 in
                let i_   = i_ptr + 1 in
                  execute i_ code mem_ look_behind
            | Cpy (Register a, Register b) ->
                let mem_ = write mem b @@ (read mem a) in
                let i_   = i_ptr + 1 in
                  execute i_ code mem_ look_behind
            | Cpy (Value a, Register b) ->
                let mem_ = write mem b a in
                let i_   = i_ptr + 1 in
                  execute i_ code mem_ look_behind
            | Jnz (Register a, Value b) ->
                if (read mem a) = 0
                   then execute (i_ptr + 1) code mem look_behind
                   else execute (i_ptr + b) code mem look_behind
            | Jnz (Value 0, _)       -> execute (i_ptr + 1) code mem look_behind
            | Jnz (Value _, Value b) -> execute (i_ptr + b) code mem look_behind
            | Jnz (Value _, Register r) -> 
                let b = read mem r in
                  execute (i_ptr + b) code mem look_behind
            | Tgl (Register r) -> 
                let a     = read mem r in
                let i_    = i_ptr + 1 in
                let code_ = toggle code @@ i_ptr + a in
                  execute i_ code_ mem look_behind
            | Tgl (Value a) -> 
                let i_    = i_ptr + 1 in
                let code_ = toggle code @@ i_ptr + a in
                  execute i_ code_ mem look_behind
            | _ -> execute (i_ptr + 1) code mem look_behind

let silver : int = (execute 0 prog {regs with a =  7} []).a
let gold : int   = (execute 0 prog {regs with a = 12} []).a

let solution : string = "\tSilver:\t" ^ (Stdlib.string_of_int silver)
                      ^ "\n\tGold:\t" ^ (Stdlib.string_of_int gold)
