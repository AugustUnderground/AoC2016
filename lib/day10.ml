let input = CCString.lines @@ Util.slurp "./rsc/day10.txt"

let add_bot (bots : (int * int list) list) (bot : (int * int list)) = 
  match List.mem_assoc (fst bot) bots with 
    | false -> bot :: bots
    | true  -> let vals = List.assoc (fst bot) bots
    in (fst bot, snd bot @ vals) :: (List.remove_assoc (fst bot) bots)

let add_out (outs : (int * int list) list) (out : (int * int)) = 
  match List.mem_assoc (fst out) outs with 
    | false -> (fst out, [snd out]) :: outs
    | true  -> let vals = List.assoc (fst out) outs
    in (fst out, snd out :: vals) :: (List.remove_assoc (fst out) outs)

let init (line : string) =
  let rex   = Str.regexp "value \\([0-9]+\\) goes to bot \\([0-9]+\\)" in
  let _     = Str.string_match rex line 0 in
  let value = Stdlib.int_of_string @@ Str.matched_group 1 line in
  let bot   = Stdlib.int_of_string @@ Str.matched_group 2 line in
  (bot, [value])

let bots = List.fold_left add_bot [] @@ List.map init
         @@ List.filter (fun s -> String.starts_with ~prefix:"value" s) input

let inst (line : string) =
  let rex   = Str.regexp "bot \\([0-9]+\\) gives low to \\(bot\\|output\\) \\([0-9]+\\) and high to \\(bot\\|output\\) \\([0-9]+\\)" in
  let _     = Str.string_match rex line 0 in
  let bot   = Stdlib.int_of_string @@ Str.matched_group 1 line in
  let lo   = ( Str.matched_group 2 line
             , Stdlib.int_of_string @@ Str.matched_group 3 line ) in
  let hi   = ( Str.matched_group 4 line
             , Stdlib.int_of_string @@ Str.matched_group 5 line ) in
  (bot, (lo, hi))

let instructions = List.map inst
                 @@ List.filter (fun s -> String.starts_with ~prefix:"bot" s) input

let perf bots outs tgt id value = match tgt with
  | "output" -> let outs_ = add_out outs (id, value) in (bots, outs_)
  | "bot"    -> let bots_ = add_bot bots (id, [value]) in (bots_, outs)
  | _        -> raise (Util.Generic "No I!")

let rec run bots2 bots1 outs instr = match bots2 with
  | ((b, [v1; v2]) :: bs) ->
      let hi                           = max v1 v2 in
      let lo                           = min v1 v2 in
      let (lo_t, lo_id), (hi_t, hi_id) = List.assoc b instr in
      let instr_                       = List.remove_assoc b instr in
      let (bots_,outs_)                = perf bots1 outs hi_t hi_id hi in
      let (bots__,outs__)              = perf bots_ outs_ lo_t lo_id lo in
      let bots1_                       = List.filter (fun (_,vs) -> List.length vs < 2) bots__ in
      let bots2_                       = bs @ List.filter (fun (_,vs) -> List.length vs > 1) bots__ in
        run bots2_ bots1_ outs__ instr_
  |                _      -> List.sort Stdlib.compare outs

let b1 = List.filter (fun (_,vs) -> List.length vs < 2) bots
let b2 = List.filter (fun (_,vs) -> List.length vs > 1) bots

let rec cmp val1 val2 bots2 bots1 outs instr = match bots2 with
  | ((b, [v1; v2]) :: bs) ->
      let hi                           = max v1 v2 in
      let lo                           = min v1 v2 in
      let (lo_t, lo_id), (hi_t, hi_id) = List.assoc b instr in
      let instr_                       = List.remove_assoc b instr in
      let (bots_,outs_)                = perf bots1 outs hi_t hi_id hi in
      let (bots__,outs__)              = perf bots_ outs_ lo_t lo_id lo in
      let bots1_                       = List.filter (fun (_,vs) -> List.length vs < 2) bots__ in
      let bots2_                       = bs @ List.filter (fun (_,vs) -> List.length vs > 1) bots__ in
      let cl (_,vs)                    = (List.sort Stdlib.compare vs)
                                       = (List.sort Stdlib.compare [val1;val2]) in
      let found                        = List.filter cl bots2 in
      if List.length found > 0
         then fst @@ List.hd found
         else cmp val1 val2 bots2_ bots1_ outs__ instr_
  |                _      -> 0

let outputs = run b2 b1 [] instructions

let silver : int  = cmp 61 17 b2 b1 [] instructions
let gold : int    = List.fold_left ( * ) 1 @@ List.map (fun id -> List.hd
                  @@ List.assoc id outputs) @@ Util.range 0 2

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver 
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
