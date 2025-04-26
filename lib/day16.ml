let input = CCString.trim @@ Util.slurp "./rsc/day16.txt"

let to_string (bs : int list) =
  List.fold_left (fun s b -> s ^ Stdlib.string_of_int b) "" bs

let rec dragon_curve (d : int) (a : int list) = match (List.length a >= d) with
  | true  -> List.take d a
  | false -> let n = d - List.length a in
             let b = List.map (fun c -> 1 - c) @@ List.take n @@ List.rev a
              in dragon_curve d @@ a @ [0] @ b

let rec pairs (s : int list) (ps : int list) = match s with
  | (a :: b :: ss) when a = b -> pairs ss @@ 1 :: ps
  | (_ :: _ :: ss)            -> pairs ss @@ 0 :: ps
  |       _                   -> List.rev ps

let rec hash (s : int list) = 
  let s_ = pairs s []
   in match (Util.is_odd @@ List.length s_) with
        | true  -> s_
        | false -> hash s_

let init_state = List.map (fun c -> Stdlib.int_of_char c - 48)
               @@ CCString.to_list input

let silver : string = to_string @@ hash @@ dragon_curve 272      init_state
let gold : string   = to_string @@ hash @@ dragon_curve 35651584 init_state

let solution : string = "\tSilver:\t" ^ silver
                      ^ "\n\tGold:\t" ^ gold
