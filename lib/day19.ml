let input = Stdlib.int_of_string @@ CCString.trim @@ Util.slurp "./rsc/day19.txt"

let len = List.length @@ Util.to_bits input
let msk = (Int.shift_left 1 len) - 1

let white_elephant (n_ : int) = 
  let n = Float.of_int n_ in
  let l = Float.floor @@ Util.logb 3.0 n in
  let k = n -. (3.0**l) in
    Float.to_int @@ match (l, k) with 
      | (_  , 0.0)                     -> n
      | (1.0, _  )                     -> k 
      | (_  , k_ ) when k_ <= (3.0**l) -> k
      |  _                             -> (3.0**l) +. 2.0 *. (k -. 3.0**l)

let silver : int = (Int.logand (Int.shift_left input 1) msk)
                 + (Int.shift_right input (len - 1))
let gold : int   = white_elephant input

let solution : string = "\tSilver:\t" ^ (Stdlib.string_of_int silver)
                      ^ "\n\tGold:\t" ^ (Stdlib.string_of_int gold)
