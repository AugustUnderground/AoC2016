let input : int list list = 
  let ss2is l = List.map Stdlib.int_of_string @@ Util.words l
   in List.map ss2is @@ CCString.lines @@ Util.slurp "./rsc/day03.txt"

let is_triangle (sides : int list) = match sides with
  | (a :: b :: c :: []) -> ((a + b) > c) && ((a + c) > b) && ((b + c) > a)
  | _                   -> false

let rec is_valid (sides : int list) = match sides with
  | (a :: b :: c :: ss) -> ( ((a + b) > c) && ((a + c) > b) && ((b + c) > a)
                           ) :: is_valid ss
  | _                   -> []

let silver : int = List.fold_left (fun s t -> if t then s + 1 else s) 0
                 @@ List.map is_triangle input
let gold : int   = List.fold_left (fun s t -> if t then s + 1 else s) 0
                 @@ is_valid @@ List.concat @@ Util.transpose input

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
