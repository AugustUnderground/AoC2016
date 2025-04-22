type device    = IC | RTG
type element   = Hydrogen | Lithium | Thulium | Plutonium | Strontium | Promethium | Ruthenium
type component = device * element

let elem (e : string) = match e with | "hydrogen"   -> Hydrogen
                                     | "lithium"    -> Lithium
                                     | "thulium"    -> Thulium
                                     | "plutonium"  -> Plutonium
                                     | "strontium"  -> Strontium
                                     | "promethium" -> Promethium
                                     | "ruthenium"  -> Ruthenium
                                     | _            -> raise (Util.Generic "No E!")

let input = CCString.lines @@ Util.slurp "./rsc/day11.txt"

let parse (line : string) (rex : Str.regexp) (dev : device) =
  let rec look pos devs =
    try
      let _ = Str.search_forward rex line pos in
      let e = Str.matched_group 1 line in
      look (Str.match_end ()) ((dev, elem e) :: devs)
    with Not_found -> List.rev devs
  in
  look 0 []

let rex_rtg = Str.regexp "\\([a-z]+\\) generator"
let rex_ic  = Str.regexp "\\([a-z]+\\)-compatible microchip"

let num_elems = List.take 3
              @@ List.map
                    (fun l -> List.length (parse l rex_rtg RTG @ parse l rex_ic IC)
                    ) input

let num_moves (ne : int list) = List.fold_left (fun a n -> a + (2 * (n - 1) - 1)) 0
                              @@ Util.accum ne

let silver : int  = num_moves num_elems
let gold : int    = num_moves @@ List.map2 (+) [4;0;0] num_elems

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver 
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
