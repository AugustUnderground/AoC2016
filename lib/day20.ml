let input = CCString.lines @@ Util.slurp "./rsc/day20.txt"

let parse (line : string) = match (String.split_on_char '-' line) with
  | a :: b :: _ -> (Stdlib.int_of_string a,Stdlib.int_of_string b)
  |           _ -> raise (Util.Generic "No R!")

let ranges = List.sort Stdlib.compare @@ List.map parse input

let rec lowest ((pa, pb) : int * int) (rs_ : (int * int) list) = match rs_ with
  | (ca, _) :: _  when not ((ca - 1) <= pb ) -> pb + 1
  | (_, cb) :: rs                            -> lowest (pa, max cb pb) rs
  | []                                       -> pb + 1

let rec num_allowed ((pa,pb) : int * int) (rs_ : (int * int) list) = match rs_ with
  | (ca, cb) :: rs when not ((ca - 1) <= pb ) -> (ca - pb - 1) + num_allowed (ca,cb) rs
  | (_,  cb) :: rs                            -> 0 + num_allowed (pa, max cb pb) rs
  | []                                        -> 0

let silver : int = lowest (List.hd ranges) @@ List.drop 1 ranges
let gold : int   = num_allowed (0,0) ranges

let solution : string = "\tSilver:\t" ^ (Stdlib.string_of_int silver)
                      ^ "\n\tGold:\t" ^ (Stdlib.string_of_int gold)
