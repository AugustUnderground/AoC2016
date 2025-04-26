let input = CCString.lines @@ Util.slurp "./rsc/day15.txt"

type disc = int * int * int

let rex = Str.regexp @@ "Disc #\\([0-9]+\\) has \\([0-9]+\\) positions; "
                      ^ "at time=0, it is at position \\([0-9]+\\)."

let parse (line : string) = 
  let _ = Str.search_forward rex line 0
   in ( Stdlib.int_of_string @@ Str.matched_group 1 line
      , Stdlib.int_of_string @@ Str.matched_group 2 line
      , Stdlib.int_of_string @@ Str.matched_group 3 line )

let discs = List.map parse input

let pos (t : int) (d : disc) = match d with
  | (i,n,p) -> (p + i + t) mod n

let rec find_slot (t : int) (ds : disc list) = 
  match (List.fold_left (+) 0 @@ List.map (pos t) ds) with
    | 0 -> t
    | _ -> find_slot (t + 1) ds 

let silver : int = find_slot 0 discs 
let gold : int   = find_slot 0 @@ (List.length discs + 1, 11, 0) :: discs

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
