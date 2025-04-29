let input = CCString.to_list @@ CCString.trim @@ Util.slurp "./rsc/day18.txt"

let rec scan (line : char list) = 
  let line_ = List.drop 1 line
   in match line with
      | ('^' :: '^' :: '.' :: _)        -> '^' :: scan line_
      | ('.' :: '^' :: '^' :: _)        -> '^' :: scan line_
      | ('.' :: '.' :: '^' :: _)        -> '^' :: scan line_
      | ('^' :: '.' :: '.' :: _)        -> '^' :: scan line_
      | (_ :: _ :: []) | (_ :: []) | [] -> []
      |             _                   -> '.' :: scan line_

let n_safe (row : char list) = List.length @@ List.filter (fun t -> t = '.') @@ row

let rec safe_tiles (count : int) (row : char list) = 
  let n = n_safe row in
  let r = List.rev @@ scan @@ '.' :: row @ ['.'] in
  match count with | 1 -> n
                   | _ -> n + safe_tiles (count - 1) r

let silver : int = safe_tiles     40 input
let gold : int   = safe_tiles 400000 input

let solution : string = "\tSilver:\t" ^ (Stdlib.string_of_int silver)
                      ^ "\n\tGold:\t" ^ (Stdlib.string_of_int gold)
