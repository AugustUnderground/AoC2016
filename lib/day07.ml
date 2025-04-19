let input = List.map CCString.to_list
          @@ CCString.lines @@ Util.slurp "./rsc/day07.txt"

let rec parse (line : char list) (snss : char list list) (hnss : char list list) =
  match line with | [] -> (snss, hnss)
                  | ('[' :: cs) -> let hns = List.take_while (fun c -> c != ']') cs in
                                   let l   = List.drop 1 @@ List.drop_while (fun c -> c != ']') cs in
                                    parse l snss (hns :: hnss)
                  | _ -> let sns = List.take_while (fun c -> c != '[') line in
                         let l    = List.drop_while (fun c -> c != '[') line in
                          parse l (sns :: snss) hnss

let rec abba (str : char list) = match str with
  | (a :: b :: c :: d :: chrs) -> if (a = d) && (b = c) && (a <> b)
                                     then true
                                     else abba (b :: c :: d :: chrs)
  | _                          -> false

let rec has_abba (seqs : char list list) = match seqs with
  |    []     -> false
  | (s :: ss) -> if (abba s) then true else has_abba ss

let count_tls (line : char list) =
  let (sns, hns) = parse line [] [] in
    match (has_abba hns) with | true  -> 0 
                              | false -> if (has_abba sns) then 1 else 0

module BAB =
  struct
    type t = char * char * char
    let compare (a0,b0,c0) (a1,b1,c1) =
      match Stdlib.compare a0 a1 with
        | 0 -> (match Stdlib.compare b0 b1 with
          | 0 -> Stdlib.compare c0 c1
          | c -> c)
        | c -> c
  end

module BABSet = Set.Make(BAB)

let rec aba (str : char list) (babs : BABSet.t) = match str with
  | (a :: b :: c :: chrs) -> if (a = c) && (a <> b)
                                then aba (b :: c :: chrs) (BABSet.add (b,a,b) babs)
                                else aba (b :: c :: chrs) babs
  | _ -> babs

let rec bab (str : char list) (babs : BABSet.t) = match str with
  | (a :: b :: c :: chrs) -> if (a = c) && (a <> b)
                                then bab (b :: c :: chrs) (BABSet.add (a,b,a) babs)
                                else bab (b :: c :: chrs) babs
  | _ -> babs

let count_ssl (line : char list) =
  let (sns, hns) = parse line [] [] in
  let abas = List.fold_left BABSet.union BABSet.empty
           @@ List.map (fun s -> aba s BABSet.empty) sns in
  let babs = List.fold_left BABSet.union BABSet.empty
           @@ List.map (fun h -> bab h BABSet.empty) hns in
    if (BABSet.disjoint abas babs) then 0 else 1

let silver : int = List.fold_left (+) 0 @@ List.map count_tls input
let gold : int   = List.fold_left (+) 0 @@ List.map count_ssl input

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
