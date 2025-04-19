let input = Util.transpose @@ List.map CCString.to_list @@ CCString.lines
          @@ Util.slurp "./rsc/day06.txt"

let rec count (msg : char list) (occ : (char * int) list) = match msg with
  |    []     -> occ
  | (c :: cs) ->
      if (List.mem_assoc c occ)
         then count cs ((c, (List.assoc c occ) + 1) :: (List.remove_assoc c occ))
         else count cs ((c, 1) :: occ)

let cmp ((c0 : char), (n0 : int)) ((c1 : char), (n1 : int)) =
  match Stdlib.compare n1 n0 with
    | 0 -> Stdlib.compare c0 c1
    | c -> c

let correct (col : char list) = fst @@ List.hd @@ List.sort cmp
                              @@ count col []

let actual (col : char list)  = fst @@ List.hd @@ List.rev @@ List.sort cmp
                              @@ count col []

let silver : string = CCString.of_list @@ List.map correct input
let gold : string   = CCString.of_list @@ List.map actual input

let solution : string = "\tSilver:\t" ^ silver
                      ^ "\n\tGold:\t" ^ gold
