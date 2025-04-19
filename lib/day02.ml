type dir = U | D | L | R

let mk_dir d = match d with | 'U' -> U
                            | 'D' -> D
                            | 'L' -> L
                            | 'R' -> R
                            |  _  -> raise (Util.Generic "No D!")

let input = List.map (fun l -> List.map mk_dir @@ CCString.to_list l)
          @@ CCString.lines @@ Util.slurp "./rsc/day02.txt"

let move (b : int) (d : dir) = 
  match b with | 1 -> (match d with | U -> 1 | D -> 4 | L -> 1 | R -> 2)
               | 2 -> (match d with | U -> 2 | D -> 5 | L -> 1 | R -> 3)
               | 3 -> (match d with | U -> 3 | D -> 6 | L -> 2 | R -> 3)
               | 4 -> (match d with | U -> 1 | D -> 7 | L -> 4 | R -> 5)
               | 5 -> (match d with | U -> 2 | D -> 8 | L -> 4 | R -> 6)
               | 6 -> (match d with | U -> 3 | D -> 9 | L -> 5 | R -> 6)
               | 7 -> (match d with | U -> 4 | D -> 7 | L -> 7 | R -> 8)
               | 8 -> (match d with | U -> 5 | D -> 8 | L -> 7 | R -> 9)
               | 9 -> (match d with | U -> 6 | D -> 9 | L -> 8 | R -> 9)
               | _ -> raise (Util.Generic "No B!")

let rec find_button (b : int) (ds_ : dir list) =
  match ds_ with |    []     -> b
                 | (d :: ds) -> find_button (move b d) ds

let rec find_buttons (b : int) (dss_ : dir list list) =
  match dss_ with |     []      -> []
                  | (ds :: dss) -> let b_ = (find_button b ds) 
                                    in b_ :: find_buttons b_ dss

let move_ (b : char) (d : dir) =
  match b with | '1' -> (match d with | U -> '1' | D -> '3' | L -> '1' | R -> '1')
               | '2' -> (match d with | U -> '2' | D -> '6' | L -> '2' | R -> '3')
               | '3' -> (match d with | U -> '1' | D -> '7' | L -> '2' | R -> '4')
               | '4' -> (match d with | U -> '4' | D -> '8' | L -> '3' | R -> '4')
               | '5' -> (match d with | U -> '5' | D -> '5' | L -> '5' | R -> '6')
               | '6' -> (match d with | U -> '2' | D -> 'A' | L -> '5' | R -> '7')
               | '7' -> (match d with | U -> '3' | D -> 'B' | L -> '6' | R -> '8')
               | '8' -> (match d with | U -> '4' | D -> 'C' | L -> '7' | R -> '9')
               | '9' -> (match d with | U -> '9' | D -> '9' | L -> '8' | R -> '9')
               | 'A' -> (match d with | U -> '6' | D -> 'A' | L -> 'A' | R -> 'B')
               | 'B' -> (match d with | U -> '7' | D -> 'D' | L -> 'A' | R -> 'C')
               | 'C' -> (match d with | U -> '8' | D -> 'C' | L -> 'B' | R -> 'C')
               | 'D' -> (match d with | U -> 'B' | D -> 'D' | L -> 'D' | R -> 'D')
               |  _ -> raise (Util.Generic "No B!")

let rec find_button_ (b : char) (ds_ : dir list) =
  match ds_ with |    []     -> b
                 | (d :: ds) -> find_button_ (move_ b d) ds

let rec find_buttons_ (b : char) (dss_ : dir list list) =
  match dss_ with |     []      -> []
                  | (ds :: dss) -> let b_ = (find_button_ b ds) 
                                    in b_ :: find_buttons_ b_ dss

let silver : string = List.fold_left (^) "" @@ List.map Stdlib.string_of_int
                    @@ find_buttons 5 input
let gold : string   = CCString.of_list @@ find_buttons_ '5' input

let solution : string = "\tSilver:\t" ^ silver
                      ^ "\n\tGold:\t" ^ gold
