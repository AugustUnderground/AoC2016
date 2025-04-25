type coord = int * int

module Coords =
  struct
    type t = coord
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        | 0 -> Stdlib.compare y0 y1
        | c -> c
  end

module CoordSet = Set.Make(Coords)

let input : string = String.trim @@ Util.slurp "./rsc/day13.txt"

let fav_num : int  = Stdlib.int_of_string input
let target : coord = (31,39)
let start : coord  = (1,1)

let free_space x y = Util.is_even @@ List.fold_left (+) 0 @@ Util.to_bits
                   @@ (x * x + 3 * x + 2 * x * y + y + y * y) + fav_num

let cross = function
  | (x,y) -> CoordSet.of_list
          @@ List.filter (fun (x_,y_) -> x_ >= 0 && y_ >= 0 && free_space x_ y_)
             [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]

let expand (front : CoordSet.t) =
  List.fold_left (fun f c -> CoordSet.union f @@ cross c) CoordSet.empty
    @@ CoordSet.elements front

let rec wave (step : int) (seen : CoordSet.t) (front : CoordSet.t) =
  match (CoordSet.mem target front) with
    | true  -> step
    | false -> let seen_  = CoordSet.union front seen in 
               let front_ = CoordSet.diff (expand front) seen_ in
               let step_  = step + 1 in
                wave step_ seen_ front_

let rec wave_ (step : int) (seen : CoordSet.t) (front : CoordSet.t) =
  match step with
    | 50 -> CoordSet.cardinal @@ CoordSet.union front seen
    | _  -> let seen_  = CoordSet.union front seen in 
               let front_ = CoordSet.diff (expand front) seen_ in
               let step_  = step + 1 in
                wave_ step_ seen_ front_

let silver : int = wave 0 CoordSet.empty @@ CoordSet.singleton start
let gold : int   = wave_ 0 CoordSet.empty @@ CoordSet.singleton start

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver 
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
