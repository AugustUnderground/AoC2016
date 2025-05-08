let input = List.map CCString.to_list @@ CCString.lines @@ Util.slurp "./rsc/day24.txt"

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

let raw           = List.flatten @@ List.mapi (fun ri r -> List.mapi (fun ci c -> ((ri,ci), c)) r) input
let grid          = CoordSet.of_list
                  @@ List.filter_map (fun (x,c) -> if (c != '#') then Some x else None) raw
let ft ((x,y), c) = if (c != '.') && (c != '#') then Some ((x,y),c) else None
let targets       = List.filter_map ft raw

let cross = function | (x,y) ->
  let d = [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
   in CoordSet.inter grid @@ CoordSet.of_list d

let expand (front : CoordSet.t) =
  List.fold_left (fun f c -> CoordSet.union f @@ cross c) CoordSet.empty
    @@ CoordSet.elements front

let rec wave (tgts : CoordSet.t) (hits : (coord * int) list) (step : int)
             (seen : CoordSet.t) (front : CoordSet.t) =
  match (CoordSet.is_empty tgts || CoordSet.is_empty front) with
    | true  -> hits
    | false -> let found  = CoordSet.inter tgts front in
               let hits_  = hits @ List.map (fun c -> (c,step))
                          @@ CoordSet.to_list @@ found in
               let seen_  = CoordSet.union front seen in 
               let front_ = CoordSet.diff (expand front) seen_ in
               let tgts_  = CoordSet.diff tgts found in
               let step_  = step + 1 in
                 wave tgts_ hits_ step_ seen_ front_

let rec uniq (r : ((char * char) * int) list) (s : ((char * char) * int) list) =
  match r with |    []     -> s
               | (((a,b),x) :: ls) -> if (List.mem_assoc (a,b) s) || (List.mem_assoc (b,a) s)
                                     then uniq ls s
                                     else uniq ls (((a,b),x) :: s);;

let rec walk (tgts : (coord * char) list) = match tgts with
  |             []        -> []
  | ((start, c) :: tgts_) ->
      let targs = CoordSet.of_list @@ List.map fst @@ List.remove_assoc start targets in
      let cost = List.map (fun (g,s) -> ((c, List.assoc g targets),s))
               @@ wave targs [] 0 CoordSet.empty @@ CoordSet.singleton start in
        cost :: walk tgts_

let graph = List.flatten @@ walk targets
let nodes = List.tl @@ List.sort_uniq Stdlib.compare
          @@ List.map (Fun.compose fst fst) graph

let perms        = List.map (fun l -> '0' :: l) @@ Util.permutations nodes
let perms_return = List.map (fun l -> '0' :: l @ ['0'])
                 @@ Util.permutations nodes

let rec traverse = function
  | (a :: b :: ns) -> let cost = List.assoc (a,b) graph
                      in cost + traverse (b :: ns)
  |         _      -> 0

let silver : int = Util.minimum @@ List.map traverse perms
let gold : int   = Util.minimum @@ List.map traverse perms_return

let solution : string = "\tSilver:\t" ^ (Stdlib.string_of_int silver)
                      ^ "\n\tGold:\t" ^ (Stdlib.string_of_int gold)
