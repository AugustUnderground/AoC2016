let input = List.drop 2 @@ CCString.lines @@ Util.slurp "./rsc/day22.txt"

let rex = Str.regexp "/dev/grid/node-x\\([0-9]+\\)-y\\([0-9]+\\)[ \n\r\x0c\t]+\\([0-9]+\\)T[ \n\r\x0c\t]+\\([0-9]+\\)T[ \n\r\x0c\t]+\\([0-9]+\\)T[ \n\r\x0c\t]+\\([0-9]+\\)%"

type coord = (int * int)
type disk = { size:    int
            ; used:    int
            ; avail:   int
            ; usep:    int
            ; g_score: int
            ; f_score: int
            ; parent:  (coord * disk) option }
type node  = coord * disk

module Nodes =
  struct
    type t = node
    let compare ((x0,y0),_) ((x1,y1),_) =
      match (Stdlib.compare x0 x1) with
        | 0 -> Stdlib.compare y0 y1
        | c -> c
  end

module NodeSet = Set.Make(Nodes)

let parse (line : string) =
  let _  = Str.search_forward rex line 0 in
  let ex = Fun.compose Stdlib.int_of_string (Fun.flip Str.matched_group line)
   in match (List.map ex @@ Util.range 1 6) with
        | [x; y; s; u; a; p] -> let dev = { size    = s
                                          ; used    = u
                                          ; avail   = a
                                          ; usep    = p
                                          ; g_score = 0
                                          ; f_score = 0 
                                          ; parent  = None }
                                 in ( ((x, y), dev) : node )
        | _                  -> raise (Util.Generic "No M!")

let (target, grid) : node * (node list) = 
  let nodes = List.map parse input in
  let max_x = Util.maximum @@ List.map (Fun.compose fst fst) nodes in
  let max_y = Util.maximum @@ List.map (Fun.compose snd fst) nodes in
  ( List.hd @@ List.filter (fun ((x,y),_) -> (x = (max_x - 1)) && (y = 0)) nodes
  , List.map (fun (c,n) -> (c, {n with f_score = (max_x + 1) * (max_y + 1)})) nodes)

let viable (((x0,y0),n0), ((x1,y1),n1)) = ((x0 != x1) || (y0 != y1))
                                        && (n0.used > 0) && (n0.used <= n1.avail)

let cmp (_,d0) (_,d1) = Stdlib.compare (d0.g_score + d0.f_score)
                                       (d1.g_score + d1.f_score)

let neighbors (ns : NodeSet.t) (n : node) = match n with
  | ((x,y),_) -> let a = [(x, y + 1); (x, y - 1); (x - 1, y); (x + 1, y)]
                  in NodeSet.filter (fun (c,_) -> List.mem c a)
                                    @@ NodeSet.remove n ns

let distance (((x0,y0),_) : node) (((x1,y1),_) : node) =
  (abs (x0 - x1)) + (abs (y0 - y1))

let rec path ((c,n) : node) = match (n.parent) with
  | None   -> [(c,n)]
  | Some p -> ((c,n) :: path p)

let rec a_star (goal : node) (cv : int) (grid : NodeSet.t)
               (open_set : NodeSet.t) (closed_set : NodeSet.t) =
  match (NodeSet.is_empty open_set, (snd goal).parent) with
    | (true,  Some _) -> path goal
    | (true,  None  ) -> raise (Util.Generic "No P!")
    | (false, _     ) -> 
        let current     = List.hd @@ List.sort cmp @@ NodeSet.to_list open_set in
        let closed_set_ = NodeSet.add current closed_set in
          if ((fst current) = (fst goal))
             then let (gc,gn) = goal in
                  let (_ ,cn) = current in
                  let goal_   = (gc, {gn with parent = cn.parent})
                   in a_star goal_ cv grid NodeSet.empty closed_set_
             else 
                  let t_score n       = (match n with | (cn,nn) ->
                        (let t_gscore = (snd current).g_score + (distance current n)
                          in if (not @@ NodeSet.mem n open_set) || (t_gscore < nn.g_score)
                                then Some (cn, { nn with parent  = Some current
                                               ;         g_score = t_gscore
                                               ;         f_score = t_gscore + (distance n goal)})
                                else None)) in
                  let nexts           = NodeSet.filter_map t_score
                                      @@ NodeSet.filter (fun (_,n) -> n.used <= cv)
                                      @@ NodeSet.diff (neighbors grid current) closed_set_ in
                  let open_set_       = NodeSet.union nexts @@ NodeSet.remove current open_set in
                    a_star goal cv grid open_set_ closed_set_

let hole = List.hd @@ List.rev
         @@ List.sort (fun (_,g0) (_,g1) -> Stdlib.compare g0.avail g1.avail) grid

let shortest_path = List.length
                  @@ a_star target ((snd hole).avail) (NodeSet.of_list grid)
                                   (NodeSet.singleton hole) NodeSet.empty

let silver : int = List.length @@ List.filter viable
                 @@ List.concat_map (fun x -> List.map (fun y -> (x, y)) grid) grid

let gold : int   = shortest_path - 1 + 5 * (fst @@ fst target) + 1

let solution : string = "\tSilver:\t" ^ (Stdlib.string_of_int silver)
                      ^ "\n\tGold:\t" ^ (Stdlib.string_of_int gold)
