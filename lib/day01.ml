type dir  = N | S | E | W
type turn = R | L
type move = turn * int
type pos  = dir * (int * int)

let rec parse (line : char list) =
  match line with | [] -> []
                  | l -> let mov = List.take_while (fun c -> c <> ',') l in
                         let rst = List.drop (List.length mov + 2) l in
                          (CCString.of_list mov) :: parse rst

let as_move (seq : string) = 
  let t = match seq.[0] with | 'R' -> R | _ -> L in 
  let d = Stdlib.int_of_string @@ String.sub seq 1 (String.length seq - 1) in
    (t,d)

let input : move list = 
  let content : string = Util.slurp "./rsc/day01.txt" in
    List.map as_move @@ parse @@ CCString.to_list
                     @@ String.trim content

let init : pos = (N, (0,0))

let go (d : dir) (l : int) ((x : int), (y : int)) =
  match d with | N -> (x, y + l)
               | S -> (x, y - l)
               | W -> (x - l, y)
               | E -> (x + l, y)

let move ((d, (x, y)) : pos) ((t, l) : move) = 
  let d_ = match d with | N -> (match t with | R -> W | L -> E)
                        | S -> (match t with | R -> E | L -> W)
                        | W -> (match t with | R -> S | L -> N)
                        | E -> (match t with | R -> N | L -> S) in
  let (x_, y_) = go d_ l (x, y) in
    (d_, (x_, y_))

module Coords =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        | 0 -> Stdlib.compare y0 y1
        | c -> c
  end

module CoordSet = Set.Make(Coords)

let path = CoordSet.(empty |> add (0,0))

let trace (d : dir) (l : int) ((x : int), (y : int)) = CoordSet.of_list @@
  match d with | N -> List.map (fun y_ -> (x, y + y_)) @@ Util.range 1 @@ l - 1
               | S -> List.map (fun y_ -> (x, y - y_)) @@ Util.range 1 @@ l - 1
               | W -> List.map (fun x_ -> (x - x_, y)) @@ Util.range 1 @@ l - 1
               | E -> List.map (fun x_ -> (x + x_, y)) @@ Util.range 1 @@ l - 1

let move_ ((d, (x, y)) : pos) ((t, l) : move) = 
  let (d_,(x_,y_)) = move (d, (x,y)) (t,l) in
  let ms           = trace d_ l (x, y) in
    (d_, (x_,y_), ms)

let rec walk (p : pos) (ms : move list) (s : CoordSet.t) = 
  match ms with | []         -> (0,0)
                | (m :: ms_) -> 
                    let (d_, (x_,y_), s_) = move_ p m in
                      if CoordSet.disjoint s s_ 
                        then walk (d_, (x_,y_)) ms_ @@ CoordSet.union s s_
                        else List.hd @@ CoordSet.to_list @@ CoordSet.inter s s_

let silver : int =
  let (_, ((x : int), (y : int))) = List.fold_left move init input
   in (Int.abs x) + (Int.abs y)

let gold : int   =
  let (x,y) = walk init input CoordSet.(empty |> add (0,0))
   in (Int.abs x) + (Int.abs y)

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
