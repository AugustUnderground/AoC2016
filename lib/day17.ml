let input = CCString.trim @@ Util.slurp "./rsc/day17.txt"

let md5 (str : string) = Digest.to_hex @@ Digest.string str

type state = (int * int) * char list

let len (s0 : state) (s1 : state) = match s0, s1 with
  | (_,p0), (_,p1) -> Stdlib.compare (List.length p0) (List.length p1)

let is_open = function | 'b' | 'c' | 'd' | 'e' | 'f' -> true
                       | _ -> false

let cross (p : char list) (c : int * int) = match c with
  | (x,y) -> let os = List.map is_open @@ List.take 4
                    @@ CCString.to_list @@ md5
                    @@ input ^ CCString.of_list p in
             let cs = List.combine os
                    [ ('U', (x, y - 1))
                    ; ('D', (x, y + 1))
                    ; ('L', (x - 1, y))
                    ; ('R', (x + 1, y)) ]
              in List.map snd @@
                 List.filter (fun (o ,(_,(x_,y_))) -> o && x_ >= 0
                                                        && y_ >= 0
                                                        && x_ < 4
                                                        && y_ < 4) cs

let open_doors (p : char list) = List.map is_open @@ List.take 4
                               @@ CCString.to_list @@ md5
                               @@ input ^ CCString.of_list p

let rec walk (states : state list) = match states with
  | (((3,3),p) ::  _) -> CCString.of_list p
  | ((  c  ,p) :: ss) -> let os = List.map (fun (d_,c_) -> (c_, p @ [d_]))
                                @@ cross p c
                          in walk @@ List.sort len @@ ss @ os
  |    []     -> raise (Util.Generic "No S!")

let rec walk_ (states : state list) (longest : int) = match states with
  |    []             -> longest
  | (((3,3),p) :: ss) -> walk_ ss @@ max longest @@ List.length p
  | ((  c  ,p) :: ss) -> let os = List.map (fun (d_,c_) -> (c_, p @ [d_]))
                                @@ cross p c
                          in walk_ (List.sort len @@ ss @ os) longest 

let silver : string = walk  [((0,0),[])]
let gold : int      = walk_ [((0,0),[])] 0

let solution : string = "\tSilver:\t" ^ silver
                      ^ "\n\tGold:\t" ^ (Stdlib.string_of_int gold)
