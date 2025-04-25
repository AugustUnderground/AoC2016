let input : string = CCString.trim @@ Util.slurp "./rsc/day14.txt"

let md5 (str : string) = Digest.to_hex @@ Digest.string str

module Hash =
  struct
    type t = int * char
    let compare (i0,h0) (i1,h1) =
      match Stdlib.compare i0 i1 with
        | 0 -> Stdlib.compare h0 h1
        | c -> c
  end

module HashSet = Set.Make(Hash)

let has_triple (s : HashSet.t) (c : char) =
  let s_ = HashSet.filter (fun (_,c_) -> c_ = c) s
   in if HashSet.cardinal s_ > 0
         then let e = List.hd @@ List.sort Stdlib.compare @@ HashSet.to_list s_
               in Some e
         else None

let rec search (s : HashSet.t) (t : HashSet.t) (i : int) (h : int) =
  let i_ = i + 1 in
  let s_ = HashSet.filter (fun (x,_) -> (i - x) <= 1000) s
   in match (HashSet.cardinal t) with
       | n when n > 63 -> List.hd @@ List.drop 63 @@ List.map fst @@ HashSet.to_list t
       | _             -> let hash = CCString.to_list
                                   @@ List.fold_left (fun y _ -> md5 y)
                                                     (input ^ Stdlib.string_of_int i)
                                   @@ Util.range 0 h
                           in match (Util.find_rep 5 hash) with
                                | Some c -> let r = HashSet.filter (fun (_,c_) -> c_ = c) s_
                                             in search (HashSet.diff s_ r) (HashSet.union t r) i_ h
                                | None   -> match (Util.find_rep 3 hash) with
                                               | None   -> search s_ t i_ h
                                               | Some g -> search (HashSet.add (i,g) s_) t i_ h

let silver : int = search HashSet.empty HashSet.empty 0 0
let gold : int   = search HashSet.empty HashSet.empty 0 2016

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
