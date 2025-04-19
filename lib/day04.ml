let input = CCString.lines @@ Util.slurp "./rsc/day04.txt"

let parse (l : char list) = 
  let checksum  = List.rev @@ List.take 5 @@ List.drop 1 l in
  let sector_id = Stdlib.int_of_string @@ CCString.of_list @@ List.rev
                @@ List.take_while (fun c -> c != '-') @@ List.drop 7 l in
  let room_name = List.rev @@ List.drop 1 
                @@ List.drop_while (fun c -> c != '-') l in
    (room_name, sector_id, checksum)

let cmp ((c0 : char), (n0 : int)) ((c1 : char), (n1 : int)) =
  match Stdlib.compare n1 n0 with
    | 0 -> Stdlib.compare c0 c1
    | c -> c

let rec count (occ : (char * int) list) (name : char list) = match name with
  | (c :: cs) -> 
    let occ_ = if List.mem_assoc c occ
                  then ((c, (List.assoc c occ) + 1) :: (List.remove_assoc c occ))
                  else ((c, 1) :: occ)
      in count occ_ cs
  |    []     -> List.sort cmp occ

let valid_room (name : char list) (check : char list) = 
  let sum = List.map (fun (c,_) -> c) @@ List.take 5 @@ count []
          @@ List.sort Stdlib.compare
          @@ List.fold_left (fun a c -> if c != '-' then c :: a else a) [] name
   in check = sum

let room_list = List.map (fun i -> parse @@ List.rev @@ CCString.to_list i) input

let sec_sum sum (room, sector, check) =
  if (valid_room room check)
     then sum + sector
     else sum

let decode (c : char) (s : int) = match c with
  | '-' -> ' '
  |  _  -> Stdlib.char_of_int @@
            (((((Stdlib.int_of_char c) - 97) + s) mod 26) + 97)

let rec decypher rooms_ = match rooms_ with
  | ((name, sector, check) :: rooms) -> 
    if (valid_room name check)
       then let real_name = CCString.of_list
                          @@ List.map (fun c -> decode c sector) name
             in (match real_name with | "northpole object storage" -> sector
                                      | _ -> decypher rooms)
       else decypher rooms
  | [] -> 0

let silver : int = List.fold_left sec_sum 0 room_list
let gold : int   = decypher room_list

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
