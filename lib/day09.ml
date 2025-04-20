let input : string = String.trim @@ Util.slurp "./rsc/day09.txt"

let rec decompress (com : char list) (dec : char list) = match com with
  |     []    -> List.rev dec
  | '(' :: cs -> 
      let n = Stdlib.int_of_string @@ CCString.of_list 
            @@ List.take_while (fun c -> c != 'x') cs in
      let r = Stdlib.int_of_string @@ CCString.of_list
            @@ List.take_while (fun c -> c != ')') @@ List.drop 1
            @@ List.drop_while (fun c -> c != 'x') cs in
      let q = List.rev @@ Util.repeat r @@ List.take n @@ List.drop 1
            @@ List.drop_while (fun c -> c != ')') cs in
      let p = List.drop (n + 1) @@ List.drop_while (fun c -> c != ')') cs in
      decompress p (q @ dec)
  | c :: cs -> decompress cs @@ c :: dec

let rec decompress_v2 (css : char list) =
  match css with
    | '(' :: cs -> 
        let n = Stdlib.int_of_string @@ CCString.of_list 
              @@ List.take_while (fun c -> c != 'x') cs in
        let m = Stdlib.int_of_string @@ CCString.of_list
              @@ List.take_while (fun c -> c != ')') @@ List.drop 1
              @@ List.drop_while (fun c -> c != 'x') cs in
        let q = List.take n @@ List.drop 1
              @@ List.drop_while (fun c -> c != ')') cs in
        let p = List.drop (n + 1)
              @@ List.drop_while (fun c -> c != ')') cs in
        let r = m * decompress_v2 q in
        r + decompress_v2 p
    |  _  :: cs -> 1 + decompress_v2 cs
    |     []    -> 0

let silver : int  = List.length @@ decompress (CCString.to_list input) []
let gold : int    = decompress_v2 @@ CCString.to_list input

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver 
                      ^ "\n\tGold:\t" ^ Stdlib.string_of_int gold
