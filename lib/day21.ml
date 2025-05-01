let input = CCString.lines @@ Util.slurp "./rsc/day21.txt"

let plain : char list = CCString.to_list "abcdefgh"
let hash : char list  = CCString.to_list "fbgdceah"

let rec scramble (pw : char list) (inst : string) = match inst with
  | ins when (String.starts_with ~prefix:"swap position" ins) ->
      let r = Str.regexp "swap position \\([0-9]\\) with position \\([0-9]\\)" in
      let _ = Str.search_forward r ins 0 in
      let x = Stdlib.int_of_string @@ Str.matched_group 1 ins in
      let y = Stdlib.int_of_string @@ Str.matched_group 2 ins in
      let i = min x y in
      let j = max x y in
      let a = List.nth pw i in
      let b = List.nth pw j in
      let u = List.take i pw in
      let v = List.take (j - i - 1) @@ List.drop (i + 1) pw in
      let w = List.drop (j + 1) pw in
        u @ [b] @ v @ [a] @ w
  | ins when (String.starts_with ~prefix:"swap letter" ins) ->
      let r = Str.regexp "swap letter \\([a-z]\\) with letter \\([a-z]\\)" in
      let _ = Str.search_forward r ins 0 in
      let a = List.hd @@ CCString.to_list @@ Str.matched_group 1 ins in
      let b = List.hd @@ CCString.to_list @@ Str.matched_group 2 ins in
        (match (List.find_index (fun c -> c = a) pw, List.find_index (fun c -> c = b) pw) with
          | (Some x, Some y) -> scramble pw @@ "swap position " ^ (Stdlib.string_of_int x)
                                            ^ " with position " ^ (Stdlib.string_of_int y)
          | _                -> raise (Util.Generic "No M!"))
  | ins when (String.starts_with ~prefix:"rotate based" ins) ->
      let r = Str.regexp "rotate based on position of letter \\([a-z]\\)" in
      let _ = Str.search_forward r ins 0 in
      let x = List.hd @@ CCString.to_list @@ Str.matched_group 1 ins in
        (match (List.find_index (fun c -> c = x) pw) with
          | Some a when a >= 4 -> scramble pw @@ "rotate right "
                                             ^ (Stdlib.string_of_int (a + 2))
                                             ^ " steps"
          | Some a             -> scramble pw @@ "rotate right "
                                             ^ (Stdlib.string_of_int (a + 1))
                                             ^ " steps"
          | _                -> raise (Util.Generic "No M!"))
  | ins when (String.starts_with ~prefix:"rotate" ins) ->
      let l = List.length pw in
      let r = Str.regexp "rotate \\([a-z]+\\) \\([0-9]+\\) step[s]?" in
      let _ = Str.search_forward r ins 0 in
      let d = Str.matched_group 1 ins in
      let x = Fun.flip (mod) l @@ Stdlib.int_of_string @@ Str.matched_group 2 ins in
      let m = if d = "left" then x else (l - x) in
        (List.drop m pw) @ (List.take m pw)
  | ins when (String.starts_with ~prefix:"reverse" ins) ->
      let r = Str.regexp "reverse positions \\([0-9]\\) through \\([0-9]\\)" in
      let _ = Str.search_forward r ins 0 in
      let x = Stdlib.int_of_string @@ Str.matched_group 1 ins in
      let y = Stdlib.int_of_string @@ Str.matched_group 2 ins in
      let u = List.take x pw in
      let v = List.rev @@ List.take (y - x + 1) @@ List.drop x pw in
      let w = List.drop (y + 1) pw in
        u @ v @ w
  | ins when (String.starts_with ~prefix:"move" ins) ->
      let r = Str.regexp "move position \\([0-9]\\) to position \\([0-9]\\)" in
      let _ = Str.search_forward r ins 0 in
      let x = Stdlib.int_of_string @@ Str.matched_group 1 ins in
      let y = Stdlib.int_of_string @@ Str.matched_group 2 ins in
      let c = List.nth pw x in
      let u = List.take x pw @ (List.drop (x + 1) pw) in
        (List.take y u) @ [c] @ (List.drop y u)
  | _ -> raise (Util.Generic ("No I: " ^ inst))


let unscramble (pw : char list) (inst : string) = 
  let i = match inst with
    | ins when (String.starts_with ~prefix:"swap position" ins) ->
        let r = Str.regexp "swap position \\([0-9]\\) with position \\([0-9]\\)" in
        let _ = Str.search_forward r ins 0 in
        let x = Str.matched_group 1 ins in
        let y = Str.matched_group 2 ins in
          "swap position " ^ y ^ " with position " ^ x
    | ins when (String.starts_with ~prefix:"swap letter" ins) ->
        let r = Str.regexp "swap letter \\([a-z]\\) with letter \\([a-z]\\)" in
        let _ = Str.search_forward r ins 0 in
        let a = Str.matched_group 1 ins in
        let b = Str.matched_group 2 ins in
        "swap letter " ^ b ^ " with letter " ^ a
    | ins when (String.starts_with ~prefix:"rotate based" ins) ->
        let r = Str.regexp "rotate based on position of letter \\([a-z]\\)" in
        let _ = Str.search_forward r ins 0 in
        let x = List.hd @@ CCString.to_list @@ Str.matched_group 1 ins in
          (match (List.find_index (fun c -> c = x) pw) with
            | Some j ->
                let l = List.length pw in
                let rec op i = match (if i >= 4 then i * 2 + 2 else i * 2 + 1)
                          with | p when ((p mod l) = j) -> i
                               | _                      -> op (i - 1) in
                let i = op (l - 1) in
                let s = if j > i then "left "  ^ (Stdlib.string_of_int (j - i))
                                 else "right " ^ (Stdlib.string_of_int (i - j))
                 in "rotate " ^ s ^ " steps"
            | _      -> raise (Util.Generic "No M!"))
    | ins when (String.starts_with ~prefix:"rotate" ins) ->
        let l = List.length pw in
        let r = Str.regexp "rotate \\([a-z]+\\) \\([0-9]+\\) step[s]?" in
        let _ = Str.search_forward r ins 0 in
        let d = Str.matched_group 1 ins in
        let x = Stdlib.string_of_int @@ Fun.flip (mod) l @@ Stdlib.int_of_string
              @@ Str.matched_group 2 ins in
          if d = "left"
             then ("rotate right " ^ x ^ " steps")
             else ("rotate left " ^ x ^ " steps")
    | ins when (String.starts_with ~prefix:"reverse" ins) -> ins
    | ins when (String.starts_with ~prefix:"move" ins) ->
        let r = Str.regexp "move position \\([0-9]\\) to position \\([0-9]\\)" in
        let _ = Str.search_forward r ins 0 in
        let x = Str.matched_group 1 ins in
        let y = Str.matched_group 2 ins in
          "move position " ^ y ^ " to position " ^ x
    | _ -> raise (Util.Generic ("No I: " ^ inst))
   in scramble pw i

let silver : string = CCString.of_list @@ List.fold_left scramble plain input
let gold : string   = CCString.of_list @@ List.fold_left unscramble hash
                    @@ List.rev input

let solution : string = "\tSilver:\t" ^ silver
                      ^ "\n\tGold:\t" ^ gold
