type op        = int * int
type operation = Rect of op
               | Roro of op
               | Roco of op

let input : string list = CCString.lines @@ Util.slurp "./rsc/day08.txt"

let width : int  = 50
let height : int = 6

let parse (line : string) = 
  let line_ = CCString.to_list line in
  match (String.starts_with ~prefix:"rotate" line) with
    | false -> let c = Stdlib.int_of_string @@ CCString.of_list
                     @@ List.take_while (fun c -> c != 'x') @@ List.drop 1
                     @@ List.drop_while (fun c -> c != ' ') line_ in
               let r = Stdlib.int_of_string @@ CCString.of_list @@  List.drop 1
                     @@ List.drop_while (fun c -> c != 'x') line_ in
                Rect (c, r)
    | true -> let i = Stdlib.int_of_string @@ CCString.of_list
                    @@ List.take_while (fun c -> c != ' ') @@ List.drop 1
                    @@ List.drop_while (fun c -> c != '=') line_ in
              let n = Stdlib.int_of_string @@ CCString.of_list @@ List.rev
                    @@ List.take_while (fun c -> c != ' ')
                    @@ List.rev line_ in
                if (String.starts_with ~prefix:"rotate row" line)
                  then Roro (i, n) else Roco (i, n)

let rect_idxs nr nc = 
  let cs = Util.range 0 (nc - 1) in
  let rs = Util.range 0 (nr - 1) in 
    List.concat_map (fun c -> List.map (fun r -> (r, c)) rs) cs

let perform (screen : ((int * int) * int) list) (inst : operation) =
  match inst with | Rect (c, r) ->
                      let idxs = rect_idxs r c 
                       in List.map (fun i -> (i, 1)) idxs @
                          List.fold_left (Fun.flip List.remove_assoc) screen idxs
                  | Roro (r, n) ->
                      let idxs = List.map (fun c -> (r,c))
                               @@ Util.range 0 (width - 1) in
                      let shft = List.map (fun c -> (r,(c + n) mod width))
                               @@ Util.range 0 (width - 1) in
                      let row  = List.map (fun (a, b) -> (b, List.assoc a screen))
                               @@ List.combine idxs shft in
                        row @ List.fold_left (Fun.flip List.remove_assoc) screen idxs
                  | Roco (c, n) ->
                      let idxs = List.map (fun r -> (r,c))
                               @@ Util.range 0 (height - 1) in
                      let shft = List.map (fun r -> ((r + n) mod height, c))
                               @@ Util.range 0 (height - 1) in
                      let col  = List.map (fun (a, b) -> (b, List.assoc a screen))
                               @@ List.combine idxs shft in
                        col @ List.fold_left (Fun.flip List.remove_assoc) screen idxs

let insructions = List.map parse input
let pixels      = List.map (fun c -> (c, 0)) @@ rect_idxs height width

let rec rows l = match l with 
  | [] -> ""
  | _ -> let line = Str.global_replace (Str.regexp "#") "█" @@ CCString.of_list
                  @@ List.map (fun (_,p) -> if p = 1 then '#' else ' ')
                  @@ List.take width l
          in line ^ "\n\t\t" ^ (rows @@ List.drop width l)

let silver : int  = List.fold_left (+) 0 @@ List.map snd
                  @@ List.fold_left perform pixels insructions
let gold : string = rows @@ List.sort Stdlib.compare
                  @@ List.fold_left perform pixels insructions

let solution : string = "\tSilver:\t" ^ Stdlib.string_of_int silver
                      ^ "\n\tGold:\t" ^ gold
