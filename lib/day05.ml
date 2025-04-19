let input : string = String.trim @@ Util.slurp "./rsc/day05.txt" 

let md5 (str : string) = Digest.to_hex @@ Digest.string str

let rec hash (idx : int) (sum : char list) =
  if (List.length sum) = 8
    then sum
    else let md5hash = md5 @@ input ^ (Stdlib.string_of_int idx)
          in match (String.sub md5hash 0 5) with
            | "00000" -> hash (idx + 1) ((String.get md5hash 5) :: sum)
            | _       -> hash (idx + 1) sum

let rec decypher (idx : int) (sum : (int * char) list) =
  if (List.length sum) = 8 
    then sum
    else let md5hash = md5 @@ input ^ (Stdlib.string_of_int idx)
          in match (String.sub md5hash 0 5) with
            | "00000" ->
                let i = (Stdlib.int_of_char @@ String.get md5hash 5) - 48
                 in if (i < 8) && not (List.mem_assoc i sum)
                       then decypher (idx + 1) ((i, String.get md5hash 6) :: sum)
                       else decypher (idx + 1) sum
            | _       -> decypher (idx + 1) sum

let silver : string = CCString.of_list @@ List.rev @@ hash 0 []
let gold : string   = CCString.of_list @@ List.map snd
                    @@ List.sort Stdlib.compare @@ decypher 0 []

let solution : string = "\tSilver:\t" ^ silver
                      ^ "\n\tGold:\t" ^ gold
