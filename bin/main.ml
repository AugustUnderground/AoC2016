let solutions : string list =
  [ AoC2016.Day01.solution; AoC2016.Day02.solution; AoC2016.Day03.solution
  ; AoC2016.Day04.solution; AoC2016.Day05.solution; AoC2016.Day06.solution
  ; AoC2016.Day07.solution; AoC2016.Day08.solution; AoC2016.Day09.solution
  ; AoC2016.Day10.solution; AoC2016.Day11.solution; AoC2016.Day12.solution
  ; AoC2016.Day13.solution; AoC2016.Day14.solution; AoC2016.Day15.solution
  ; AoC2016.Day16.solution; AoC2016.Day17.solution; AoC2016.Day18.solution
  ; AoC2016.Day19.solution; AoC2016.Day20.solution; AoC2016.Day21.solution
  ; AoC2016.Day22.solution; AoC2016.Day23.solution; AoC2016.Day24.solution
  ; AoC2016.Day25.solution ]

let rec run (sols : string list) = match sols with
  |    [] -> let len = Stdlib.string_of_int @@ List.length solutions
              in print_endline @@ "✧･ﾟ: *✧･ﾟ:* " ^ len
                                ^ " solutions for AoC 2016 *:･ﾟ✧*:･ﾟ✧ "
  | (s :: ss) -> let len = List.length solutions in
                 let l   = List.length ss in
                 let d   = Printf.sprintf "%02d" (len - l) in
                 let ()  = print_endline @@ "Day " ^ d ^ ":\n" ^ s in
                  run ss

let () = run solutions

(* let () = print_endline AoC2016.Day25.solution *)
