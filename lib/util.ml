exception Generic of string

let string_of_string_list = [%show: string list]

let slurp filename =
  try In_channel.with_open_text filename In_channel.input_all
  with Sys_error msg -> failwith ("Failed to read from file: " ^ msg)

let words l = Str.split (Str.regexp "[ \n\r\x0c\t]+") l

let range n m = List.map (fun x -> x + n - 1)
              @@ List.init (m - n + 1) (fun x -> x + 1)

let rec transpose lst = match lst with
  |    []            -> []
  |    []     :: xss -> transpose xss
  | (x :: xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)



(*
let (>>) f g x = g (f x)
let (<<) f g x = f (g x)
*)
