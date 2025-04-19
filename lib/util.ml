let string_of_string_list = [%show: string list]

let slurp filename =
  try In_channel.with_open_text filename In_channel.input_all
  with Sys_error msg -> failwith ("Failed to read from file: " ^ msg)

let range n m = List.map (fun x -> x + n - 1)
              @@ List.init (m - n + 1) (fun x -> x + 1)

let (>>) f g x = g (f x)

let (<<) f g x = f (g x)
