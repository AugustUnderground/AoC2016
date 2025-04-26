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

let rec repeat  (n : int) (l : 'a list) = match n with 
  | 0 -> []
  | n -> l @ repeat (n - 1) l

let rec combos (l : 'a list) = match l with
  | x :: xs -> (List.map (fun y -> [x; y]) xs) @ combos xs
  |   _     -> []

let singleton (x : 'a) = [x]

let update elem idx lst = 
  let pre = List.take idx lst in
  let post = List.drop (idx + 1) lst in
  pre @ [elem] @ post

let accum (l : int list) =
  let rec cum acc sum lst = match lst with
    | [] -> List.rev acc
    | x :: xs ->
        let sum' = sum + x in
        cum (sum' :: acc) sum' xs
   in cum [] 0 l

let split_on e l = 
  let a = List.take_while (fun e_ -> e_ != e) l in
  let b = List.drop 1 @@ List.drop_while (fun e_ -> e_ != e) l in
  (a,b)

let rec i2b n a = match n with 
  | 0 -> a
  | _ -> i2b (n lsr 1) @@ (n land 1) :: a

let to_bits = function | 0 -> singleton 0 | n -> i2b n []

let is_even x = x mod 2 = 0

let is_odd x = not @@ is_even x

let find_rep (n: int) (l : 'a list) =
  let rec rep_ p c r = match r with
    |   []    -> None
    | x :: xs -> let c_ = if Some x = p then c + 1 else 1
                 in if c_ >= n then Some x else rep_ (Some x) c_ xs
   in rep_ None 0 l;;

let maximum = function | (x :: xs) -> List.fold_left (fun a e -> if e > a then e else a) x xs
                       |    _      -> 0

let minimum = function | (x :: xs) -> List.fold_left (fun a e -> if e < a then e else a) x xs
                       |    _      -> 0

let rec gcd u v =
  if v <> 0
     then gcd v @@ u mod v
     else abs u

let lcm m n = match m, n with
  | 0, _ | _, 0 -> 0
  | m, n        -> abs (m * n) / (gcd m n)
