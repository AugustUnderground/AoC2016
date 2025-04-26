exception Generic of string
val string_of_string_list : string list -> string
val slurp : string -> string
val words : string -> string list
val range : int -> int -> int list
val transpose : 'a list list  -> 'a list list
val repeat : int -> 'a list -> 'a list
val combos : 'a list -> 'a list list
val singleton : 'a -> 'a list
val update : 'a -> int -> 'a list -> 'a list
val accum : int list -> int list
val split_on : 'a -> 'a list -> 'a list * 'a list
val to_bits : int -> int list
val is_even : int -> bool
val is_odd : int -> bool
val find_rep : int -> 'a list -> 'a option
val maximum : int list -> int
val minimum : int list -> int
val gcd : int -> int -> int
val lcm : int -> int -> int
