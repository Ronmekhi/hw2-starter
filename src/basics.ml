let abs x =
  if x >= 0 then x
  else (-x)

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (a, b) = (b, a)
let rev_triple (a, b, c) = (c, b, a)

let is_odd x = x mod 2 <> 0 

let is_older (y1, m1, d1) (y2, m2, d2) = (y1, m1, d1) < (y2, m2, d2)

let to_us_format (y, m, d) = (m, d, y)

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec pow x p =
  if p = 0 then 1
  else x * pow x (p - 1)

let rec fac n =
  if n = 1 then 1 
  else n * fac (n - 1)

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get_nth (idx, lst) =
  match (idx, lst) with
  | (0, h :: _) -> h
  | (n, _ :: t) when n > 0 -> get_nth (n - 1, t)
  | _ -> failwith "Index out of bounds"

let larger lst1 lst2 =
  let len1 = List.length lst1 in
  let len2 = List.length lst2 in
  if len1 >= len2 then lst1
  else lst2

let rec sum_list lst =
  match lst with
  | [] -> 0
  | h :: t -> h + sum_list t

let sum lst1 lst2 = sum_list lst1 + sum_list lst2
