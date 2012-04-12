type 'a t = 'a list

let length = List.length

let empty () = []

let cons (x:'a) (s:'a t) = x::s

let singleton x = [x]

let append = List.append

let tabulate f n =
  let rec helper acc x =
    if x = n then List.rev acc
    else helper ((f x)::acc) (x+1) in
  helper [] 0

let nth = List.nth

let filter = List.filter

let map = List.map

let reduce = List.fold_left

let map_reduce l e n s = reduce n e (map l s)

let repeat x n =
  let rec helper x n acc =
    if n = 0 then acc else helper x (n-1) (x::acc) in
  helper x n []

let flatten = List.flatten

let zip (s1,s2) = List.combine s1 s2

let split s i =
  let rec helper s i acc =
    match s,i with
      | _,0 -> (List.rev acc,s)
      | [],_ -> failwith "Split index out of bounds"
      | h::t,_ -> helper t (i-1) (h::acc) in
  helper s i []
