type 'a t

(* length s returns the number of elements in the segment  *)
val length : 'a t -> int

(* empty returns a sequence of length 0 *)
val empty : unit  -> 'a t

(* cons x xs
 * if the length of xs is l then cons evaluates to a sequence of length l+1
 * where the first element is x and the remaining l items are exactly the same
 * as the sequence xs *)
val cons : 'a -> 'a t -> 'a t

(* singleton x
 * evaluates to a sequence of length 1 whose only item is x *)
val singleton : 'a -> 'a t

(* seq s1 s2
 * if s1 is a sequence of length l1 and s2 is a sequence of length s2 then
 * append s1 s2 appends to a sequence of length l1+l2 whose first l1 items are
 * the sequence s1 and whose last l2 items are the sequence s2 *)
val append : 'a t -> 'a t -> 'a t

(* tabulate f n
 * evaluates to a sequence s of length n where the ith item is the result of
 * evaluating (f i) *)
val tabulate : (int -> 'a) -> int -> 'a t

(* nth s i
 * evaluates to the ith element in s. Sequences are 0-indexed (nth s 0 returns
 * the first element of the sequence) *)
val nth : 'a t -> int -> 'a

(* filter p s
 * returns the longest subsequence ss of s such that p evaluates to
 * true for every item in ss *)
val filter : ('a -> bool) -> 'a t -> 'a t

(* map f s
 * evaluates to a sequence s' such that the length of s' is the same as the
 * length s and the ith of the element of s' is the result of applying f to the
 * ith element of s *)
val map : ('a -> 'b) -> 'a t -> 'b t

(* reduce c b s
 * combines all of the items in s pairwise with c using b as the base case. c
 * must be  associative, with b as its identity *)
val reduce : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a

(* mapreduce l e n s
 * equivalent to reduce n e (map l s) *)
val map_reduce : ('a -> 'b) -> 'b -> ('b -> 'b -> 'b) -> 'a t -> 'b

(* repeat x n
 * evaluates to a sequence consisting of exactly n-many copies of x. *)
val repeat : 'a -> int -> 'a t

(* flatten ss
 * is equivalent to reduce append (empty ()) ss *)
val flatten : 'a t t -> 'a t

(* zip (s1,s2)
 * evaluates to a sequence whose nth item is the pair of the nth
 * item of s1 and the nth item of s2. *)
val zip : ('a t * 'b t) -> ('a * 'b) t

(* split s i
 * evaluates to a pair of sequences (s1,s2) where s1 has length i and
 * append(s1,s2) is the same as s. *)
val split : 'a t -> int -> 'a t * 'a t
