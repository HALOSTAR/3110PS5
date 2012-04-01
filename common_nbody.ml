(* Constants *)
let bigG : Plane.scalar = 6.67 *. (10.**(-11.))

(* Bodies *)
type mass = Plane.scalar

type location  = Plane.point

type velocity  = Plane.vector

type body = mass * location * velocity

(* Streams *)
type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

let peek (s:'a stream) : 'a = 
  match s with 
    | Nil -> raise Not_found
    | Cons(x,_) -> x

type simulation = (body Sequence.t) stream
