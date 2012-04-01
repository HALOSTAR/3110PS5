open Common_nbody 

(* [acceleration b1 b2] : compute gravitational acceleration vector for [b1] 
 * due to [b2]. *)
let acceleration (b1:body) (b2:body) : Plane.vector =
  failwith "not implemented"

(* [accelerations bodies]: compute gravitational acceleration vector for
 *  each body in [bodies] *)
let accelerations (bodies:body Sequence.t) : Plane.vector Sequence.t = 
  failwith "not implemented"

(* [update bodies]: apply acceleration to update the positions & velocities
 * of all bodies in [bodies] *)
let update (bodies:body Sequence.t) : body Sequence.t = 
  failwith "not implemented"

(* [make_simulation bodies]: Create a stream representation of the N-Body simulation  *)
let rec make_simulation (bodies:body Sequence.t) : simulation = 
  failwith "nope"
