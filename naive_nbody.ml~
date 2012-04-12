open Common_nbody 
open Plane
open Sequence

(* [acceleration b1 b2] : compute gravitational acceleration vector for [b1] 
 * due to [b2]. *)
let acceleration (b1:body) (b2:body) : Plane.vector =
  let (m1,l1,v1) = b1 and
      (m2,l2,v2) = b2 in
  let d = distance l1 l2 in
    if d = 0. then (0.,0.)
    else 
      let a = s_divide (s_times bigG m2) (s_times d d) in
      let unitV = unit_vector l1 l2 in
        scale_point a unitV

(* [accelerations bodies]: compute gravitational acceleration vector for
 *  each body in [bodies] *)
let accelerations (bodies:body Sequence.t) : Plane.vector Sequence.t =
  let accels = ref (empty ()) in
  for i = (length bodies)-1 downto 0 do
    let currBody = (nth bodies i) in
    let mapF b = acceleration currBody b in
    accels := cons (map_reduce mapF (0.,0.) v_plus bodies) !accels
  done;
  !accels
      
(* [update bodies]: apply acceleration to update the positions & velocities
 * of all bodies in [bodies] *)
let update (bodies:body Sequence.t) : body Sequence.t = 
  let newBodies = ref (empty ()) in
  let accels = accelerations bodies in
  for i = ((length bodies)-1) downto 0 do
    let (m,l,v) = nth bodies i in
    let newLoc = v_plus l (v_plus v (scale_point 0.5 (nth accels i))) in
    let newVel = v_plus v (nth accels i) in
    newBodies := cons (m,newLoc,newVel) (!newBodies)
  done;
  (!newBodies)

(* [make_simulation bodies]: Create a stream representation of the N-Body simula
tion  *)
let rec make_simulation (bodies:body Sequence.t) : simulation = 
  Cons(bodies, (fun () -> make_simulation (update bodies)))
