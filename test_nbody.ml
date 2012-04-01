open Common_nbody

let origin = (0.,0.)
let testM = 90000000000.
let std_theta = 42.

(* 10 bodies in a straight line. Initial velocity = 0 *)
let lineup_bodies =
  let m = testM in
  let v = origin in
  Sequence.tabulate(fun x->(m, (1.0+. 15.*.(float_of_int x),5.0),v)) 10
let lineup_theta =
  std_theta

(* 8 bodies in diamond, 0 velocity*)
let diamond_bodies =
  let m = testM in 
  let v = origin in 
  (* Sets absolute coordinates *)
  let pos = function
   | 0 -> ((0.),(50.))
   | 1 -> ((25.),(-25.))
   | 2 -> ((0.),(-50.))
   | 3 -> ((-25.),(-25.))
   | 4 -> ((-50.),(0.))
   | 5 -> ((-25.),(25.))
   | 6 -> ((50.),(0.))
   | _ -> ((25.),(25.)) in
  Sequence.tabulate (fun n ->(m,pos n,v)) 8
let diamond_theta = std_theta

(* Bodies begin in line, given varying initial velocity *)
let twister_bodies = 
  let m = testM in
  let (xI, yI) = ((-60.),(200.)) in
  let v = (0.5, 0.25) in 
  let numBodies = 6 in
  let pos (n:int) : Plane.point = 
    (xI +. (float_of_int n) *. 50. , yI) in
  (* Velocity is function of tabulate index *)
  let speed (n:int) : Plane.vector =
    Plane.scale_point ((float_of_int n) *. -1.) v in
  Sequence.tabulate(fun n->(m, (pos n), speed n)) numBodies 
let twister_theta = std_theta

(* Tiny planetary sim. One sun & 4 moons *)
let orbit_bodies = 
  let bigM = testM *. 75. in
  let lilM = testM /. 10. in   
  let pM = 2.6 in
  let (xI, yI) = origin in
  let v = origin in 
  (* Absolute coordinates for moons. Diamond shape. *)
  let pos = function
   | 0 -> (50., 0.)
   | 1 -> (0.,50.)
   | 2 -> ((-50.),0.)
   | _ -> (0., (-50.)) in 
  (* Absolute velocities. Moons move clockwise around sun *)
  let nM = (-1.)*.pM in  
  let speed = function
   | 0 -> ((0.), (nM))
   | 1 -> (pM,0.)
   | 2 -> ((0.),pM)
   | _ -> ((nM), (0.)) in 
  Sequence.cons
    (bigM,(xI,yI),v)
    (Sequence.tabulate (fun n ->(lilM, pos n, speed n)) 4)  
let orbit_theta = std_theta
