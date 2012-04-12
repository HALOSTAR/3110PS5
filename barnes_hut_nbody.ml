open Common_nbody

(* Representation of a 2d rectangular plane *)
type bbox = { north_west:Plane.point; south_east:Plane.point }

(* Our representation of a Barnes-Hut tree. You are free to use this type
 * or create your own. *)
type bhtree = 
    Empty of bbox
  | Single of body*bbox
  | Cell of body*bbox*(bhtree Sequence.t)


let getFour bbox =
    let nw = bbox.north_west and se = bbox.south_east in
    let nwx = fst nw and nwy = snd nw and sex = fst se and sey = snd se in
    let wm = ((nwx), (nwy+.sey)/.2.) in
    let nm =  ((nwx +. sex)/.2., nwy) in
    let mm = ((nwx +. sex)/.2., (nwy+.sey)/.2.) in
    let sm = ((nwx +. sex)/.2., sey) in 
    let em = (sex, (nwy+.sey)/.2.) in
    [{north_west = nw; south_east = mm};{north_west = nm; south_east = em};
    {north_west = wm; south_east = sm};{north_west = mm; south_east = se}]
    

(* [quarters box]: Divide a bounding box into 4 equally sized quadrants. *)
let quarters (box:bbox) : bbox Sequence.t = 
    let l = getFour bbox in
    List.fold_left (fun a x -> cons x a) empty() (List.rev l)
    

(* [psuedobody bodies]: Return a psuedobody representing all bodies in [bodies]  *)
let pseudobody (bodies:body Sequence.t) : body = 
    let sum = ref (0., (0., 0.), 0.) in
    for i = (length bodies)-1 downto 0 do
        let (sm, (sx, sy), v) = !sum in
        let (m,(x,y),v) = (nth bodies i) in
        let sm = sm +. m and sl = (sx+.(m*.x), sy+.(m*.y)) in
        sum := (sm, sl ,v)
    done;
    let (m,(mx, my),v) = !sum in
    (m, (mx/.m, my/.m), v)

exception outException

let ifIn b bb = 
    let (m, (x,y), v) = b in
    let ((nwx, nwy), (sex, sey)) = bb in
    ( x>= nwx && x < sex && y <= nwy && y> sey)
        
let rec insert bd bhtree rtbox=
    if ifIn bd rtbox then 
    match bhtree with 
    | Empty (bbox) -> if ifIn bd bbox then Single (bd,bbox) else Empty bbox
    | Single (b, bbox) -> Cell (pseudobody (cons (cons empty() b) bd), bbox,
    map (fun x -> (insert bd (insert b (Empty x) rtbox) rtbox) ) (quarters bbox))
    | Cell (b,bbox, bhtree_seq) -> let pse = pseudobody (cons (cons empty() b)
    bd) in
      Cell (pse, bbox, map (fun x -> insert bd x rtbox) bhtree_seq)
    else raise outException

(* [make_bhtree bodies box]: Create Barnes-Hut tree of [bodies].
 * Raise an exception if a body in [bodies] is not contained within [box]. *)
let make_bhtree (bodies:body Sequence.t) (box:bbox) : bhtree =
    let tr = ref Empty (box) in 
    for i = (length bodies)-1 downto 0 do
        tr := try insert (nth bodies i) tr box with outException -> (print_string
        "out"; tr) 
    done;
    (!tr)

(* [in_range b1 b2]: returns true if the mass-weighted distance from
 * [b1] to [b2] is less than [theta]. Use to determine whether Barnes-Hut 
 * approximation is appropriate for a body and psuedobody. *)
(*b2 is the exerting body*)
let in_range (b1:body) (b2:body) (theta:Plane.scalar) : bool = 
     let (m2,l2,v2) = b2 in 
     let (m1,l1,v1) = b1 in
     let d = distance l1 l2 in
     (m/.d < theta)



let rec traverse bhtree bd acc theta =
    match bhtree with
    | Empty (bb) -> acc
    | Single (b, bb) -> 
        let (m,l,v)=b and (md,ld,vd)= bd in if (distance l ld) = 0. then acc else 
    v_plus acc (scale_point (s_divide (s_times bigG m2) (s_times d d)) (unit_vector ld l))
    | Cell (b,bb,btr_seq) -> 
        if in_range bd b theta then sum (fun x -> traverse x bd acc theta) btr_seq else 
            let (m,l,v)=b and (md,ld,vd)= bd in 
    v_plus acc (scale_point (s_divide (s_times bigG m2) (s_times d d)) (unit_vector ld l))

(* [acceleration tree theta body]: use [tree] and [theta] to approximate the
 * acceleration on [body] due to every other body in the tree.*)
let acceleration (tree:bhtree) (theta:Plane.scalar) (body:body) : Plane.vector =
    traverse bhtree body (0., 0.) theta 
    
(* [accelerations bodies theta box]: Approximate the acceleration vector for each 
 * body in [bodies]. *)
let accelerations (bodies:body Sequence.t) (theta:Plane.scalar) (box:bbox) 
    : Plane.vector Sequence.t = 
    let bhtree = make_bhtree bodies box in
    let vseq = ref (empty()) in
    for i = (length bodies)-1 downto 0 do
        let curB = nth bodies i in
        let v = !vseq in
        vseq := cons (acceleration bhtree theta curB) v
        
    done;
    (!vseq)

let toSortedArr seq cmp=
    let arr = Array.make (length bodies) ((0.),(0.,0.),(0.)) in
    for i = (length bodies)-1 downto 0 do
        arr.(i) <- (nth seq i);
    done;
    Array.sort arr cmp

let cmpnx a b = let (ma,(xa, ya),va) = a and (mb,(xb,yb),vb) = b in 
        if xa > xb then 1 else if xa = xb then 0 else -1

let cmpny a b = let (ma,(xa, ya),va) = a and (mb,(xb,yb),vb) = b in 
        if ya > yb then 1 else if ya = yb then 0 else -1


let calcBox bodies = 
    let len = length bodies in
    let xs = toSortedArr (map (fun e -> let (m,(x,y),v) = e in x) bodies) cmpnx
    in
    let ys = toSortedArr (map (fun e -> let (m,(x,y),v) = e in y) bodies) cmpnx
    in
    let nwx = xs.(0) and nwy = ys.(len-1) and sex = xs.(len-1) and sey = ys.(0) 
    {north_west = (nwx,nwy); south_east = (sex,sey)}

(* [update bodies theta box]: update the positions of all bodies in [bodies] via
 * Barnes-Hut approximation *)
let update (bodies:body Sequence.t) (theta:Plane.scalar) : body Sequence.t =
  let newBodies = ref (empty ()) in
  let box = calcBox bodies in 
  let accels = accelerations bodies theta box in
  for i = ((length bodies)-1) downto 0 do
    let (m,l,v) = nth bodies i in
    let newLoc = v_plus l (v_plus v (scale_point 0.5 (nth accels i))) in
    let newVel = v_plus v (nth accels i) in
    newBodies := cons (m,newLoc,newVel) (!newBodies)
  done;
  (!newBodies)


(* [make_simulation bodies theta box]: create a stream representing the simulation. *)
let rec make_simulation (bodies:body Sequence.t) (theta:Plane.scalar) : simulation =
   Cons(bodies, (fun () -> make_simulation (update bodies theta)))
