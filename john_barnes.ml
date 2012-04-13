let theta = 1.0

let origin = (0., 0.)
let computeOriginalBox seq = 
  let mapF (_,loc,_) = loc in
  let southeast_reduceF (_,(x1,y1),_) (_,(x2,y2),_) = (max x1 x2, max y1 y2) in
  let northwest_reduceF (_,(x1,y1),_) (_,(x2,y2),_) = (min x1 x2, min y1 y2) in
  let southeast = map_reduce mapF (0,0) southeast_reduceF seq in
  let northwest = map_reduce mapF southeast northwest_reduceF seq in
  (northwest, southeast)

let emptyBody = (0., origin, origin)

let emptySeq () = tabulate (fun _ -> Empty) 1

let quarters ((x1,y1),(x2,y2)) =
  let midX = s_divide (s_plus x1 x2) 2. and
      midY = s_divide (s_plus y1 y2) 2. in
  let f = function 
    | 0 -> ((x1,y1),(midX,midY))
    | 1 -> ((midX,y1),(x2,midY))
    | 2 ->  ((x1,midY),(midX,y2))
    | 3 ->  ((midX,midY),(x2,y2)) 
    | _ -> failwith "error2" in
  tabulate (fun i -> f i) 4

let make_bhtree bodies bbox = 
  let originalBody = (0., origin, origin) in
  let originalBox = computeOriginalBox bodies in
  let tree = ref (originalBody, originalTree, empty ()) in 
  for i = 0 to (length bodies) do
    tree := insert tree (nth bodies i)
  done; 
  tree

let withinBox ((nw_x, nw_y),(se_x,se_y)) (x,y) =
  if x >= nw_x && x <= se_x && y <= nw_y && y >= se_y then true
  else false

let centerOfMass (m1,(x1,y1),_) (m2,(x2,y2),_) =
  let newX = s_divide (s_plus (s_times m1 x1) (s_times m2 x2)) (s_plus m1 m2)
  in
  let newY = s_divide (s_plus (s_times m1 y1) (s_times m2 y2)) (s_plus m1 m2)
  in (newX, newY)
  
let rec insert bht (mass,loc,vel) = 
  match bht with
    Empty -> Single of bod
  | Single x -> 
  | Cell ((pMass,pLoc,pVel),oldBox,seq) ->
      if withinBox oldBox loc then 
	if length seq = 0 then
	  (* make four empty sectors *)
	  let newQuads = quarters oldBox
	let newMass = pMass + mass and
	    newLoc = centerOfMass (pMass,pLoc,pVel) (mass,loc,vel) and
	    newSeq = map (fun 
	Cell ((newMass,newLoc,pVel), oldBox, 
      
