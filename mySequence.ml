type 'a t = 'a array
(* basically a 2d array, but easier to handle *)
type 'a arrList = Arr of 'a array * 'a arrList | Empty

let multi_create (f:int -> unit) (n:int) : Thread.t array = 
  let a = Array.make n (Thread.self ()) in 
  for i=0 to n-1 do 
    a.(i) <- Thread.create f i;
  done;
  a
    
let multi_join (a:Thread.t array) : unit = 
  Array.iter Thread.join a

let length = Array.length

let empty () = [||]

let cons item arr = 
  let b = Array.make (1 + length arr) item in
  let f i = 
    if i = 0 then b.(i) <- item 
    else b.(i) <- (arr.(i-1)) in
  ignore(multi_create f (length b));
  b
  
let singleton item = Array.make 1 item
  
let tabulate f n =
  let m = Mutex.create () in
  let b = ref (empty ()) in
  let f2 i = (
    let v = f i in
    (if (length !b) = 0 then
      (Mutex.lock m; b := Array.make n v; Mutex.unlock m)
    else 
      (!b).(i) <- v) ) in
  multi_join (multi_create f2 n); 
  !b

let nth = Array.get 

let split s i = 
  let res = (Array.make i s.(0), Array.make ((length s)-i) s.(0)) in
  let f j =  
    if j < i then (fst res).(j) <- s.(j)
    else (snd res).(j-i) <- s.(j) in 
  let a = multi_create f (length s) in
  multi_join a;
  res

let append s s2 = 
  let b = Array.make ((length s)+(length s2)) (nth s 0) in 
  let f i = 
    if i < (length s) then
      b.(i) <- s.(i)
    else 
      b.(i) <- s2.(i-(length s)) in 
  multi_join (multi_create f ((length s)+(length s2)));
  b

let map f s =
  let b = ref (empty()) in
  let m = Mutex.create () in
  let f2 i =
    let v = f s.(i) in
    if (length !b) = 0 then
      (Mutex.lock m; b := Array.make (length s) v; Mutex.unlock m;)
    else (!b).(i) <- v in 
  multi_join (multi_create f2 (length s));
  !b

let repeat x n = Array.make n x

let zip (s1,s2) = 
  (if (length s1) <> (length s2) then 
    failwith "Lengths of sequences must be equal"
  else ());
  let b = Array.make (length s1) ((nth s1 0),(nth s2 0)) in 
  let f i = 
    b.(i) <- ((nth s1 i),(nth s2 i)) in 
  multi_join (multi_create f (length s1));
  b

(* Construct an upside-down binary tree with counts,
   and then another binary tree with indices.
   tfArr is padded with false's so its length is a power of 2.
   The algorithm computes indices for these fake elements, but they are trash
   and not used because the corresponding tfArr element is false *)
let filter f arr = 
  let nearestPowerTwo n = 
    let l = ceil (log (float_of_int n) /. log 2.) in
      int_of_float (2. ** l) in
  let count = ref Empty in
  let indices = ref Empty in
  let finalArray = ref (empty ()) in
  let tfLength = nearestPowerTwo (length arr) in
  let tfArr = Array.make tfLength false in
  let tfTempArr = map f arr in
  let f2 i = tfArr.(i) <- tfTempArr.(i) in
  (* tfArr is power of two -> constructing binary trees much easier *)
  multi_join (multi_create f2 (length arr));

  let createInitialTree i =
    if tfArr.(i) then 
      (match (!count) with 
           Arr (a,_) -> a.(i) <- 1
         | _ -> failwith "error1")
    else () in

  let fillTree i = 
    match (!count) with
        Arr (a, (Arr (a2,_))) -> a.(i) <- a2.(2*i) + a2.(2*i+1) 
      | _ -> failwith "error2" in

  (*left branch's indices will be (prevLow, prevHigh - numTrueOnRight)
    right branch's indices will be (prevLow + numTrueOnLeft, prevHigh) *)
  let fillIndices i =
    match (!count) with
        Arr (c,_) -> (
          match (!indices) with
              Arr (currIndices, (Arr (prevIndices,_))) ->
                (let (low,high) = prevIndices.(i) in
                   currIndices.(2*i) <- (low, high - c.(2*i+1));
                   currIndices.(2*i+1) <- (low + c.(2*i), high))
            | _ -> failwith "error3")
      | _ -> failwith "error7" in  
                  
  let fillFinal i =
    if tfArr.(i) then
      (match (!indices) with 
           Arr (a,_) -> (!finalArray).(fst a.(i)) <- arr.(i)
         | _ -> failwith "error4")
    else () in

  let rec reconstruct n =
    if n > tfLength then (* done *)
      (multi_join (multi_create fillFinal (length arr)))
    else (* keep going up *)
      (indices := Arr (Array.make n (0,0), (!indices));
       multi_join (multi_create fillIndices (n/2));
       (match (!count) with 
            Arr (_,b) -> count := b
          | _ -> failwith "error5");
       reconstruct (2*n)) in 
   
  let rec spawner n = 
    if n = (-1) then (* initial counts *)
      (count := Arr (Array.make tfLength 0, Empty);
       multi_join (multi_create createInitialTree (length arr));
       spawner (tfLength/2))
    else if n = 0 then (* counting done, start reconstructing *)
      (match (!count) with
           Arr (a,b) -> 
             finalArray := Array.make a.(0) (nth arr 0);
             indices := Arr (Array.make 1 (0, a.(0)-1), Empty);
             count := b;
             reconstruct 2
         | _ -> failwith "error6")
    else (* keep counting *)
      (count := Arr (Array.make n 0, (!count));
       multi_join (multi_create fillTree n);
       spawner (n/2)) in
  spawner (-1);
  (!finalArray) 
 
let half x = int_of_float (ceil ((float_of_int x) /. 2.)) 
let reduce c b s =
  let newLength = (length s) + 1 in
  let workingArr = Array.make newLength b in
  let workingArrCopy = Array.make newLength b in
  let currMaxIndex = ref (newLength-1) in
  multi_join (multi_create (fun i -> workingArrCopy.(i) <- s.(i)) (length s));
  let f i =
    if (2*i+1) > (!currMaxIndex) then (workingArr.(i) <- workingArrCopy.(2*i))
    else (workingArr.(i) <- c workingArrCopy.(2*i) workingArrCopy.(2*i+1)) in
  let rec spawner n = 
    if (!currMaxIndex) = 0 then
      workingArr.(0)
    else 
      (multi_join (multi_create f n);
       multi_join 
         (multi_create (fun i -> workingArrCopy.(i) <- workingArr.(i)) n);
       currMaxIndex := ((!currMaxIndex) / 2);
       spawner (half ((!currMaxIndex)+1))) 
  in
  spawner (half ((!currMaxIndex)+1))
    
let map_reduce l e n s = reduce n e (map l s)

let flatten ss = reduce append (empty ()) ss
