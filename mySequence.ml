type 'a t = 'a array

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
    else b.(i) <- (arr.(i-1))
  in
  ignore(multi_create f (length b));
  b
  
let singleton item = 
  let a = empty () in
  cons item a
  
let tabulate f n =
  let m = Mutex.create () in
  let b = ref (empty ()) in
  let f2 i = (
    let v = f i in
    Mutex.lock m;
    (if (length !b) = 0 then
      (b := Array.make n v)
    else 
      (!b).(i) <- v); 
    Mutex.unlock m) in
  multi_join (multi_create f2 n); 
  !b

let nth = 
  Array.get 

let split s i = 
  let res = (Array.make i s.(0), Array.make ((length s)-i) s.(0)) in
  let f j =  
    if j < i then (fst res).(j) <- s.(j)
    else (snd res).(j-i) <- s.(j)
  in 
  let a = multi_create f (length s) in
  multi_join a;
  res

(* Returns 'a * ('a array * 'a array)
   First element is the middle of the sequence
   second element are items to the left of middle
   third element are items to the right of the middle *)
let split_half s =
  let i = (length s) / 2 in
  let res = (Array.make i s.(0), Array.make ((length s)-i-1) s.(0)) in
  let f j =  
    if j < i then (fst res).(j) <- s.(j) 
    else if j = i then ()
    else (snd res).(j-i-1) <- s.(j)
  in 
  let a = multi_create f (length s) in
  multi_join a;
  (i, res)

let filter f arr = 
  let rec run s =
    let (mid,(left,right)) = split_half s in
    let newThread = Thread.create f mid in
    Thread.delay (run left);
    Thread.delay (run right);
    newThread
