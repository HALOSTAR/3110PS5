let maxThreads = 400

type ('a,'b) jobFunc = ('a -> 'b)
type job = {thr:Thread.t; finished:bool}
type ('a,'b) task = {
  mutex: Mutex.t;
  mutable stack: (('a,'b) jobFunc * 'a) list;
  mutable activeThreads: Thread.t list;
}

let makeTask () = 
  {mutex = Mutex.create (); stack = []; activeThreads = []} 
  
let create (f:'a -> 'b) (n:'a) : task = 
  Mutex.lock 
  
