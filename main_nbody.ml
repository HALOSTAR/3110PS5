open Common_nbody
open Test_nbody

(* run_simulation: execute simulation [stream] for [steps] and write
   transcript to [filename]. *)
let run_simulation (stream:simulation) (steps:int) (filename:string) : unit =
  let string_of_float f = 
    match Pervasives.string_of_float f with 
      | "nan" -> "0.001"
      | "-nan" -> "-0.001"
      | s -> if s.[String.length s - 1] = '.' then s ^ "0" else s in
  let string_of_bodies bodies = 
    Sequence.map_reduce 
      (fun (_,(x,y),_) -> 
        Printf.sprintf "%s %s %d\n" 
          (string_of_float x) (string_of_float y) 0)
      "" (^) bodies in 
  let rec make_transcript stream steps acc = 
    if steps = 0 then acc 
    else match stream with 
      | Nil -> acc
      | Cons(bodies, more) ->
        make_transcript (more ()) (steps-1) 
          (acc ^ (string_of_bodies bodies)) in 
  let o = open_out filename in 
  output_string o 
    (string_of_int (Sequence.length (peek stream))
     ^ "\n" 
     ^ make_transcript stream steps "");
  close_out o

let main = 
  run_simulation
    (Naive_nbody.make_simulation lineup_bodies)
    200
    "output/lineup-naive.txt";
  run_simulation
    (Naive_nbody.make_simulation twister_bodies)
    200
    "output/twister-naive.txt";
  run_simulation
    (Naive_nbody.make_simulation diamond_bodies)
    200 
    "output/diamond-naive.txt";
  run_simulation
    (Naive_nbody.make_simulation orbit_bodies)
    500 
    "output/orbit-naive.txt"
