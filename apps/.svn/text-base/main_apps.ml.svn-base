open Apm
open Inverted_index
let main =
  if Sys.argv.(1) = "mkindex" then mkindex Sys.argv.(2)
  else matchme (Array.of_list [Sys.argv.(2); Sys.argv.(3); Sys.argv.(4); Sys.argv.(5)])
