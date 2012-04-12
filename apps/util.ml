open Complex
open Str

let mul = Complex.mul
let pow = Complex.pow
let div = Complex.div
let sub = Complex.sub
let add = Complex.add
let norm = Complex.norm

let zero = Complex.zero
let one = Complex.one
type complex = Complex.t

type polynomial = float list

let polynomial_of_string (p : string) : float list =
  let coeffs = Str.split (Str.regexp ", ?") p in
    List.rev (List.rev_map (float_of_string) coeffs)

let string_of_c ({re=r; im=i} : complex) =
  (string_of_float r) ^ " + " ^ (string_of_float i) ^ "i"

let c_of_r (r : float) : complex =
  {re=r; im=0.}

let c_of_i (i : int) : complex =
  c_of_r (float_of_int i)

let cut_off_float (f : float) : float =
  (floor ((f *. 10.) +. 0.5)) /. 10.

let round_complex_number (z : complex) : complex =
  let {re = r; im = i} = z in
    {re = cut_off_float r; im = cut_off_float i}

(* Returns the entire contents of the provided filename *)
let read_whole_file filename =
  let file = open_in_bin filename in
  let size = in_channel_length file in
  let contents = String.create size in
    really_input file contents 0 size;
    close_in_noerr file;
    contents

(* Splits a string into words *)
let split_words = Str.split (Str.regexp "[^a-zA-Z0-9]+")

(* take up to the first n elements of a list *)
let rec take (l : 'a list) (n : int) : 'a list =
  if n = 0 then [] else
  match l with
    [] -> []
  | x :: t -> x :: take t (n - 1)

(* iterate a function n times *)
let iterate (f : 'a -> 'a) (n : int) : 'a -> 'a =
  let rec g n x = if n = 0 then x else g (n-1) (f x)
  in g n

type document = {id : int; title : string; contents : string}

(* Computes word vectors for a set of documents. The given file should
 * contain a list of documents: one per line. Each document is of the
 * format: "id @ title @ body" such that '@' does not appear in the title
 * or contents. You could potentially use map-reduce to compute these
 * word vectors *)
let load_documents (filename : string) : document list =
  let f = open_in filename in
  let rec next accum =
    match (try Some (input_line f) with End_of_file -> None) with
    | None -> accum
    | Some line ->
      (match Str.split (Str.regexp "@\\|$") line with
        | [id; title; contents] ->
          next ({id = int_of_string id; title = title; contents = contents} :: accum)
        | _ -> failwith "malformed input") in
  let docs = next [] in
  close_in f;
  docs

let print_map_results (kv_list : (string * string) list) : unit =
  print_endline "Map Results";
  List.iter
    (fun (k, v) -> Printf.printf "Key: {'%s'} Value: {'%s'}\n" k v)
    (List.sort (fun (k1, _) (k2, _) -> compare k1 k2) kv_list)

let print_kvs (kvs_list : (string * string list) list) : unit =
  List.iter
    (fun (k, vs) ->
      let s = match vs with
        | [] -> ""
        | _ -> Printf.sprintf "'%s'" (String.concat "', '" vs) in
      Printf.printf "Key: {'%s'} Values: {%s}\n" k s)
    (List.sort (fun (k1, _) (k2, _) -> compare k1 k2) kvs_list)

let print_combine_results (kvs_list : (string * string list) list) : unit =
  print_endline "Combine Results";
  print_kvs kvs_list

let print_reduce_results (kvs_list : (string * string list) list) : unit =
  print_endline "Reduce Results";
print_kvs kvs_list
