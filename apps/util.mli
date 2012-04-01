type complex = Complex.t
val string_of_c : complex -> string
val c_of_r : float -> complex
val c_of_i : int -> complex
val round_complex_number : complex -> complex
val mul : complex -> complex -> complex
val pow : complex -> complex -> complex
val div : complex -> complex -> complex
val sub : complex -> complex -> complex
val add : complex -> complex -> complex
val norm : complex -> float
val zero : complex
val one : complex

type polynomial = float list
val polynomial_of_string : string -> polynomial

val read_whole_file : string -> string
val split_words : string -> string list

(* take up to the first n elements of a list *)
val take : 'a list -> int -> 'a list

(* iterate a function n times *)
val iterate : ('a -> 'a) -> int -> 'a -> 'a

type document = {id : int; title : string; contents : string}

(* Computes word vectors for a set of documents. The given file should
 * contain a list of documents: one per line. Each document is of the
 * format: "id @ title @ body" such that '@' does not appear in the title
 * or contents. You could potentially use map-reduce to compute these
 * word vectors *)
val load_documents : string -> document list

val print_kvs : (string * string list) list -> unit
val print_map_results : (string * string) list -> unit
val print_combine_results : (string * string list) list -> unit
val print_reduce_results : (string * string list) list -> unit
