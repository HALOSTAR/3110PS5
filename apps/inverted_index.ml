(* inverted_index computes an inverted index for the contents of
 * a given file. The filename is the given string.
 * The results are output to stdout. *)

module Dict = Map.Make (
    struct 
        type t = string
        let compare = compare
    end
)

let f emp x =
    let lx = Dict.bindings x in 
    List.fold_left( fun ac e -> if Dict.mem (fst e) ac then 
    (Dict.add (fst e) ((snd e)@(Dict.find (fst e) ac)) ac) else (Dict.add (fst e)
    (snd e) ac)) emp lx

let intl_to_str l = List.fold_left (fun a x -> (string_of_int x)::a ) [] l

let to_str_str l = List.fold_left (fun a x ->  (fst x, intl_to_str (snd x) )::a ) [] l 

let mkindex (args : string ) : unit = 
    let docs = load_documents args in
    let s = Array.of_list docs in
    let m = map_reduce 
    (fun x -> List.fold_left (fun a e -> if not (Dict.mem e a) then Dict.add e
    [x.id] a else a) Dict.empty (split_words x.contents)) Dict.empty f s in 
    let final = Dict.bindings m in
    let p = to_str_str final in 
    print_kvs p
    

