#use "sequence.ml"
(* inverted_index computes an inverted index for the contents of
 * a given file. The filename is the given string.
 * The results are output to stdout. *)

module Dict = Map.Make (
    struct 
        type t = string
        let compare = compare
    end
)

let mkindex (args : string ) : unit = 
    let file = read_whole_file args in
    let docs = load_documents file in
    let s = Array.of_list docs in
    
    let map = map_reduce 
    (fun x -> List.fold_left (fun a e -> if !(Dict.mem e a) then Dict.add e
    [x.id] a else a) Dict.empty (split_words
    x.contents)) Dict.emtpy acc s in 
    Dict.
    
let acc = (fun a x -> List.fold_left( fun ac e -> if Dict.mem (fst e) ac then
    Dict.add (fst e) (snd e)@(Dict.find (fst e) ac) ac else Dict.add (fst e)
