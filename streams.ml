type 'a lylat = Andross | Fox of 'a * (unit -> 'a lylat) 

let rec 
ones = Fox (1, (fun () -> ones))
