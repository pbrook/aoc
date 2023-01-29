let clean s = let sp = String.map (function
    | ',' | ':' -> ' '
    | c -> c) s in
    String.trim sp

let parse_spec line =
    match String.split_on_char ' ' line |> List.map clean with
    | [a; b] -> (a, int_of_string b)
    | _ -> assert false

let rec read_spec ic =
    try
        let line = input_line ic in
        parse_spec line :: read_spec ic
    with End_of_file -> []

let spec = open_in "spec" |> read_spec |> List.to_seq |> Hashtbl.of_seq

let rec pairs = function
    | [] -> []
    | [_] -> assert false
    | x::y::xr -> (x, int_of_string y) :: pairs xr

let parse line =
    let w = String.split_on_char ' ' line in
    let props = w |> (List.map clean) |> pairs in
    assert (List.hd props |> fst |> String.equal "Sue");
    (List.hd props |> snd, List.tl props)

let rec readfile ic =
    try
        let line = input_line ic in
        parse line :: readfile ic
    with End_of_file -> []

let match_prop (name, num) =
    Hashtbl.find spec name == num

let match_spec a =
    List.for_all match_prop (snd a)

let part1 filename =
    let aunts = readfile (open_in filename) in
    let who = List.find match_spec aunts in
    fst who

let () = 
    Printf.printf "%d\n" (part1 "input");
