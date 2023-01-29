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

let part1 name expected actual =
    ignore name;
    actual == expected

let part2 name expected actual = match name with
    | "cats" | "trees" -> actual > expected
    | "pomeranians" | "goldfish" -> actual < expected
    | _ -> actual == expected


let find_aunt part aunts =
    let match_prop (name, num) =
        let expected = Hashtbl.find spec name in
        part name expected num in
    let match_spec a = List.for_all match_prop (snd a) in
    List.find match_spec aunts


let find_sue filename =
    let aunts = readfile (open_in filename) in
    let sue1 = find_aunt part1 aunts in
    let sue2 = find_aunt part2 aunts in
    (fst sue1, fst sue2)

let () = 
    let sue = find_sue "input" in
    Printf.printf "%d\n" (fst sue);
    Printf.printf "%d\n" (snd sue);
