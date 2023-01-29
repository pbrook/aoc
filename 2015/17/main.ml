let rec readfile ic =
    try
        let line = input_line ic in
        int_of_string line :: readfile ic
    with End_of_file -> []

let rec fill vol n tubs =
    if vol < 0 then []
    else if vol == 0 then [n]
    else match tubs with
    | []-> begin assert (vol > 0); [] end
    | x::xr -> List.rev_append (fill (vol - x) (n+1) xr) (fill vol n xr)


let fill_all filename vol =
    let tubs = readfile (open_in filename) in
    let res = fill vol 0 tubs in
    let part1 = List.length res in
    let minval = List.fold_left min Int.max_int res in
    let part2 = List.length (List.filter ((==) minval) res) in
    (part1, part2)

let () = 
    let test1 = fill_all "test1" 25 in
    assert (fst test1 == 4);
    assert (snd test1 == 3);
    let input = fill_all "input" 150 in
    Printf.printf "%d\n" (fst input);
    Printf.printf "%d\n" (snd input);
