let rec readfile ic =
    try
        let line = input_line ic in
        int_of_string line :: readfile ic
    with End_of_file -> []

let rec fill vol tubs =
    if vol < 0 then 0
    else if vol == 0 then 1
    else match tubs with
    | [] -> if vol == 0 then 1 else 0
    | x::xr-> (fill (vol - x) xr) + (fill vol xr)


let part1 filename vol =
    let tubs = readfile (open_in filename) in
    fill vol tubs

let () = 
    assert (part1 "test1" 25 == 4);
    Printf.printf "%d\n" (part1 "input" 150);
