let parse c = match c with
    | '(' -> 1
    | ')' -> -1
    | _ -> assert false

let rec readfile ic =
    try
        let c = input_char ic in
        parse c :: readfile ic
    with End_of_file -> []

let part1 filename =
    let inp = readfile (open_in filename) in
    List.fold_left (+) 0 inp

let rec find_basement pos s =
    match s with
    | x::xs -> let np = pos + x in
        if np == -1 then 1 else 1 + find_basement np xs
    | [] -> assert false

let part2 filename =
    let inp = readfile (open_in filename) in
    find_basement 0 inp

let () = 
    Printf.printf "%d\n" (part1 "input");
    Printf.printf "%d\n" (part2 "input")
