let part1 filename =
    let ic = open_in filename in
    let pos = ref (0, 0) in
    let ht = Hashtbl.create 1024 in
    Hashtbl.replace ht !pos true;
    try
        while true do
            let c = input_char ic in
            let x, y = !pos in
            let np = match c with
            | '>' -> x+1, y
            | '<' -> x-1, y
            | 'v' -> x, y+1
            | '^' -> x, y-1
            | _ -> assert false
            in
            Hashtbl.replace ht np true;
            pos := np
        done;
        assert false
    with End_of_file -> Hashtbl.length ht

let () = 
    Printf.printf "%d\n" (part1 "input")
