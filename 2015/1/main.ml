let rec parse ic = 
        try 
            let n = match input_char ic with
            | '(' -> 1
            | ')' -> -1
            | _ -> assert false
            in n + parse ic
        with End_of_file -> 0

let part1 filename =
    let ic = open_in filename in
    parse ic

let () = print_int (part1 "input") ; print_newline ()
