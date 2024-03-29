let part1 filename =
    let raw = ref 0 and parsed = ref 0 in
    let ic = open_in filename in
    let rec parse line n = 
        match line.[n] with
        | '"' -> assert (n + 1 == String.length line)
        | '\\' -> incr parsed; let nn = match line.[n+1] with
            | '"' | '\\' -> n + 2
            | 'x' -> n + 4
            | _ -> assert false
        in parse line nn
        | _ -> incr parsed; parse line (n + 1)
    in
    try
        while true do
            let line = input_line ic in
            assert (line.[0] == '"') ;
            raw := !raw + String.length line ;
            parse line 1
        done;
        assert false
    with End_of_file -> (!raw - !parsed)

let escape n c = match c with
    | '\\' | '"' -> n + 1
    | _ -> n

let part2 filename =
    let extra = ref 0 in
    let ic = open_in filename in
    try
        while true do
            let line = input_line ic in
            extra := 2 + String.fold_left escape !extra line
        done;
        assert false
    with End_of_file -> !extra

let () = 
    Printf.printf "%d\n" (part1 "input");
    Printf.printf "%d\n" (part2 "input");
