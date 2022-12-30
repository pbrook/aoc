let part1 filename =
    let sum = ref 0 in
    let n = ref 0 in
    let op = ref (+) in
    let ic = open_in filename in
    try
        while true do
            let c = input_char ic in
            if c == '-' then
                op := (-)
            else if c >= '0' && c <= '9' then
                n := !n * 10 + Char.code c - Char.code '0'
            else begin
                sum := !op !sum !n;
                n := 0;
                op := (+)
            end
        done;
        assert false
    with End_of_file -> !op !sum !n

let () = 
    Printf.printf "%d\n" (part1 "input");
