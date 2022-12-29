let vowels = "aeiou"

let vowel_count n c =
    n + if String.contains vowels c then 1 else 0

let trystring ?(start=0) f s len =
    let rec loop n =
        if n > String.length s - len then
            false
        else if f s n then
            true
        else
            loop (n+1)
    in
    loop start

let badstr = Str.regexp {|ab\|cd\|pq\|xy|}
let hasbad s =
    try
        ignore (Str.search_forward badstr s 0);
        true
    with Not_found -> false

let part1 s =
    let nv = String.fold_left vowel_count 0 s in
    let dup = trystring (fun s n -> s.[n] == s.[n+1]) s 2 in
    nv >= 3 && dup && not (hasbad s)

let ispair s n0 =
    let a, b = s.[n0], s.[n0+1] in
    trystring ~start:(n0+2) (fun s n -> s.[n] == a && s.[n+1] == b) s 2 

let part2 s =
    let pair = trystring ispair s 4 in
    let aba = trystring (fun s n -> s.[n] == s.[n+2]) s 3 in
    pair && aba

let test part filename =
    let count = ref 0 in
    let ic = open_in filename in
    try
        while true do
            let line = input_line ic in
            if part line then incr count
        done;
        assert false
    with End_of_file -> !count

let () = 
    Printf.printf "%d\n" (test part1 "input");
    Printf.printf "%d\n" (test part2 "input")
