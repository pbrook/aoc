let vowels = "aeiou"

let vowel_count n c =
    n + if String.contains vowels c then 1 else 0

let rec isdup s n =
    if n > String.length s - 2 then
        false
    else
        if s.[n] == s.[n+1] then
            true
        else
            isdup s (n+1)

let badstr = Str.regexp {|ab\|cd\|pq\|xy|}
let hasbad s =
    try
        ignore (Str.search_forward badstr s 0);
        true
    with Not_found -> false

let is_nice s =
    let nv = String.fold_left vowel_count 0 s in
    let dup = isdup s 0 in
    nv >= 3 && dup && not (hasbad s)


let part1 filename =
    let count = ref 0 in
    let ic = open_in filename in
    try
        while true do
            let line = input_line ic in
            if is_nice line then incr count
        done;
        assert false
    with End_of_file -> !count

let () = 
    Printf.printf "%d\n" (part1 "input")
