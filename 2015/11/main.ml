let input = "hepxcrrq"

let next_char x = Char.chr (Char.code x + 1)

let skip_char = function
    | 'i' | 'o' | 'l' -> true
    | _ -> false

let advance_char x = let n = next_char x in
    if skip_char n then next_char n else n

let is_run x y z = x == next_char y && y == next_char z

let rec find_seq s = match s with
    | [] -> false
    | [_] -> false
    | [_;_] -> false
    | x :: (y :: z :: _ as xs) ->
        if is_run x y z then true else find_seq xs

let rec find_pair ?(prev = ' ') s = match s with
    | [] -> false
    | [_] -> false
    | x :: (y :: ys as xs) ->
        if x == y && x != prev then
            if prev == ' ' then
                find_pair ~prev:x ys
            else
                true
        else
            find_pair ~prev xs

let is_valid s =
    find_seq s && find_pair s

let rec advance = function
    | [] -> []
    | 'z' :: xs -> 'a' :: advance xs
    | x :: xs -> advance_char x :: xs

let scrub_char bad c = if bad then (true, 'a') else
    if skip_char c then
        (true, next_char c)
    else
        (false, c)

let scrub s = List.fold_left_map scrub_char false s |> snd

let s2l ?(ns=false) s =
    let lst = String.to_seq s |> List.of_seq in
    List.rev (if ns then lst else scrub lst)

let l2s s = s |> List.rev |> List.to_seq |> String.of_seq

let part1 s =
    let lst = s2l s in
    let rec loop s = let ns = advance s in
        if is_valid ns then
            ns
        else
            loop ns
    in let res = loop lst in
    l2s res


let () = 

    assert (let lst = s2l ~ns:true "hijklmmn" in find_seq lst && not (find_pair lst));
    assert (let lst = s2l "abbceffg" in find_pair lst && not (find_seq lst));
    assert (let lst = s2l "abbcegjk" in not (find_pair lst));
    assert (part1 "abcdefgh" |> String.equal "abcdffaa");
    assert (part1 "ghijklmn" |> String.equal "ghjaabcc");
    Printf.printf "%s\n" (part1 input);
