let parse_point s =
    match String.split_on_char ',' s with
    | [x; y] -> (int_of_string x, int_of_string y)
    | _ -> assert false

let parse line =
    let line = if String.starts_with ~prefix:"turn " line then
        Str.string_after line 5 else line in
    match String.split_on_char ' ' line with
    | [op; a; _; b] -> (op, parse_point a, parse_point b)
    | _ -> assert false

let sum2d ar = Array.fold_left (Array.fold_left (+)) 0 ar

let part1 op oldval = match op with
    | "toggle" -> 1 - oldval
    | "on" -> 1
    | "off" -> 0
    | _ -> assert false

let part2 op oldval = match op with
    | "toggle" -> oldval + 2
    | "on" -> oldval + 1
    | "off" -> max (oldval - 1) 0
    | _ -> assert false

let blinken part filename =
    let grid = Array.make_matrix 1000 1000 0 in
    let action line =
        let (op, (x0, y0), (x1, y1)) = parse line in
        for x = x0 to x1 do
            for y = y0 to y1 do
                grid.(x).(y) <- part op grid.(x).(y)
            done
        done
    in
    let ic = open_in filename in
    try
        while true do
            let line = input_line ic in
            action line
        done;
        assert false
    with End_of_file -> sum2d grid

let () = 
    Printf.printf "%d\n" (blinken part1 "input");
    Printf.printf "%d\n" (blinken part2 "input");
