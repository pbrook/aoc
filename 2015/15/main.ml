let getnum w n =
    let s = List.nth w n in
    int_of_string (String.trim (
        String.map (fun c -> if c == ',' then ' ' else c) s))

let parse line =
    let w = String.split_on_char ' ' line in
    Array.init 4 (fun n -> getnum w (n*2+2))

let rec readfile ic =
    try
        let line = input_line ic in
        parse line :: readfile ic
    with End_of_file -> []

let add_ing left n right =
    Array.map2 (fun a b -> a + b * n) left right

let total_score cookie =
    Array.fold_left (fun x y -> x * (max y 0)) 1 cookie

let rec try_recipe left r_size r_list =
    match r_list with
    | [] -> assert false
    | [ing] -> total_score (add_ing left r_size ing)
    | ing::rest -> begin
        let best = ref 0 in
        for n = 0 to r_size do
            let score = try_recipe (add_ing left n ing) (r_size - n) rest in
            best := max !best score
        done;
        !best
    end

let part1 filename =
    let ings = readfile (open_in filename) in
    let empty = Array.make 4 0 in
    try_recipe empty 100 ings

let () = 
    assert (part1 "test1" = 62842880);
    Printf.printf "%d\n" (part1 "input");
