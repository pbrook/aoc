let getnum w n =
    let s = List.nth w n in
    int_of_string (String.trim (
        String.map (fun c -> if c == ',' then ' ' else c) s))

let parse line =
    let w = String.split_on_char ' ' line in
    List.rev (List.init 5 (fun n -> getnum w (n*2+2)))

let rec readfile ic =
    try
        let line = input_line ic in
        parse line :: readfile ic
    with End_of_file -> []

let add_ing left n right =
    List.map2 (fun a b -> a + b * n) left right

let total_score cookie =
    List.fold_left (fun x y -> x * (max y 0)) 1 (List.tl cookie)

let part1 = ref 0
let part2 = ref 0

let rec try_recipe left r_size r_list =
    match r_list with
    | [] -> assert false
    | [ing] -> begin
        let recipe = add_ing left r_size ing in
        let score = total_score recipe in
        part1 := max !part1 score;
        if List.hd recipe == 500 then part2 := max !part2 score;
    end
    | ing::rest -> for n = 0 to r_size do
        try_recipe (add_ing left n ing) (r_size - n) rest
    done

let cook filename =
    part1 := 0;
    part2 := 0;
    let ings = readfile (open_in filename) in
    let empty = [0; 0; 0; 0; 0] in
    try_recipe empty 100 ings

let () = 
    cook "test1";
    assert (!part1 = 62842880);
    assert (!part2 = 57600000);
    cook "input";
    Printf.printf "%d\n" !part1;
    Printf.printf "%d\n" !part2;
