let parse x y z = (x, y, z)

let rec readfile ic =
    try
        let x = Scanf.bscanf ic "%dx%dx%d\n" parse in
        x :: readfile ic
    with End_of_file -> []

let sum = List.fold_left (+) 0

let min = List.fold_left (fun a b -> if a <= b then a else b) Int.max_int

let comb f (x, y, z) = [f x y; f y z; f x z]

let part1 box =
    let sz = comb ( * ) box in
    sum sz * 2 + min sz

let part2 ((x,y,z) as box) =
    let side = 2 * min (comb (+) box) in
    let vol = x*y*z in
    vol + side
    
let wrap part filename =
    let inp = readfile (Scanf.Scanning.open_in filename) in
    sum (List.map part inp)

let () = 
    assert (wrap part1 "test1" == 58);
    assert (wrap part2 "test1" == 34);
    Printf.printf "%d\n" (wrap part1 "input");
    Printf.printf "%d\n" (wrap part2 "input")
