type deer = {mutable score: int; mutable dist: int; speed: int; run: int; stop: int}

let parse line =
    let w = String.split_on_char ' ' line in
    let speed = int_of_string (List.nth w 3) in
    let run = int_of_string (List.nth w 6) in
    let stop = int_of_string (List.nth w 13) in
    {score=0; dist=0; speed; run; stop}

let rec readfile ic =
    try
        let line = input_line ic in
        parse line :: readfile ic
    with End_of_file -> []

let advance deer t =
    let bit = t mod (deer.run + deer.stop) in
    if bit < deer.run then deer.dist <- deer.dist + deer.speed

let add_score deer best =
    if deer.dist == best then deer.score <- deer.score + 1

let leader herd = List.fold_left (fun prev deer -> max prev deer.dist) 0 herd
let winner herd = List.fold_left (fun prev deer -> max prev deer.score) 0 herd

let race filename time =
    let herd = readfile (open_in filename) in
    for t = 0 to time-1 do
        List.iter (fun deer -> advance deer t) herd;
        let best = leader herd in
        List.iter (fun deer -> add_score deer best) herd
    done;
    herd

let () = 
    let test1 = race "test1" 1000 in
    assert (leader test1 == 1120);
    assert (winner test1 == 689);
    let input = race "input" 2503 in
    Printf.printf "%d\n" (leader input);
    Printf.printf "%d\n" (winner input);
