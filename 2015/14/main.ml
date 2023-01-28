let parse line =
    let w = String.split_on_char ' ' line in
    let speed = List.nth w 3 in
    let run = List.nth w 6 in
    let stop = List.nth w 13 in
    (int_of_string speed, int_of_string run, int_of_string stop)

let part1 filename time =
    let best = ref 0 in
    let ic = open_in filename in
    try
        while true do
            let line = input_line ic in
            let (speed, run, stop) = parse line in
            let period = run + stop in
            let cycles = time / period in
            let bit = time mod period in
            let moving = cycles * run + (min bit run) in
            let dist = moving * speed in
            best := max !best dist
        done;
        assert false
    with End_of_file -> !best

let () = 
    assert (part1 "test1" 1000 == 1120);
    Printf.printf "%d\n" (part1 "input" 2503);
