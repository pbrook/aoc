module SS = Set.Make(String)

let rec combs s =
    if SS.is_empty s then [[]]
    else
    let el = SS.elements s in
    let inner e = let ss = SS.remove e s in
        List.map (fun tail -> e::tail) (combs ss)
    in
    List.concat_map inner el

let part1 filename =
    let m = Hashtbl.create 7 in
    let all = ref SS.empty in
    let ic = open_in filename in
    let addh a b dist =
        let mm = match Hashtbl.find_opt m a with
        | Some x -> x
        | None -> let x = Hashtbl.create 7 in
            Hashtbl.replace m a x ; x
        in Hashtbl.replace mm b (int_of_string dist)
    in
    let parse line = 
        match String.split_on_char ' ' line with
        | [a; "to"; b; "=" ; dist] -> 
            addh a b dist ;
            addh b a dist ;
            all := SS.add a !all |> SS.add b
        | _ ->assert false
    in
    try
        while true do
            let line = input_line ic in
            parse line
        done;
        assert false
    with End_of_file -> begin
        let add_dist (prev, d0) pos =
            let d = Hashtbl.find (Hashtbl.find m prev) pos in
            (pos, d0 + d)
        in
        let route_dist s = 
            List.fold_left add_dist (List.hd s, 0) (List.tl s) |> snd
        in
        List.fold_left min Int.max_int (List.map route_dist (combs !all))
    end

let () = 
    assert (part1 "test1" == 605) ;
    Printf.printf "%d\n" (part1 "input");
