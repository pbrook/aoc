open Aoc;;

module SS = Set.Make(String)

let parse filename =
    let m = Hashtbl.create 112 in
    let ic = open_in filename in
    try
        while true do
            let line = input_line ic in
            let ar = String.split_on_char ' ' line in
            match ar with
            | [a;_;action;amount;_;_;_;_;_;_;b] ->
                let b = String.sub b 0 (String.length b - 1) in
                let amount = (int_of_string amount) * if String.equal action "gain" then 1 else -1 in
                Hashtbl.replace m (a, b) amount
            | _ -> assert false
        done;
        assert false
    with End_of_file ->
        let peeps = Hashtbl.to_seq_keys m |> Seq.map fst |> SS.of_seq in
        (peeps, m)

let rec combs s =
    if SS.is_empty s then [[]]
    else
    let el = SS.elements s in
    let inner e = let ss = SS.remove e s in
        List.map (fun tail -> e::tail) (combs ss)
    in
    List.concat_map inner el

let part1 filename =
    let peeps, m = parse filename in
    let happy_pair (n, a) b =
        let h0 = Hashtbl.find m (a, b) in
        let h1 = Hashtbl.find m (b, a) in
        (n + h0 + h1, b)
    in let happy plan =
        let a = List.hd plan in
        let n, b = List.fold_left happy_pair (0, a) (List.tl plan) in
        fst (happy_pair (n, b) a)
    in let orders = combs peeps in
    fold1 max (List.map happy orders)

let () = 
    assert (part1 "test1" == 330);
    Printf.printf "%d\n" (part1 "input");
