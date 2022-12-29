type value =
    | Int of int
    | Name of string
    | Op of string * value * value

let parse_value s = match int_of_string_opt s with
    | Some i -> Int i
    | None -> Name s

let parse line =
    let pv = parse_value in
    match String.split_on_char ' ' line with
    | [a; op; b; "->"; dest] -> (dest, Op (op, pv a, pv b))
    | ["NOT"; a; "->"; dest] -> (dest, Op ("NOT", pv a, Int 0))
    | [a; "->"; dest] -> (dest, pv a)
    | _ -> assert false

let solve circ =
    let again = ref true in
    let get name = match Hashtbl.find circ name with
        | Int i -> Some i
        | _ -> None
    in
    let rec subst v = match v with
        | Int _ -> v
        | Name s -> begin
            match get s with
            | Some i -> again := true; Int i
            | None -> v
        end
        | Op (op, av, bv) -> match (subst av, subst bv) with
            | (Int a, Int b) -> begin
                again := true; 
                let i = match op with
                | "NOT" -> lnot a
                | "OR" -> a lor b
                | "AND" -> a land b
                | "LSHIFT" -> a lsl b
                | "RSHIFT" -> a lsr b
                | _ -> assert false
                in Int (i land 0xffff)
            end
            | _ -> v
    in
    while !again do
        again := false;
        Hashtbl.filter_map_inplace (fun _ v -> Some (subst v)) circ
    done;
    Option.get (get "a")

let load filename =
    let circ = Hashtbl.create 400 in
    let ic = open_in filename in
    try
        while true do
            let line = input_line ic in
            let (dest, gate) = parse line in
            Hashtbl.replace circ dest gate
        done;
        assert false
    with End_of_file -> circ

let part1 filename = solve (load filename)

let part2 filename =
    let circ = load filename in
    let c2 = Hashtbl.copy circ in
    let a1 = solve circ in
    Hashtbl.replace c2 "b" (Int a1);
    solve c2
    

let () = 
    Printf.printf "%d\n" (part1 "input");
    Printf.printf "%d\n" (part2 "input");
