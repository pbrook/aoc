let input = "yzbqklnj"

let goal = String.make 5 '0'

let mine data len =
    let goal = String.make len '0' in
    let rec find n =
        let md5 = Digest.string (data ^ Int.to_string n) |> Digest.to_hex in
        if String.starts_with ~prefix:goal md5 then
            n
        else
            find (n + 1)
    in
    find 1

let () = 
    assert (mine "abcdef" 5 == 609043);
    assert (mine "pqrstuv" 5 == 1048970);
    Printf.printf "%d\n%!" (mine input 5);
    Printf.printf "%d\n" (mine input 6)
