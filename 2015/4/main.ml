let goal = String.make 5 '0'

let rec mine data n =
    let md5 = Digest.string (data ^ Int.to_string n) |> Digest.to_hex in
    if String.starts_with ~prefix:goal md5 then
        n
    else begin
        mine data (n+1)
    end

let () = 
    assert (mine "abcdef" 1 == 609043);
    assert (mine "pqrstuv" 1 == 1048970);
    Printf.printf "%d\n" (mine "yzbqklnj" 1)
