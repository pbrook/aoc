let input = "1113222113"

let rec say ?(n=0) s =
    let get x = try s.[x] with Invalid_argument _ -> ' ' in
    let c = get n in
    if c == ' ' then
        ""
    else begin
        let rec same x = if get (n + x) == c then same (x+1) else x in
        let count = same 1 in
        (string_of_int count) ^ (String.make 1 c) ^ (say ~n:(n + count) s)
    end

let run s n =
    let rec loop s n = Printf.printf "%d %d\n%!" n (String.length s) ; match n with
        | 0 -> s
        | _ -> loop (say s) (n-1)
    in String.length (loop s n)

let () = 
    assert (say "211" |> String.equal "1221") ;
    assert (say "1" |> String.equal "11");
    assert (say "11" |> String.equal "21");
    assert (say "21" |> String.equal "1211");
    assert (say "1211" |> String.equal "111221");
    assert (say "111221" |> String.equal "312211");
    Printf.printf "%d" (run input 40);
