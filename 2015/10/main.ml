let input = "1113222113"

let digit n = Char.chr (n + Char.code '0')

let say (n, prev, acc) c =
    if c == prev then
        (n + 1, prev, acc)
    else
        (1, c, prev :: digit n :: acc)

let seesay s =
    let first = List.hd s in
    let n, c, acc = List.fold_left say (1, first, []) (List.tl s) in
    List.rev (c :: digit n :: acc)

let say ?(n=1) s =
    let lst = List.of_seq (String.to_seq s) in
    let rec loop n v =
        (*Printf.printf "%d %d\n%!" n (List.length v) ;*)
        if n == 0 then v else loop (n - 1) (seesay v)
    in let res = loop n lst in
    String.of_seq (List.to_seq res)

let () = 
    assert (say "211" |> String.equal "1221") ;
    assert (say "1" |> String.equal "11");
    assert (say "11" |> String.equal "21");
    assert (say "21" |> String.equal "1211");
    assert (say "1211" |> String.equal "111221");
    assert (say "111221" |> String.equal "312211");
    Printf.printf "%d\n%!" (say ~n:40 input |> String.length);
    Printf.printf "%d\n" (say ~n:50 input |> String.length);
