let part1 filename =
    let sum = ref 0 in
    let n = ref 0 in
    let op = ref (+) in
    let ic = open_in filename in
    try
        while true do
            let c = input_char ic in
            if c == '-' then
                op := (-)
            else if c >= '0' && c <= '9' then
                n := !n * 10 + Char.code c - Char.code '0'
            else begin
                sum := !op !sum !n;
                n := 0;
                op := (+)
            end
        done;
        assert false
    with End_of_file -> !op !sum !n

let nul = '\x00'
class buffer ic =
object (_)
    val mutable buf = nul
    method getc =
        if buf == nul then begin
            input_char ic
        end else begin
            let c = buf in
            buf <- nul;
            c
        end
    method ungetc c =
        assert (buf == nul);
        buf <- c
end

type json =
    | Int of int
    | String of string
    | Obj of (string * json) list
    | Array of json list

let rec pj buf =
    match buf#getc with
    | '"' -> String (pj_string buf)
    | '{' -> Obj (pj_obj buf)
    | '[' -> Array (pj_array buf)
    | c -> buf#ungetc c; Int (pj_int buf)

and pj_string buf =
    let rec loop () =
        let c = buf#getc in
        if c == '"' then
            []
        else
            c :: loop ()
    in
    loop () |> List.to_seq |> String.of_seq
and pj_int buf =
    let c = buf#getc in
    let sign = if c == '-' then -1 else begin
        buf#ungetc c;1
    end in
    let rec loop n =
        let c = buf#getc in
        if c >= '0' && c <= '9' then
            loop (n * 10 + Char.code c - Char.code '0')
        else begin
            buf#ungetc c ; n
        end
    in sign * (loop 0)
and pj_obj buf =
    assert (buf#getc == '"');
    let k = pj_string buf in
    assert (buf#getc == ':');
    let v = pj buf in
    let c = buf#getc in
    if c == ',' then
        ((k, v) :: pj_obj buf)
    else begin
        assert (c == '}');
        [(k, v)]
    end
and pj_array buf =
    let v = pj buf in
    let c = buf#getc in
    if c == ',' then
        v :: pj_array buf
    else begin
        assert (c == ']');
        [v]
    end

let jp = function
    | Int i -> string_of_int i
    | String s -> "\"" ^ s ^ "\""
    | Array _ -> "Array"
    | Obj _ -> "Obj"

let rec fold_json skip_red acc (j : json) =
    match j with
    | Int n -> acc + n
    | String _ -> acc
    | Array ar -> List.fold_left (fold_json skip_red) acc ar
    | Obj o -> let skip = if skip_red then
            List.exists (fun (_,v) -> match v with | String "red" -> true | _ -> false) o
        else false in if skip then acc else
            List.fold_left (fun a (_, v) -> fold_json skip_red a v) acc o

let part2 filename =
    let json = pj (new buffer (open_in filename)) in
    let p1 = fold_json false 0 json in
    Printf.printf "%d\n" p1 ;
    fold_json true 0 json

let () = 
    ignore (jp (Int 0));
    Printf.printf "%d\n" (part1 "input");
    Printf.printf "%d\n" (part2 "input");
