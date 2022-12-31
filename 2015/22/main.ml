let tot = ref 0

let parse filename =
    let hp = ref 0 and damage = ref 0 in
    let ic = open_in filename in
    try
        while true do
            let line = input_line ic in
            let ar = String.split_on_char ':' line in
            match ar with
            | ["Hit Points"; s] -> hp := int_of_string (String.trim s)
            | ["Damage"; s] -> damage := int_of_string (String.trim s)
            | _ -> assert false
        done;
        assert false
    with End_of_file -> (!hp, !damage)

type player_action = Missile | Drain | Shield | Poison | Recharge

exception Finished of int
let hard = ref false

let rec insert_gs cost gs tail = match tail with
    | [] -> [cost, gs]
    | ((tcost, _) as x)::xs ->
        if tcost < cost then
            x :: insert_gs cost gs xs
        else
            (cost, gs) :: tail

class ['a] search =
object (_)
    val ht = Hashtbl.create 1000
    val mutable sq = []
    val hits = ref 0
    method clear =
        sq <- [];
        Hashtbl.clear ht;
        tot := 0;
        hits := 0
    method add_state (gs : 'a) =
        let guess = gs#guess in
        let hash = gs#hash in
        let prev = match Hashtbl.find_opt ht hash with
        | None -> Int.max_int
        | Some ogs -> ogs
        in ignore (prev);
        if guess < prev then begin
            Hashtbl.replace ht hash guess;
            sq <- insert_gs guess gs sq;
        end
        else incr hits
    method search =
        try
            while sq <> [] do
                incr tot;
                let guess, gs = List.hd sq in
                ignore (guess);
                (*Printf.printf "%d %d " guess (List.length sq);*)
                sq <- List.tl sq;
                gs#play
            done;
            assert false
        with Finished mana -> mana
    method stats =
        Printf.printf "stats: %d %d %d\n" !tot (Hashtbl.length ht) !hits
end

let ss = new search

let action_cost = function
    | Missile -> 53
    | Drain -> 73
    | Shield -> 113
    | Poison -> 173
    | Recharge -> 229

class game_state (boss_hp, boss_damage) (player_hp, mana) action =
object (self)
    val mutable player_hp = player_hp
    val mutable boss_hp = boss_hp
    val mutable mana = mana - action_cost action
    val mutable spent = action_cost action
    val mutable stime = 0
    val mutable ptime = 0 
    val mutable rtime = 0
    method clone sp s p r =
        spent <- sp;
        stime <- s;
        ptime <- p;
        rtime <- r;
    method hash =
        (boss_hp, mana, player_hp, action, stime, ptime, rtime)
    method dump msg =
        Printf.printf "%s %d %d %d %d %d %d %d\n" msg boss_hp spent mana player_hp stime ptime rtime
    method try_action new_action =
        let cost = action_cost new_action in
        if mana >= cost then begin
            let sg = new game_state (boss_hp, boss_damage) (player_hp, mana) new_action in
            sg#clone (spent+cost) stime ptime rtime;
            (*sg#dump "I";*)
            ss#add_state sg
        end
    method guess =
        let rounds = boss_hp / (3 + 3 + 4) in
        spent + 53 * rounds
    method guess_dumb =
        spent
    method pick_action =
        self#try_action Missile;
        self#try_action Drain;
        if stime == 0 then self#try_action Shield;
        if ptime == 0 then self#try_action Poison;
        if rtime == 0 then self#try_action Recharge;

    method do_action = match action with
        | Missile -> boss_hp <- boss_hp - 4
        | Drain -> boss_hp <- boss_hp - 2; player_hp <- player_hp + 2
        | Shield -> stime <- 6
        | Poison -> ptime <- 6
        | Recharge -> rtime <- 5

    method start_turn =
        if stime > 0 then stime <- stime - 1;
        if ptime > 0 then begin
            boss_hp <- boss_hp - 3;
            ptime <- ptime - 1;
        end;
        if rtime > 0 then begin
            mana <- mana + 101;
            rtime <- rtime - 1;
        end;
        self#check_win
    method boss_action =
        let damage = boss_damage - if stime > 0 then 7 else 0 in
        player_hp <- player_hp - damage
    method check_win =
        if boss_hp <= 0 then begin
            raise (Finished spent)
        end
    method play =
        (*self#dump "P";*)
        self#do_action;
        self#check_win;
        self#start_turn;
        self#boss_action;
        if !hard then player_hp <- player_hp - 1;
        if player_hp > 0 then begin
            self#start_turn;
            self#pick_action;
        end
end

let fight boss player =
    ss#clear;
    List.iter (fun action -> ss#add_state (new game_state boss player action))
    [Missile; Drain; Shield; Poison; Recharge];
    ss#search

let () = 
    assert (fight (13, 8) (10, 250) == 173 + 53);
    assert (fight (14, 8) (10, 250) == 229 + 113 + 73 + 173 + 53);
    Printf.printf "Start\n%!";
    let boss = parse "input" in
    Printf.printf "%d\n%!" (fight boss (50, 500));
    (*ss#stats;*)
    hard := true;
    Printf.printf "%d\n" (fight boss (49, 500));
    (*ss#stats;*)
