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
let sq = ref []

let rec insert_gs cost gs tail = match tail with
    | [] -> [cost, gs]
    | ((tcost, _) as x)::xs ->
        if tcost < cost then
            x :: insert_gs cost gs xs
        else
            (cost, gs) :: tail

let add_state gs = sq := insert_gs gs#guess gs !sq

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
    method dump msg =
        ignore(msg);
        ()(*
        Printf.printf "%s %d %d %d %d %d\n" msg spent mana player_hp boss_hp ptime;
        *)
    method try_action new_action =
        let cost = action_cost new_action in
        if mana >= cost then begin
            let sg = new game_state (boss_hp, boss_damage) (player_hp, mana) new_action in
            sg#clone (spent+cost) stime ptime rtime;
            sg#dump "I";
            add_state sg
        end
    method guess =
        let r = boss_hp mod 18 in
        let poison = boss_hp / 18 in
        let missile = max 3 (r/4) in
        spent + 53 * missile + 173 * poison
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
        self#dump "P";
        self#do_action;
        self#check_win;
        self#start_turn;
        self#boss_action;
        if player_hp > 0 then begin
            self#start_turn;
            self#pick_action;
        end
end

let part1 boss player =
    assert (snd boss == 8);
    sq := [];
    List.iter (fun action -> add_state (new game_state boss player action))
    [Missile; Drain; Shield; Poison; Recharge];
    try
        while true do
            let lst = !sq in
            assert (lst <> []);
            let sg = snd (List.hd lst) in
            sq := List.tl lst;
            sg#play
        done;
        assert false
    with Finished mana -> mana

let () = 
    assert (part1 (13, 8) (10, 250) == 173 + 53);
    assert (part1 (14, 8) (10, 250) == 229 + 113 + 73 + 173 + 53);
    Printf.printf "%d\n" (part1 (parse "input") (50, 500));
