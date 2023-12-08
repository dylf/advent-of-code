open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

type poker_hand =
  | FiveKind
  | FourKind
  | FullHouse
  | ThreeKind
  | TwoPair
  | OnePair
  | HighCard

let poker_hand_to_rank hand =
  match hand with
  | FiveKind -> 6
  | FourKind -> 5
  | FullHouse -> 4
  | ThreeKind -> 3
  | TwoPair -> 2
  | OnePair -> 1
  | HighCard -> 0

(*
   Convert each hand_str to a poker hand
   Comparator for poker hands
   Comparator for cards too
   Sort the list of input
*)

let parse line =
  match String.lsplit2_exn line ~on:' ' with
  | cards, bid -> (cards, Int.of_string bid)

let card_to_int ?(jokers = false) card =
  match card with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> if jokers then -1 else 11
  | 'T' -> 10
  | c -> Char.to_int c - Char.to_int '0'

let to_card_list hand_str joker_filter place_jokers =
  String.to_list hand_str |> joker_filter
  |> List.sort_and_group ~compare:(fun a b ->
         Int.compare (card_to_int a) (card_to_int b))
  |> List.map ~f:List.length
  |> List.sort ~compare:(fun a b -> Int.compare b a)
  |> place_jokers
  |> List.sort ~compare:(fun a b -> Int.compare b a)

let filter_jokers l =
  List.filter l ~f:(fun c -> match c with 'J' -> true | _ -> false)

let noop a = a

let place_jokers hand =
  let jokers = 5 - List.fold hand ~init:0 ~f:( + ) in
  let rec place_jokers_rec jokers acc rem =
    match (jokers, rem) with
    | 0, [] -> acc
    | 0, hd :: tl -> place_jokers_rec jokers (hd :: acc) tl
    | d, hd :: tl ->
        let count = hd + d in
        let jokers = count mod 5 in
        place_jokers_rec jokers (count :: acc) tl
    | _, [] -> -1 :: acc
  in
  place_jokers_rec jokers [] hand

let to_poker_hand ?(jokers = false) hand_str =
  let joker_filter, place_jokers =
    match jokers with
    | true -> (filter_jokers, place_jokers)
    | false -> (noop, noop)
  in
  match to_card_list hand_str joker_filter place_jokers with
  | [ 5 ] -> FiveKind
  | [ 4; 1 ] -> FourKind
  | [ 3; 2 ] -> FullHouse
  | 3 :: _ -> ThreeKind
  | [ 2; 2; 1 ] -> TwoPair
  | 2 :: _ -> OnePair
  | _ -> HighCard

let cmp_stronger_hand ?(jokers = false) a b =
  let a = String.to_list a |> List.map ~f:(fun c -> card_to_int ~jokers c) in
  let b = String.to_list b |> List.map ~f:(fun c -> card_to_int ~jokers c) in
  let rec first_diff a b =
    match (a, b) with
    | aHd :: aTl, bHd :: bTl -> (
        match Int.compare aHd bHd with 0 -> first_diff aTl bTl | d -> d)
    | _ -> 0
  in
  first_diff a b

let cmp_hands ?(jokers = false) a b =
  let aRank = poker_hand_to_rank (to_poker_hand a ~jokers) in
  let bRank = poker_hand_to_rank (to_poker_hand b ~jokers) in
  match Int.compare aRank bRank with
  | 0 -> cmp_stronger_hand a b ~jokers
  | r -> r

let solve input jokers =
  In_channel.read_all input |> String.split_lines |> List.map ~f:parse
  |> List.sort ~compare:(fun (a, _) (b, _) -> cmp_hands a b ~jokers)
  |> List.foldi ~init:0 ~f:(fun i acc (_, el) -> ((i + 1) * el) + acc)

(* |> List.map ~f:(fun (c, b) -> printf "Hand: %s Bid: %d\n" c b) *)
(* |> List.hd_exn *)
(* 0 *)

(* let () = solve filename true *)
let () =
  printf "Solution 1: %d\nSolution 2: %d\n" (solve filename false)
    (solve filename true)
