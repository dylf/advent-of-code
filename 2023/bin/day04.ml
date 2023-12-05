open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

type card = { picks : int list; winning : int list }

let parse_nums num_str =
  String.split num_str ~on:' '
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:String.strip |> List.map ~f:Int.of_string

let parse_card line : card =
  let _, n = String.lsplit2_exn line ~on:':' in
  let winnings, picks = String.lsplit2_exn n ~on:'|' in
  { winning = parse_nums winnings; picks = parse_nums picks }

let calc_score card =
  List.fold card.winning ~init:0 ~f:(fun res n ->
      match List.mem card.picks n ~equal:(fun p n -> p = n) with
      | true when res > 0 -> res * 2
      | true when res = 0 -> 1
      | _ -> res)

let solve file =
  In_channel.read_all file |> String.split_lines |> List.map ~f:parse_card
  |> List.map ~f:calc_score |> List.fold ~init:0 ~f:( + )

let calc_winners card =
  List.filter card.winning ~f:(fun w ->
      List.mem card.picks w ~equal:(fun p w -> p = w))
  |> List.length

let count_winning_cards cards_list =
  (* accumulate the duplicates for subsequent cards *)
  let rec inc_k_els_by_n l k n =
    match (l, k) with
    | _, 0 -> l
    | [], _ -> List.init k ~f:(fun _ -> n)
    | hd :: tl, _ -> (hd + n) :: inc_k_els_by_n tl (k - 1) n
  in
  let rec count sum rem_cards copies =
    match (rem_cards, copies) with
    | [], _ -> sum
    | cur_card :: rest_cards, copies ->
        let cur_copies, rest_copies =
          match copies with [] -> (0, []) | hd :: tl -> (hd, tl)
        in
        let cur_count = 1 + cur_copies in
        let win_count = calc_winners cur_card in
        count (cur_count + sum) rest_cards
          (inc_k_els_by_n rest_copies win_count cur_count)
  in
  count 0 cards_list []

let solve2 file =
  In_channel.read_all file |> String.split_lines |> List.map ~f:parse_card
  |> count_winning_cards

let () =
  printf "Solution 1: %d\nSolution 2: %d\n" (solve filename) (solve2 filename)
