open Core

(* let noop x = x *)

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

let digits =
  [
    "1";
    "2";
    "3";
    "4";
    "5";
    "6";
    "7";
    "8";
    "9";
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine";
  ]

let convert_digit d =
  match d with
  | "one" -> "1"
  | "two" -> "2"
  | "three" -> "3"
  | "four" -> "4"
  | "five" -> "5"
  | "six" -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine" -> "9"
  | d -> d

let find_digit line digit =
  match String.substr_index line ~pattern:digit with
  | None -> None
  | Some pos -> Some (pos, digit)

let compare_pos (x, _) (y, _) = compare x y
let compare_pos_rev (x, _) (y, _) = compare y x

let find_n_digit line f =
  let found_digits = List.map digits ~f:(find_digit line) |> List.filter_opt in
  let _, d = List.sort found_digits ~compare:f |> List.hd_exn in
  convert_digit d

let find_first_digit line = find_n_digit line compare_pos
let find_last_digit line = find_n_digit line compare_pos_rev
let filter_digits line = line |> String.filter ~f:Char.is_digit

let first_and_last line =
  let first_digit = find_first_digit line in
  let last_digit = find_last_digit line in
  first_digit ^ last_digit

let solve file =
  let contents = In_channel.read_all file in
  String.split_lines contents
  |> List.map ~f:filter_digits |> List.map ~f:first_and_last
  |> List.map ~f:Int.of_string |> List.fold ~init:0 ~f:( + )

let () =
  printf "Solution 1: %d\nSolution 2: %d\n" (solve filename) (solve filename)
