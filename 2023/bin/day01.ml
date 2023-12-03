open Core

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

let noop x = x

let find_digit_occurrences line digit =
  (String.substr_index_all line ~may_overlap:true ~pattern:digit, digit)

let min l = List.min_elt l ~compare:(fun x y -> compare x y) |> Option.value_exn
let max l = List.max_elt l ~compare:(fun x y -> compare x y) |> Option.value_exn
let cmp_first (x, _) (y, _) = compare (min x) (min y)
let cmp_last (x, _) (y, _) = compare (max y) (max x)
let filter_char_digits line = line |> String.filter ~f:Char.is_digit

let find_n_digit line cmp =
  let found_digits =
    List.map digits ~f:(find_digit_occurrences line)
    |> List.filter ~f:(fun (x, _) -> List.is_empty x |> not)
  in
  let _, d = List.sort found_digits ~compare:cmp |> List.hd_exn in
  convert_digit d

let find_first_digit line = find_n_digit line cmp_first
let find_last_digit line = find_n_digit line cmp_last
let first_and_last line = find_first_digit line ^ find_last_digit line

let solve file f =
  let contents = In_channel.read_all file in
  String.split_lines contents
  |> List.map ~f |> List.map ~f:first_and_last |> List.map ~f:Int.of_string
  |> List.fold ~init:0 ~f:( + )

let () =
  printf "Solution 1: %d\nSolution 2: %d\n"
    (solve filename filter_char_digits)
    (solve filename noop)
