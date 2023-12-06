open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

let string_not_empty str = not (String.is_empty str)

let int_list_from_string str =
  String.split str ~on:' ' |> List.map ~f:String.strip
  |> List.filter ~f:string_not_empty
  |> List.map ~f:Int.of_string

let int_from_string str = String.filter str ~f:Char.is_digit |> Int.of_string

let parse_list file =
  let inputs =
    String.split_lines file
    |> List.filter ~f:string_not_empty
    |> List.map ~f:(fun s ->
           let _, v = String.lsplit2_exn s ~on:':' in
           v)
    |> List.map ~f:int_list_from_string
  in
  let time, dist =
    match inputs with [ x; y ] -> (x, y) | _ -> ([ -1 ], [ -1 ])
  in
  List.zip_exn time dist

let parse_int file =
  let inputs =
    String.split_lines file
    |> List.filter ~f:string_not_empty
    |> List.map ~f:(fun s ->
           let _, v = String.lsplit2_exn s ~on:':' in
           v)
    |> List.map ~f:int_from_string
  in
  match inputs with [ x; y ] -> [ (x, y) ] | _ -> [ (-1, -1) ]

(* Brute force og *)
(* let find_ways_to_win (time, dist) = *)
(*   List.init time ~f:(fun n -> n) *)
(*   |> List.filter ~f:(fun hold -> *)
(*          let rem = time - hold in *)
(*          if hold * rem > dist then true else false) *)
(*   |> List.length *)

(* a = 1 b = time c = dist *)
let find_ways_to_win_but_smarter (time, dist) =
  let discriminant = float_of_int ((time * time) - (4 * 1 * dist)) in
  let sqrt_discriminant = sqrt discriminant in
  let root1 = (-.float_of_int time +. sqrt_discriminant) /. 2.0 in
  let root2 = (-.float_of_int time -. sqrt_discriminant) /. 2.0 in
  (Float.round_down root1 |> int_of_float)
  - (Float.round_up root2 |> int_of_float)

let product l = List.fold l ~init:1 ~f:( * )

let solve file ~parse =
  let contents = In_channel.read_all file in
  parse contents |> List.map ~f:find_ways_to_win_but_smarter |> product

let () =
  printf "Solution 1: %d\nSolution 2: %d\n"
    (solve filename ~parse:parse_list)
    (solve filename ~parse:parse_int)
