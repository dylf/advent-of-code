open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

let solve input =
  String.split_lines input
  (* Accumlate multiple lists of numbers separated by empty lines *)
  |> List.fold ~init:[ [] ] ~f:(fun acc line ->
         match line with
         | "" -> [] :: acc
         | _ ->
             let last = List.hd_exn acc in
             (Int.of_string line :: last) :: List.tl_exn acc)
     (* Sum each list of numbers *)
  |> List.map ~f:(List.fold ~init:0 ~f:( + ))
  (* Sort the sums *)
  |> List.sort ~compare:Int.compare
  (* Get the max *)
  |> List.last

(* Day 1 Part 1 of AOC 2022 *)
let run f =
  let input = In_channel.read_all f in
  let solution = solve input in
  match solution with
  | None -> printf "No input\n"
  | Some input -> printf "Max %d\n" input

let () = run filename
