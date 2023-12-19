open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

module CondRecord = struct
  let get_arrangements (springs, groups) =
    let rec get_arr prev_broken springs rem_groups =
      match (springs, rem_groups) with
      | [], [] | [], 0 :: [] -> 1
      | [], _ :: _ -> 0
      | hd :: tl, [] -> ( match hd with '#' -> 0 | _ -> get_arr false tl [])
      | spring :: rem_springs, count :: rem_groups -> (
          match prev_broken with
          | true -> (
              match (spring, count) with
              | '#', 0 -> 0
              | '.', d when d > 0 -> 0
              | '?', 0 | '.', 0 -> get_arr false rem_springs rem_groups
              | '#', d | '?', d ->
                  get_arr true rem_springs ((d - 1) :: rem_groups)
              | _, _ -> failwith "Bad character in input")
          | false -> (
              match (spring, count) with
              | '#', d -> get_arr true rem_springs ((d - 1) :: rem_groups)
              | '.', _ -> get_arr false rem_springs (count :: rem_groups)
              | '?', d ->
                  get_arr true rem_springs ((d - 1) :: rem_groups)
                  + get_arr false rem_springs (d :: rem_groups)
              | _, _ -> failwith "Bad input"))
    in
    get_arr false (String.to_list springs) groups

  let line_to_record line =
    let springs, groups = String.lsplit2_exn line ~on:' ' in
    let groups = String.split groups ~on:',' |> List.map ~f:Int.of_string in
    (springs, groups)

  let from_input input = String.split_lines input |> List.map ~f:line_to_record

  let line_to_record_5_times line =
    let springs, groups = String.lsplit2_exn line ~on:' ' in
    let springs = List.init 5 ~f:(fun _ -> springs) |> String.concat ~sep:"?" in
    let groups = List.init 5 ~f:(fun _ -> groups) |> String.concat ~sep:"," in
    let groups = String.split groups ~on:',' |> List.map ~f:Int.of_string in
    (springs, groups)

  let from_input_2 input =
    String.split_lines input |> List.map ~f:line_to_record_5_times
end

let solve file =
  In_channel.read_all file |> CondRecord.from_input
  |> List.map ~f:CondRecord.get_arrangements
  |> List.fold ~init:0 ~f:( + )

let solve2 file =
  In_channel.read_all file |> CondRecord.from_input_2
  |> List.map ~f:CondRecord.get_arrangements
  |> List.fold ~init:0 ~f:( + )

let () =
  printf "\nSolution 1: %d\nSolution 2: %d\n" (solve filename) (solve2 filename)
(* let () = printf "\nSolution 1: %d\n" (solve filename) *)
