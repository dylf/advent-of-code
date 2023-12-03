open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

type rgb_cubes = int * int * int
type game = { id : int; results : rgb_cubes list }

let parse_game_id game_id_str =
  let _, id = String.lsplit2_exn game_id_str ~on:' ' in
  Int.of_string id

let parse_cube (cnt, clr) =
  let cnt = Int.of_string cnt in
  match (cnt, clr) with
  | cnt, "red" -> (cnt, 0, 0)
  | cnt, "green" -> (0, cnt, 0)
  | cnt, "blue" -> (0, 0, cnt)
  | _ -> (0, 0, 0)

(* 4 red, 2 blue *)
let parse_single_result results_str =
  String.split results_str ~on:','
  |> List.map ~f:String.strip
  |> List.map ~f:(String.lsplit2_exn ~on:' ')
  |> List.map ~f:parse_cube
  |> List.fold ~init:(0, 0, 0) ~f:(fun (r, g, b) (r1, g1, b1) ->
         (r + r1, g + g1, b + b1))

let parse_results results_str =
  String.split results_str ~on:';' |> List.map ~f:parse_single_result

let parse_game line : game =
  let game_id_str, game_results_str = String.lsplit2_exn line ~on:':' in
  { id = parse_game_id game_id_str; results = parse_results game_results_str }

let valid_result (r, g, b) (rM, gM, bM) = r <= rM && g <= gM && b <= bM

let valid_game max game =
  List.for_all game.results ~f:(fun r -> valid_result r max)

(* Problem 1: 12 red, 13 green, 14 blue *)
let solve1 file =
  let max = (12, 13, 14) in
  In_channel.read_all file |> String.split_lines |> List.map ~f:parse_game
  |> List.filter ~f:(valid_game max)
  |> List.map ~f:(fun g -> g.id)
  |> List.fold ~init:0 ~f:( + )

let min_cubes cubes =
  List.fold cubes ~init:(0, 0, 0) ~f:(fun (rM, gM, bM) (r, g, b) ->
      (max rM r, max gM g, max bM b))

let solve2 file =
  In_channel.read_all file |> String.split_lines |> List.map ~f:parse_game
  |> List.map ~f:(fun g -> min_cubes g.results)
  |> List.map ~f:(fun (x, y, z) -> x * y * z)
  |> List.fold ~init:0 ~f:( + )

let () =
  printf "Solution 1: %d\nSolution 2: %d\n" (solve1 filename) (solve2 filename)
