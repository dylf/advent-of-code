open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

module Space = struct
  let find_galaxies input =
    String.split_lines input
    |> List.foldi ~init:[] ~f:(fun x acc line ->
           String.to_list line
           |> List.foldi ~init:acc ~f:(fun y acc c ->
                  match c with '#' -> (x, y) :: acc | _ -> acc))
    |> List.rev

  let get_dimensions input =
    let lines = String.split_lines input in
    let cols = List.hd_exn lines |> String.length in
    (List.length lines, cols)

  let expanded_rows galaxies dimensions =
    List.init (fst dimensions) ~f:(fun i -> i)
    |> List.fold ~init:[] ~f:(fun acc x ->
           let empty = List.exists galaxies ~f:(fun (gX, _) -> x = gX) in
           match empty with true -> acc | _ -> x :: acc)
    |> List.rev

  let expanded_cols galaxies dimensions =
    List.init (snd dimensions) ~f:(fun i -> i)
    |> List.fold ~init:[] ~f:(fun acc y ->
           let empty = List.exists galaxies ~f:(fun (_, gY) -> y = gY) in
           match empty with true -> acc | _ -> y :: acc)
    |> List.rev

  let galaxies_after_expansion ~expansion galaxies expanded_rows expanded_cols =
    List.map galaxies ~f:(fun (gX, gY) ->
        let x_delta =
          List.filter expanded_rows ~f:(fun x -> x < gX) |> List.length
        in
        let y_delta =
          List.filter expanded_cols ~f:(fun y -> y < gY) |> List.length
        in
        (gX + (x_delta * (expansion - 1)), gY + (y_delta * (expansion - 1))))

  let find_galaxies_after_expansion ~expansion input =
    let galaxies = find_galaxies input in
    let dimensions = get_dimensions input in
    let rows = expanded_rows galaxies dimensions in
    let cols = expanded_cols galaxies dimensions in
    galaxies_after_expansion ~expansion galaxies rows cols

  let shortest_path a b =
    match (a, b) with (x, y), (a, b) -> abs (b - y) + abs (a - x)

  let find_shortest_paths galaxies =
    let rec find_paths_pairs curr rem_galaxies rest_pairs acc =
      match (rem_galaxies, rest_pairs) with
      | [], [] -> acc
      | rem, hd :: tl ->
          shortest_path curr hd :: find_paths_pairs curr rem tl acc
      | hd :: tl, [] -> find_paths_pairs hd tl tl acc
    in
    match galaxies with
    | hd :: rem -> find_paths_pairs hd rem rem []
    | _ -> failwith "No galaxies found"
end

(* let solve file = *)
(*   let contents = In_channel.read_all file in *)
(*   let galaxies = Space.find_galaxies contents in *)
(*   List.iter galaxies ~f:(fun (x, y) -> printf "%d, %d\n" x y); *)
(*   let x, y = Space.get_dimensions contents in *)
(*   printf "%d * %d\n\n" x y; *)
(*   Space.expanded_rows galaxies (x, y) |> List.iter ~f:(fun d -> printf "%d  " d); *)
(*   printf "\n\n"; *)
(*   Space.expanded_cols galaxies (x, y) |> List.iter ~f:(fun d -> printf "%d  " d); *)
(*   printf "\n\nAfter:\n"; *)
(*   List.iteri *)
(*     (Space.find_shortest_paths (Space.find_galaxies_after_expansion contents)) *)
(*     ~f:(fun i x -> printf "i: %d %d \n" i x); *)
(*   printf "-----\n"; *)
(*   List.fold *)
(*     (Space.find_shortest_paths (Space.find_galaxies_after_expansion contents)) *)
(*     ~init:0 ~f:( + ) *)

let solve ?(expansion = 2) file =
  In_channel.read_all file
  |> Space.find_galaxies_after_expansion ~expansion
  |> Space.find_shortest_paths |> List.fold ~init:0 ~f:( + )

let () =
  printf "\nSolution 1: %d\nSolution 2: %d\n" (solve filename)
    (solve filename ~expansion:1000000)
