open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

type node = { l : string; r : string }

let parse_mapping line =
  match
    String.split_on_chars line ~on:[ '='; '('; ','; ')'; ' ' ]
    |> List.filter ~f:(fun s -> not (String.is_empty s))
  with
  | [ k; l; r ] -> (k, { l; r })
  | _ -> ("", { l = ""; r = "" })

let parse input =
  let instructions, input = String.lsplit2_exn input ~on:'\n' in
  let mappings =
    String.split_lines input
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:parse_mapping
    (* |> List.map ~f:(fun (a, b) -> printf "Line: %s %s %s\n\n" a b.l b.r) *)
    (* |> List.hd_exn *)
  in

  let mappings =
    List.fold mappings
      ~init:(Hashtbl.create ~size:(List.length mappings) (module String))
      ~f:(fun acc (k, v) ->
        Hashtbl.add_exn acc ~key:k ~data:v;
        acc)
  in
  (instructions, mappings)

let walk_map instructions mappings =
  let inst_list = String.to_list instructions in
  let rec walk_map_inner instructions cur_node acc =
    match (instructions, cur_node) with
    | [], "ZZZ" -> acc
    | [], n -> walk_map_inner inst_list n acc
    | hd :: tl, n -> (
        match hd with
        | 'L' -> walk_map_inner tl (Hashtbl.find_exn mappings n).l acc + 1
        | 'R' -> walk_map_inner tl (Hashtbl.find_exn mappings n).r acc + 1
        | _ -> 0)
  in
  walk_map_inner inst_list "AAA" 0

let walk_map_multi instructions mappings =
  let starting_nodes =
    Hashtbl.keys mappings
    |> List.filter ~f:(fun s -> String.is_suffix s ~suffix:"A")
  in
  let inst_list = String.to_list instructions in
  let rec walk_map_inner instructions cur_nodes acc =
    match (instructions, cur_nodes) with
    | _, n when List.for_all n ~f:(fun s -> String.is_suffix s ~suffix:"Z") ->
        acc
    | [], nodes -> walk_map_inner inst_list nodes acc
    | hd :: tl, n ->
        let next =
          match hd with
          | 'L' -> List.map n ~f:(fun n -> (Hashtbl.find_exn mappings n).l)
          | 'R' -> List.map n ~f:(fun n -> (Hashtbl.find_exn mappings n).r)
          | _ -> []
        in
        walk_map_inner tl next acc + 1
  in
  walk_map_inner inst_list starting_nodes 0

let solve ?(walk_map = walk_map) file =
  let instructions, mappings = In_channel.read_all file |> parse in
  walk_map instructions mappings

let () = printf "Solution 1: %d\n" (solve filename ~walk_map:walk_map_multi)
