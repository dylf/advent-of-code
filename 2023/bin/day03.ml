open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

type location = {
  str : string;
  (* The last position in the string *)
  pos : int * int;
}

let is_adj_pos part_pos sym_pos =
  let px, py = part_pos in
  let sx, sy = sym_pos in
  match (sx - px, sy - py) with
  | x, y when x < 2 && x > -2 && y < 2 && y > -2 -> true
  | _ -> false

let is_adj_loc part_loc sym_loc =
  let rec gen_part_pos acc (r, c) n =
    if n <= 0 then acc else gen_part_pos ((r, c - 1) :: acc) (r, c - 1) (n - 1)
  in
  gen_part_pos [ part_loc.pos ] part_loc.pos (String.length part_loc.str - 1)
  |> List.exists ~f:(fun p -> is_adj_pos p sym_loc.pos)

(* ..456..8..$ *)
let parse_chars row_i row_str =
  let _, res =
    String.foldi row_str ~init:(false, []) ~f:(fun col_i (in_num, l) c ->
        match c with
        | d when Char.is_digit d ->
            ( true,
              match l with
              | head :: tail when in_num ->
                  { str = head.str ^ String.make 1 d; pos = (row_i, col_i) }
                  :: tail
              | _ -> { str = String.make 1 d; pos = (row_i, col_i) } :: l )
        | '.' -> (false, l)
        | _ -> (false, { str = String.make 1 c; pos = (row_i, col_i) } :: l))
  in
  res

let is_symbol loc =
  String.length loc.str = 1
  && match int_of_string_opt loc.str with Some _ -> false | None -> true

let parse_input input =
  List.foldi input ~init:[] ~f:(fun row acc row_str ->
      parse_chars row row_str @ acc)
  |> List.partition_tf ~f:is_symbol

let solve file =
  let symbols, parts =
    In_channel.read_all file |> String.split_lines |> parse_input
  in
  List.filter parts ~f:(fun p ->
      List.exists symbols ~f:(fun s -> is_adj_loc p s))
  |> List.map ~f:(fun l -> l.str)
  |> List.map ~f:Int.of_string |> List.fold ~init:0 ~f:( + )

let solve2 file =
  let symbols, parts =
    In_channel.read_all file |> String.split_lines |> parse_input
  in
  let gears =
    (* Filter with (fun s -> s.str = "*") not working? *)
    List.filter symbols ~f:(fun s ->
        match s.str with "*" -> true | _ -> false)
  in
  List.map gears ~f:(fun g ->
      let adj =
        List.filter parts ~f:(fun p -> is_adj_loc p g)
        |> List.map ~f:(fun p -> p.str)
        |> List.map ~f:Int.of_string
      in
      if List.length adj = 2 then List.fold adj ~init:1 ~f:( * ) else 0)
  |> List.fold ~init:0 ~f:( + )

let () =
  printf "Solution 1: %d\nSolution 2: %d\n" (solve filename) (solve2 filename)
