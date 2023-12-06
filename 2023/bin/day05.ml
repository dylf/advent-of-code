open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

(* dst src rng *)
type mapping = int * int * int

type almanac = {
  seeds : int list;
  seed_to_soil : mapping list;
  soil_to_fert : mapping list;
  fert_to_wat : mapping list;
  wat_to_lit : mapping list;
  lit_to_temp : mapping list;
  temp_to_hum : mapping list;
  hum_to_loc : mapping list;
}

let parse_seeds line =
  let _, seeds = String.lsplit2_exn line ~on:':' in
  String.split seeds ~on:' ' |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:Int.of_string

let mapping_to_tuple l =
  match l with x :: y :: z :: _ -> (x, y, z) | _ -> (0, 0, 0)

let rec parse_mappings lines mappings =
  match lines with
  | [] -> ([], mappings)
  | hd :: tl when String.is_empty hd -> (tl, mappings)
  | hd :: tl ->
      let mapping =
        String.split hd ~on:' ' |> List.map ~f:Int.of_string |> mapping_to_tuple
      in
      parse_mappings tl (mapping :: mappings)

let parse_almanac lines =
  let almanac =
    {
      seeds = [];
      seed_to_soil = [];
      soil_to_fert = [];
      fert_to_wat = [];
      wat_to_lit = [];
      lit_to_temp = [];
      temp_to_hum = [];
      hum_to_loc = [];
    }
  in
  let rec build_almanac almanac lines =
    match lines with
    | [] -> almanac
    | hd :: tl when String.is_empty hd -> build_almanac almanac tl
    | hd :: tl when String.is_prefix hd ~prefix:"seeds:" ->
        let a = { almanac with seeds = parse_seeds hd } in
        build_almanac a tl
    | hd :: tl ->
        let tl, mappings = parse_mappings tl [] in
        let a =
          match hd with
          | hd when String.is_prefix hd ~prefix:"seed-to-soil map:" ->
              { almanac with seed_to_soil = mappings }
          | hd when String.is_prefix hd ~prefix:"soil-to-fertilizer map:" ->
              { almanac with soil_to_fert = mappings }
          | hd when String.is_prefix hd ~prefix:"fertilizer-to-water map:" ->
              { almanac with fert_to_wat = mappings }
          | hd when String.is_prefix hd ~prefix:"water-to-light map:" ->
              { almanac with wat_to_lit = mappings }
          | hd when String.is_prefix hd ~prefix:"light-to-temperature map:" ->
              { almanac with lit_to_temp = mappings }
          | hd when String.is_prefix hd ~prefix:"temperature-to-humidity map:"
            ->
              { almanac with temp_to_hum = mappings }
          | hd when String.is_prefix hd ~prefix:"humidity-to-location map:" ->
              { almanac with hum_to_loc = mappings }
          | _ -> almanac
        in
        build_almanac a tl
  in
  build_almanac almanac lines

let find_dest_in_map e m =
  match
    List.find m ~f:(fun m ->
        match m with
        | _, src, rng when e >= src && e < src + rng -> true
        | _ -> false)
  with
  | Some (dst, src, _) -> e - src + dst
  | None -> e

let seed_to_loc almanac =
  almanac.seeds
  |> List.map ~f:(fun seed ->
         let soil = find_dest_in_map seed almanac.seed_to_soil in
         let fert = find_dest_in_map soil almanac.soil_to_fert in
         let wat = find_dest_in_map fert almanac.fert_to_wat in
         let lit = find_dest_in_map wat almanac.wat_to_lit in
         let temp = find_dest_in_map lit almanac.lit_to_temp in
         let hum = find_dest_in_map temp almanac.temp_to_hum in
         find_dest_in_map hum almanac.hum_to_loc)

let solve filename =
  let seed_locs =
    In_channel.read_all filename
    |> String.split_lines |> parse_almanac |> seed_to_loc
  in
  match List.min_elt seed_locs ~compare:Int.compare with
  | Some x -> x
  | None -> -1

let partition_overlapping_ranges range mapping =
  match (range, mapping) with
  (* mapping fully contains range*)
  | (x, y), (dst, src, rng) when x >= src && y < src + rng ->
      ([ (x - src + dst, y - src + dst) ], [])
      (* mapping contains start of range *)
  | (x, y), (dst, src, rng) when x >= src && x < src + rng ->
      ([ (x - src + dst, dst + rng - 1) ], [ (src + rng, y) ])
      (* mapping contains end of range *)
  | (x, y), (dst, src, rng) when x < src && y >= src && y < src + rng ->
      ([ (dst, y - src + dst) ], [ (x, src - 1) ])
      (* range fully contains mapping *)
  | (x, y), (dst, src, rng) when x < src && y >= src + rng ->
      ([ (dst, dst + rng - 1) ], [ (x, src - 1); (src + rng, y) ])
      (* mapping is not contained in range *)
  | (x, y), _ -> ([], [ (x, y) ])

let seed_range_to_loc almanac =
  let rec parse_ranges l acc =
    match l with
    | start :: len :: tl -> parse_ranges tl ((start, start + len - 1) :: acc)
    | _ -> acc
  in
  let seed_ranges = parse_ranges almanac.seeds [] in
  let rec find_ranges mappings src_ranges dst_ranges =
    match (mappings, src_ranges, dst_ranges) with
    | [], [], _ -> dst_ranges
    | [], _, _ -> src_ranges @ dst_ranges
    | map :: rest, hd :: tl, dst_ranges ->
        let dst, holes =
          List.fold map ~init:([], []) ~f:(fun (dest, holes) m ->
              let d, h = partition_overlapping_ranges hd m in
              (d @ dest, holes @ h))
        in
        find_ranges rest (tl @ holes) dst @ dst_ranges
    | _, _, _ -> src_ranges @ dst_ranges
  in
  find_ranges
    [
      almanac.seed_to_soil;
      almanac.soil_to_fert;
      almanac.fert_to_wat;
      almanac.wat_to_lit;
      almanac.lit_to_temp;
      almanac.temp_to_hum;
      almanac.hum_to_loc;
    ]
    seed_ranges []
  |> List.map ~f:(fun (x, _) -> x)

let solve2 filename =
  let seed_locs =
    In_channel.read_all filename
    |> String.split_lines |> parse_almanac |> seed_range_to_loc
  in
  match List.min_elt seed_locs ~compare:Int.compare with
  | Some x -> x
  | None -> -1

let () =
  printf "Solution 1: %d\nSolution 2: %d\n" (solve filename) (solve2 filename)
(* let () = solve filename *)
