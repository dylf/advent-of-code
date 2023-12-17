open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

module Point : sig
  type t = int * int

  val compare : t -> t -> int
  val sexp_of_t : t -> Sexp.t
  val hash : t -> int
end = struct
  type t = int * int

  let compare x y = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare x y
  let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t
  let hash = Hashtbl.hash
end

module Map = struct
  let is_pipe c =
    match c with '|' | '-' | 'L' | 'J' | '7' | 'F' | 'S' -> true | _ -> false

  let connected_tiles pipe (row, col) =
    match pipe with
    | '|' -> [ (row - 1, col); (row + 1, col) ]
    | '-' -> [ (row, col - 1); (row, col + 1) ]
    | 'L' -> [ (row, col + 1); (row - 1, col) ]
    | 'J' -> [ (row, col - 1); (row - 1, col) ]
    | '7' -> [ (row + 1, col); (row, col - 1) ]
    | 'F' -> [ (row + 1, col); (row, col + 1) ]
    | _ -> []

  let from_input input =
    let map = Hashtbl.create (module Point) in
    let start =
      String.split_lines input
      |> List.foldi ~init:(-1, -1) ~f:(fun x start s ->
             String.to_list s
             |> List.foldi ~init:start ~f:(fun y start c ->
                    match c with
                    | p when is_pipe p -> (
                        let pos = (x, y) in
                        Hashtbl.add_exn map ~key:pos
                          ~data:(connected_tiles p pos);
                        match p with 'S' -> pos | _ -> start)
                    | _ -> start))
    in
    (start, map)

  let find_cycle (startX, startY) map =
    let rec find_cycle_inner pos acc =
      match (pos, acc) with
      | (x, y), acc when startX = x && startY = y -> acc
      | (x, y), acc -> (
          match Hashtbl.find map (x, y) with
          | Some next ->
              let next =
                match acc with
                | [] -> failwith "No previous"
                | last :: _ ->
                    List.filter next ~f:(fun (lX, lY) ->
                        not (lX = fst last && lY = snd last))
              in
              find_cycle_inner (List.hd_exn next) (pos :: acc)
          | None -> [])
    in
    [ (0, 1); (0, -1); (-1, 0); (1, 0) ]
    |> List.map ~f:(fun (dX, dY) ->
           find_cycle_inner (startX + dX, startY + dY) [ (startX, startY) ])
    |> List.sort ~compare:(fun a b -> List.length b - List.length a)
    |> List.hd_exn

  let steps_to_furthest_point start map =
    let steps_in_cycle = find_cycle start map |> List.length in
    (steps_in_cycle / 2) + (steps_in_cycle mod steps_in_cycle / 2)

  let convert_start_to_pipe cycle start =
    let rec find_connections l =
      match l with
      | [ p; _ ] -> p
      | _ :: tl -> find_connections tl
      | [] -> failwith "No cycle"
    in
    let connections =
      match cycle with
      | first :: tl -> (first, find_connections tl)
      | [] -> failwith "No cycle"
    in
    let diff point =
      match (point, start) with (pX, pY), (sX, sY) -> (sX - pX, sY - pY)
    in
    match diff (fst connections) with
    | -1, 0 -> (
        match diff (snd connections) with
        | 1, 0 -> '|'
        | 0, 1 -> '7'
        | 0, -1 -> 'F'
        | _ -> failwith "Bad connection")
    | 1, 0 -> (
        match diff (snd connections) with
        | -1, 0 -> '|'
        | 0, 1 -> 'J'
        | 0, -1 -> 'L'
        | _ -> failwith "Bad connection")
    | 0, 1 -> (
        match diff (snd connections) with
        | 1, 0 -> 'L'
        | -1, 0 -> 'F'
        | 0, -1 -> '-'
        | _ -> failwith "Bad connection")
    | 0, -1 -> (
        match diff (snd connections) with
        | 1, 0 -> 'J'
        | -1, 0 -> '7'
        | 0, 1 -> '-'
        | _ -> failwith "Bad connection")
    | _ -> failwith "Bad connection"
end

let solve2 file =
  let contents = In_channel.read_all file in
  let start, map = Map.from_input contents in
  let loop = Map.find_cycle start map in
  let start_pipe = Map.convert_start_to_pipe loop start in
  String.split_lines contents
  |> List.foldi ~init:0 ~f:(fun x count s ->
         let cnt, _, _ =
           String.to_list s
           |> List.foldi ~init:(count, false, '_')
                ~f:(fun y (count, in_loop, last_corner) c ->
                  let c = match c with 'S' -> start_pipe | c -> c in
                  match List.find loop ~f:(fun (a, b) -> a = x && b = y) with
                  | Some _ -> (
                      match (c, last_corner) with
                      | 'J', 'F' | '7', 'L' -> (count, in_loop, '_')
                      | 'F', _ | 'L', _ -> (count, not in_loop, c)
                      | '7', _ | 'J', _ | '|', _ | 'S', _ ->
                          (count, not in_loop, '_')
                      | _ -> (count, in_loop, last_corner))
                  | None ->
                      if in_loop then (count + 1, in_loop, last_corner)
                      else (count, in_loop, last_corner))
         in
         cnt)

let solve file =
  let start, map = In_channel.read_all file |> Map.from_input in
  Map.steps_to_furthest_point start map

let () =
  printf "\nSolution 1: %d\nSolution 2: %d\n" (solve filename) (solve2 filename)
