open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

let string_not_empty str = not (String.is_empty str)

let int_list_from_string str =
  String.split str ~on:' ' |> List.map ~f:String.strip
  |> List.filter ~f:string_not_empty
  |> List.map ~f:Int.of_string

let parse file =
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

let find_ways_to_win (time, dist) =
  List.init time ~f:(fun n -> n)
  |> List.map ~f:(fun hold ->
         let rem = time - hold in
         if hold * rem > dist then true else false)
  |> List.filter ~f:(fun b -> b)
  |> List.length

let product l = List.fold l ~init:1 ~f:( * )

(* let prnt_dbg l =  *)
(*   List.map l ~f:(fun (t, d) -> printf "T:%d -> D%d" t d) *)
(*   |> List.hd_exn *)

(* let prnt_dbg l = List.map l ~f:(fun w -> printf "W:%d\n" w) |> List.hd_exn *)

let solve file =
  let contents = In_channel.read_all file in
  parse contents |> List.map ~f:find_ways_to_win |> product

let () = printf "Solution 1: %d\n" (solve filename)
(* printf "Solution 1: %d\nSolution 2: %d\n" *)
