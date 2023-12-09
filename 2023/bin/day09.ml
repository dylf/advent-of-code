open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

module Parser = struct
  let to_int_list_list str =
    String.split_lines str
    |> List.map ~f:(fun s ->
           String.split ~on:' ' s |> List.map ~f:String.strip
           |> List.filter ~f:(fun s -> not (String.is_empty s))
           |> List.map ~f:Int.of_string)

  exception BadInput
end

let rec extrapolate_next_val ?(in_seq = false) l acc =
  match l with
  | l when (not in_seq) && List.for_all l ~f:(fun n -> n = 0) -> 0
  | [] -> raise Parser.BadInput
  | a :: [] ->
      let acc = List.rev acc in
      a + extrapolate_next_val acc []
  | a :: b :: tl -> extrapolate_next_val ~in_seq:true (b :: tl) ((b - a) :: acc)

let noop a = a

let solve ?(f = noop) file =
  In_channel.read_all file |> Parser.to_int_list_list |> List.map ~f
  |> List.map ~f:(fun l -> extrapolate_next_val l [])
  |> List.fold ~init:0 ~f:( + )

let () =
  printf "Solution 1: %d\nSolution2: %d\n" (solve filename)
    (solve filename ~f:List.rev)
