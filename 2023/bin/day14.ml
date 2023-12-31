open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

module Dish = struct
  let from_string input =
    let rows = String.split_lines input in
    List.map rows ~f:String.to_array |> Array.of_list

  let sort_rocks a b =
    match (a, b) with 'O', '.' -> -1 | '.', 'O' -> 1 | _, _ -> 0

  let tilt_y ~compare dish =
    for y = 0 to Array.length dish.(0) - 1 do
      Array.foldi dish ~init:[] ~f:(fun x col _ -> dish.(x).(y) :: col)
      |> List.rev
      |> List.group ~break:(fun _ b -> Char.equal b '#')
      |> List.map ~f:(fun l -> List.sort l ~compare)
      |> List.concat
      |> List.iteri ~f:(fun x v -> dish.(x).(y) <- v)
    done;
    dish

  let tilt_north dish = tilt_y dish ~compare:sort_rocks
  let tilt_south dish = tilt_y dish ~compare:(fun a b -> sort_rocks b a)

  let tilt_x ~compare dish =
    Array.map dish ~f:(fun arr ->
        Array.to_list arr
        |> List.group ~break:(fun _ b -> Char.equal b '#')
        |> List.map ~f:(fun l -> List.sort l ~compare)
        |> List.concat |> List.to_array)

  let tilt_west dish = tilt_x dish ~compare:sort_rocks
  let tilt_east dish = tilt_x dish ~compare:(fun a b -> sort_rocks b a)
  let cycle dish = tilt_north dish |> tilt_west |> tilt_south |> tilt_east

  let dish_to_str dish =
    String.concat ~sep:"\n"
      (Array.to_list dish
      |> List.map ~f:(fun a -> String.of_char_list (Array.to_list a)))

  let run_cycles amt dish =
    let cache = Hash_set.create (module String) in
    let cycle_length =
      Aoc.range amt
      |> List.fold_until ~init:dish
           ~f:(fun dish _ ->
             let str_dish = dish_to_str dish in
             if Hash_set.mem cache str_dish then Stop (Hash_set.length cache)
             else
               let _ = Hash_set.add cache str_dish in
               Continue (cycle dish))
           ~finish:(fun _ -> Hash_set.length cache)
    in
    printf "cycle_length: %d \n" cycle_length;
    let runs = amt mod cycle_length in
    Aoc.range (cycle_length + runs)
    |> List.fold ~init:dish ~f:(fun dish _ -> cycle dish)

  let print_dish dish =
    printf "%s" (dish_to_str dish);
    dish

  let calculate_load dish =
    let rows = Array.length dish in
    Array.foldi dish ~init:0 ~f:(fun x acc col ->
        let amt =
          Array.foldi col ~init:acc ~f:(fun y acc _ ->
              match dish.(x).(y) with 'O' -> rows - x + acc | _ -> acc)
        in
        amt)
end

let solve file =
  In_channel.read_all file |> Dish.from_string |> Dish.run_cycles 1000000000
  |> Dish.print_dish |> Dish.calculate_load

let () = printf "\nSolution 1: %d\n" (solve filename)
