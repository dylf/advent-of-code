open Core

let filename =
  let args = Sys.get_argv () in
  Array.get args 1

module Patterns = struct
  let from_input input =
    String.split ~on:'\n' input
    |> List.group ~break:(fun _ b -> String.is_empty b)
    |> List.map ~f:(fun s ->
           List.filter s ~f:(fun s -> not (String.is_empty s)))
    |> List.filter ~f:(fun l -> not (List.is_empty l))

  let rec is_mirror_smudges p1 p2 smudges =
    match (p1, p2) with
    | _, _ when smudges > 1 -> smudges
    | p1_hd :: p1_tl, p2_hd :: p2_tl ->
        is_mirror_smudges p1_tl p2_tl
          (smudges + if Char.equal p1_hd p2_hd then 0 else 1)
    | [], _ | _, [] -> smudges

  let horizontal_reflection_split pattern =
    let rec is_mirror p1 p2 =
      match (p1, p2) with
      | p1_hd :: p1_tl, p2_hd :: p2_tl when String.equal p1_hd p2_hd ->
          is_mirror p1_tl p2_tl
      | [], _ | _, [] -> true
      | _, _ -> false
    in
    List.groupi pattern ~break:(fun i a b ->
        if String.compare a b = 0 then
          let p1, p2 = List.split_n pattern i in
          is_mirror (List.rev p1) p2
        else false)

  let count_diff_chars s1 s2 =
    let strs = List.zip_exn (String.to_list s1) (String.to_list s2) in
    List.count strs ~f:(fun (a, b) -> not (Char.equal a b))

  let horizontal_reflection_split_smudge pattern =
    let rec is_mirror p1 p2 smudges =
      match (p1, p2) with
      | _, _ when smudges > 1 -> smudges
      | p1_hd :: p1_tl, p2_hd :: p2_tl ->
          let smudges =
            is_mirror_smudges (String.to_list p1_hd) (String.to_list p2_hd)
              smudges
          in
          is_mirror p1_tl p2_tl smudges
      | [], _ | _, [] -> smudges
    in
    List.groupi pattern ~break:(fun i a b ->
        let smudges = count_diff_chars a b in
        if smudges < 2 then
          let p1, p2 = List.split_n pattern i in
          let p1 = List.rev p1 in
          let smudges = is_mirror p2 p1 0 in
          smudges = 1
        else false)

  let horizontal_result_above ?(smudges = false) pattern =
    let fn =
      if smudges then horizontal_reflection_split_smudge
      else horizontal_reflection_split
    in
    let split = fn pattern in
    match split with
    | [ a; _ ] -> List.length a * 100
    | [] | [ _ ] -> 0
    | _ -> failwith (string_of_int (List.length split))

  let is_vertical_mirror pattern i pattern_length =
    let rec is_mirror p1 p2 =
      match (p1, p2) with
      | p1_hd :: p1_tl, p2_hd :: p2_tl when Char.equal p1_hd p2_hd ->
          is_mirror p1_tl p2_tl
      | [], _ | _, [] -> true
      | _, _ -> false
    in
    List.for_all pattern ~f:(fun s ->
        let left = String.sub s ~pos:0 ~len:i |> String.to_list |> List.rev in
        let right =
          String.sub s ~pos:i ~len:(pattern_length - i) |> String.to_list
        in
        is_mirror left right)

  let is_vertical_mirror_smudges pattern i pattern_length =
    List.fold_until ~init:0 pattern
      ~f:(fun smudges s ->
        let left = String.sub s ~pos:0 ~len:i |> String.to_list |> List.rev in
        let right =
          String.sub s ~pos:i ~len:(pattern_length - i) |> String.to_list
        in
        if smudges > 1 then Stop smudges
        else Continue (is_mirror_smudges left right smudges))
      ~finish:(fun smudges -> smudges)
    = 1

  let vertical_result_left ?(smudges = false) pattern =
    let pattern_length = String.length (List.hd_exn pattern) in
    let split =
      Aoc.range (pattern_length - 1)
      |> List.find ~f:(fun i ->
             let i = i + 1 in
             if smudges then is_vertical_mirror_smudges pattern i pattern_length
             else is_vertical_mirror pattern i pattern_length)
    in
    match split with Some x -> x + 1 | None -> 0
end

let solve ?(smudges = false) file =
  In_channel.read_all file |> Patterns.from_input
  |> List.map ~f:(fun p ->
         Patterns.vertical_result_left ~smudges p
         + Patterns.horizontal_result_above ~smudges p)
  |> List.fold ~init:0 ~f:( + )

let () =
  printf "\nSolution 1: %d\nSolution 2: %d\n" (solve filename)
    (solve ~smudges:true filename)
