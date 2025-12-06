open! Core

type op = Add | Mult

let solve_1 (cols, ops) =
  let res =
    List.zip_exn cols ops
    |> List.fold ~init:0 ~f:(fun acc (col, op) ->
        let nums =
          col |> List.map ~f:String.to_list
          |> List.map ~f:(fun num ->
              List.filter num ~f:(fun c -> not @@ Char.equal c ' '))
          |> List.map ~f:String.of_char_list
          |> List.map ~f:Int.of_string
        in
        let func = match op with Add -> ( + ) | Mult -> ( * ) in
        let init = match op with Add -> 0 | Mult -> 1 in
        let r = List.fold nums ~init ~f:func in

        acc + r)
  in
  printf "%d\n" res

let solve_2 (cols, ops) =
  let res =
    List.zip_exn cols ops
    |> List.fold ~init:0 ~f:(fun acc (col, op) ->
        let nums =
          col |> List.map ~f:String.to_list |> List.transpose_exn
          |> List.map ~f:(fun num ->
              List.filter num ~f:(fun c -> not @@ Char.equal c ' '))
          |> List.map ~f:String.of_char_list
          |> List.map ~f:Int.of_string
        in
        let func = match op with Add -> ( + ) | Mult -> ( * ) in
        let init = match op with Add -> 0 | Mult -> 1 in
        let r = List.fold nums ~init ~f:func in

        acc + r)
  in
  printf "%d\n" res

let parse_input lines =
  let string_list aux =
    let split_list = String.split_on_chars aux ~on:[ ' '; '\n'; '\t' ] in
    List.filter split_list ~f:(fun x -> match x with "" -> false | _ -> true)
  in
  let spacing =
    List.drop lines (List.length lines - 1)
    |> List.hd_exn
    |> String.split_on_chars ~on:[ '*'; '+' ]
    |> List.map ~f:String.length
    |> List.map ~f:(( + ) 1)
    |> List.tl_exn
    |> fun x -> List.take x (List.length x - 1)
  in
  let rec take spacing lines =
    match spacing with
    | [] -> []
    | x :: xs ->
        let cur_col =
          List.map lines ~f:(fun row ->
              row |> String.to_list |> fun t -> List.take t (x - 1))
          |> List.map ~f:String.of_char_list
        in
        let res_col =
          List.map lines ~f:(fun row ->
              row |> String.to_list |> fun t -> List.drop t x)
          |> List.map ~f:String.of_char_list
        in
        cur_col :: take xs res_col
  in
  let columns = take spacing (List.take lines (List.length lines - 1)) in
  let ops =
    List.drop lines (List.length lines - 1)
    |> List.hd_exn |> string_list
    |> List.map ~f:(function "*" -> Mult | "+" -> Add | _ -> assert false)
    |> fun f -> List.take f (List.length f - 1)
  in
  (columns, ops)

let () = Aoc.run_day solve_1 solve_2 parse_input
