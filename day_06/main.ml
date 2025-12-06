open! Core

type op = Add | Mult

let solve_1 (nums, ops) =
  List.zip_exn nums ops
  |> List.iter ~f:(fun (problem, op) ->
      (match op with Add -> printf "Add: " | Mult -> printf "Mul: ");
      List.iter problem ~f:(fun elem -> printf "%d " elem);
      printf "\n");
  let res =
    List.zip_exn nums ops
    |> List.fold ~init:0 ~f:(fun acc (problem, op) ->
        let func = match op with Add -> ( + ) | Mult -> ( * ) in
        let init = match op with Add -> 0 | Mult -> 1 in
        let r = List.fold problem ~init ~f:func in
        printf " -> %d\n" r;
        acc + r)
  in
  printf "%d\n" res

let solve_2 (nums, ops) =
  let res =
    List.zip_exn nums ops
    |> List.fold ~init:0 ~f:(fun acc (problem, op) ->
        (match op with Add -> printf "Add: " | Mult -> printf "Mul: ");
        let _func = match op with Add -> ( + ) | Mult -> ( * ) in
        let _init = match op with Add -> 0 | Mult -> 1 in
        let new_prob =
          problem |> List.map ~f:Int.to_string
          |> List.map ~f:(fun str ->
              let len = String.length str in
              let pref =
                List.init (10 - len) ~f:(const '0') |> String.of_char_list
              in
              pref ^ str)
          |> List.map ~f:String.to_list (*|> List.transpose_exn*)
          |> List.map ~f:String.of_char_list
          (*|> List.map ~f:Int.of_string*)
        in
        List.iter new_prob ~f:(fun elem -> printf "%s " elem);
        printf "\n";
        (* let r = List.fold new_prob ~init ~f:func in *)
        (* printf " -> %d\n" r; *)
        acc)
  in
  printf "%d\n" res

let parse_input lines =
  let string_list aux =
    let split_list = String.split_on_chars aux ~on:[ ' '; '\n'; '\t' ] in
    List.filter split_list ~f:(fun x -> match x with "" -> false | _ -> true)
  in
  let nums =
    List.take lines (List.length lines - 1)
    |> List.map ~f:Aoc.int_list_of_line
    |> List.transpose_exn
  in
  let ops =
    List.drop lines (List.length lines - 1)
    |> List.hd_exn |> string_list
    |> List.map ~f:(function "*" -> Mult | "+" -> Add | _ -> assert false)
  in
  (nums, ops)

let () = Aoc.run_day solve_1 solve_2 parse_input
