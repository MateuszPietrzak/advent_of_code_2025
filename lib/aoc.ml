open! Core

let run solve_1 solve_2 parse_input =
  let open Result.Let_syntax in

  let%bind argv =
    if Int.equal (Array.length (Sys.get_argv ())) 3 then
      Ok (Sys.get_argv ())
    else
      Error "Please specify the input file path and the part number"
  in

  let%bind part =
    argv.(2)
    |> Int.of_string_opt
    |> Result.of_option ~error:"Could not parse part number"
  in

  let file = argv.(1) in
  let lines = In_channel.read_lines file in
  let t1 = Time_ns.now () in

  let%bind () =
    match part with
    | 1 -> Ok (solve_1 (parse_input lines))
    | 2 -> Ok (solve_2 (parse_input lines))
    | _ -> Error "Invalid part number"
  in

  let t2 = Time_ns.now () in
  let elapsed = Time_ns.diff t2 t1 in
  printf "Elapsed running time: %.6f\n" (Time_ns.Span.to_sec elapsed);

  Ok ()

let run_day solve_1 solve_2 parse_input =
  match run solve_1 solve_2 parse_input with
  | Ok () -> ()
  | Error str -> printf "Error: %s\n" str

let int_list_of_line line =
  let split_list = String.split_on_chars line ~on:[ ' '; '\n'; '\t' ] in
  let trimmed =
    List.filter split_list ~f:(fun x -> match x with "" -> false | _ -> true)
  in
  List.map trimmed ~f:Int.of_string
