open! Core

let run solve_1 solve_2 parse_input =
  let open Result.Let_syntax in

  let%bind argv =
    let list = Sys.get_argv () in
    if list |> Array.length |> Int.equal 3 then
      Ok list
    else
      Error "Please specify the input file path and the part number"
  in

  let%bind part =
    argv.(2)
    |> Int.of_string_opt
    |> Result.of_option ~error:"Could not parse part number"
  in

  let file = argv.(1) in

  let%bind lines =
    try file |> In_channel.read_lines |> Result.return
    with _ -> Error "Could not find file"
  in

  let t1 = Time_ns.now () in

  let%bind () =
    match part with
    | 1 -> lines |> parse_input |> solve_1 |> Result.return
    | 2 -> lines |> parse_input |> solve_2 |> Result.return
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
