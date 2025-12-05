open! Core

type field = Filled | Empty

let solve_1 input =
  let rows = Array.length input in
  let cols = Array.length input.(0) in
  let res = ref 0 in
  let rec gt = function
    | [] -> 0
    | (x::xs) -> (match x with
        | Filled -> 1 + (gt xs)
        | Empty -> gt xs)
  in
  for i = 1 to (rows - 2) do
    for j = 1 to (cols - 2) do
      let around = gt [
          input.(i-1).(j-1);
          input.(i-1).(j);
          input.(i-1).(j+1);
          input.(i).(j-1);
          input.(i).(j+1);
          input.(i+1).(j-1);
          input.(i+1).(j);
          input.(i+1).(j+1)
        ] in
      match input.(i).(j) with
      | Empty -> ()
      | Filled -> if around < 4 then Int.incr res;
    done;
  done;
  printf "%d\n" !res

let solve_2 input =
  let rows = Array.length input in
  let cols = Array.length input.(0) in
  let res = ref 0 in
  let rec gt = function
    | [] -> 0
    | (x::xs) -> (match x with
        | Filled -> 1 + (gt xs)
        | Empty -> gt xs)
  in
  for _z = 1 to rows do
    for i = 1 to (rows - 2) do
        for j = 1 to (cols - 2) do
        let around = gt [
            input.(i-1).(j-1);
            input.(i-1).(j);
            input.(i-1).(j+1);
            input.(i).(j-1);
            input.(i).(j+1);
            input.(i+1).(j-1);
            input.(i+1).(j);
            input.(i+1).(j+1)
            ] in
        match input.(i).(j) with
        | Empty -> ()
        | Filled -> if around < 4 then (Int.incr res; input.(i).(j) <- Empty)
        done;
    done;
  done;
  printf "%d\n" !res

let parse_input lines =
  let lines_count = List.length lines in
  let arr =
    lines
    |> List.map ~f:String.to_array
    |> List.map ~f:(fun arr ->
        Array.map arr ~f:(function
          | '.' -> Empty
          | '@' -> Filled
          | _ -> assert false))
    |> List.map ~f:(fun row -> Array.concat [[|Empty|]; row; [|Empty|]])
  in
  [
    [Array.init (lines_count + 2) ~f:(const Empty)];
    arr;
    [Array.init (lines_count + 2) ~f:(const Empty)]
  ] |> List.concat |> List.to_array


let () = Aoc.run_day solve_1 solve_2 parse_input
