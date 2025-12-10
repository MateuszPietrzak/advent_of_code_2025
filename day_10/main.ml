open! Core

type indicator = On | Off

let solve_1 input =
  let rec find_best ops state =
    match ops with
    | [] -> if phys_equal state 0 then 0 else Int.max_value / 2
    | x :: xs -> Int.min (1 + find_best xs (state lxor x)) (find_best xs state)
  in

  let res =
    List.fold input ~init:0 ~f:(fun acc (i, b, _j) ->
        let num =
          i |> Array.rev
          |> Array.fold ~init:0 ~f:(fun acc' i ->
              match i with On -> (2 * acc') + 1 | Off -> 2 * acc')
        in

        let ops =
          b
          |> List.map ~f:(fun ar ->
              List.fold ar ~init:0 ~f:(fun acc x -> acc + (1 lsl x)))
        in

        let r = find_best ops num in

        acc + r)
  in
  printf "%d\n" res

let solve_2 _input = ()

let parse_input lines =
  let open Re in
  let exp =
    seq
      [
        char '[';
        group (rep1 (alt [ char '.'; char '#' ]));
        char ']';
        char ' ';
        group (rep1 (alt [ char ' '; char '('; char ')'; char ','; digit ]));
        char ' ';
        char '{';
        group (rep1 (alt [ digit; char ',' ]));
        char '}';
      ]
    |> compile
  in
  let exp' =
    seq [ char '('; group (rep1 (alt [ char ','; digit ])); char ')' ]
    |> compile
  in
  lines
  |> List.map ~f:(fun str ->
      let groups = exec exp str in
      let indicators =
        Group.get groups 1 |> String.to_array
        |> Array.map ~f:(fun c ->
            match c with '.' -> Off | '#' -> On | _ -> assert false)
      in
      let buttons =
        Group.get groups 2 |> String.split ~on:' '
        |> List.map ~f:(fun s ->
            let groups' = exec exp' s in
            let but = Group.get groups' 1 in
            String.split but ~on:',' |> List.map ~f:Int.of_string)
      in
      let joltages = Group.get groups 3 in
      (indicators, buttons, joltages))

let () = Aoc.run_day solve_1 solve_2 parse_input
