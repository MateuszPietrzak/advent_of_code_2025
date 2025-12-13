open! Core

type input = {w: int; h: int; amounts: int list}

let solve_1 input =
  let sizes = [5; 7; 7; 7; 7; 6] in
  let r = input |> List.fold ~init:0 ~f:(fun acc inp ->
      let area = inp.w * inp.h in
      let fill = List.zip_exn inp.amounts sizes |> List.fold ~init:0 ~f:(fun acc' (a, b) -> acc' + (a * b)) in
      if fill > area then acc + 1 else acc
      ) in
  let r' = input |> List.fold ~init:0 ~f:(fun acc inp ->
      let w' = inp.w / 3 in
      let h' = inp.h / 3 in
      let sum = List.fold inp.amounts ~init:0 ~f:( + ) in
      if w' * h' >= sum then acc + 1 else acc
    ) in
  printf "%d %d\n" r r'
let solve_2 _input = ()

let parse_input lines =
  let open Re in
  let exp =
    seq
      [
        group (rep1 digit);
        char 'x';
        group (rep1 digit);
        char ':';
        char ' ';
        group (rep1 (alt [ digit; char ' ' ]));
      ]
    |> compile
  in
  lines |> List.map ~f:(fun str ->
      let groups = exec exp str in
      let w = Group.get groups 1 |> Int.of_string in
      let h = Group.get groups 2 |> Int.of_string in
      let rest = Group.get groups 3 |> String.split ~on:' ' |> List.map ~f:Int.of_string in
      {w; h; amounts = rest}
    )

let () = Aoc.run_day solve_1 solve_2 parse_input
