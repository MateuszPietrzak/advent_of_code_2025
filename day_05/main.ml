open! Core

let solve_1 (ranges, ingredients) =
  let res =
    List.fold ingredients ~init:0 ~f:(fun acc x ->
        match List.find ranges ~f:(fun (s, e) -> x >= s && x <= e) with
        | Some _ -> acc + 1
        | None -> acc)
  in
  printf "%d\n" res

let solve_2 (ranges, _) =
  let sorted_ranges =
    List.sort ranges ~compare:(fun (s, _) (s', _) -> s - s')
  in
  let rec aux = function
    | [] -> []
    | [ x ] -> [ x ]
    | (s, e) :: (s', e') :: xs ->
        if e >= e' then aux ((s, e) :: xs)
        else if e >= s' then aux ((s, e') :: xs)
        else (s, e) :: aux ((s', e') :: xs)
  in
  let combined = aux sorted_ranges in
  let res =
    List.fold combined ~init:0 ~f:(fun acc (s, e) -> acc + (e - s + 1))
  in
  printf "%d\n" res

let parse_input lines =
  let ranges, ingredients =
    List.split_while lines ~f:(fun str -> not @@ String.equal str "")
  in
  let open Re in
  let regex =
    seq [ group (rep1 digit); char '-'; group (rep1 digit) ] |> compile
  in
  let ranges' =
    List.filter ranges ~f:(fun str -> not @@ String.equal str "")
    |> List.map ~f:(fun str ->
        let groups = exec regex str in
        let d = Group.get groups 1 |> Int.of_string in
        let d' = Group.get groups 2 |> Int.of_string in
        (d, d'))
  in
  let ingredients' =
    List.filter ingredients ~f:(fun str -> not @@ String.equal str "")
    |> List.map ~f:Int.of_string
  in
  (ranges', ingredients')

let () = Aoc.run_day solve_1 solve_2 parse_input
