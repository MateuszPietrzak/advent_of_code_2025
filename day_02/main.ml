open! Core

let solve_1 input =
  let rec check s e =
    if s > e then 0
    else
      let str = Int.to_string s in
      if Int.equal (String.length str % 2) 1 then check (s + 1) e
      else
        let hl = String.length str / 2 in
        let sub1 = String.sub ~pos:0 ~len:hl str in
        let sub2 = String.sub ~pos:hl ~len:hl str in
        if String.equal sub1 sub2 then s + check (s + 1) e else check (s + 1) e
  in
  let res =
    input
    |> List.fold ~init:0 ~f:(fun acc (n1, n2) ->
        let add = check n1 n2 in
        acc + add)
  in
  printf "%d\n" res

let solve_2 input =
  let rec ver_pattern pattern str =
    if Int.equal (String.length str) 0 then true
    else if String.length str < String.length pattern then false
    else
      let sl = String.length pattern in
      let sub = String.sub ~pos:0 ~len:sl str in
      let rest = String.sub ~pos:sl ~len:(String.length str - sl) str in
      if String.equal pattern sub then ver_pattern pattern rest else false
  in
  let rec is_pal str split =
    if split > String.length str then false
    else if not @@ Int.equal (String.length str % split) 0 then
      is_pal str (split + 1)
    else
      let sl = String.length str / split in
      let pattern = String.sub ~pos:0 ~len:sl str in
      if ver_pattern pattern str then true else is_pal str (split + 1)
  in
  let rec check s e =
    if s > e then 0
    else
      let str = Int.to_string s in
      if is_pal str 2 then s + check (s + 1) e else check (s + 1) e
  in
  let res =
    input
    |> List.fold ~init:0 ~f:(fun acc (n1, n2) ->
        let add = check n1 n2 in
        acc + add)
  in
  printf "%d\n" res

let parse_input lines =
  let open Re in
  let regex =
    seq [ group (rep1 digit); char '-'; group (rep1 digit) ] |> compile
  in
  lines |> List.hd_exn |> String.split ~on:','
  |> List.map ~f:(fun s ->
      let group = exec regex s in
      let num1 = Group.get group 1 |> Int.of_string in
      let num2 = Group.get group 2 |> Int.of_string in
      (num1, num2))

let () = Aoc.run_day solve_1 solve_2 parse_input
