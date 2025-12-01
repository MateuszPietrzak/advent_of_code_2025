open! Core

type direction = L | R

let solve_1 input =
  let res = ref 0 in
  let _ =
    List.fold input ~init:50 ~f:(fun acc (dir, num) ->
        let new_acc =
          match dir with
          | L -> (acc - num + 10000000) % 100
          | R -> (acc + num) % 100
        in
        if Int.equal new_acc 0 then Int.incr res else ();
        new_acc)
  in
  printf "%d\n" !res

let solve_2 input =
  let res = ref 0 in
  let add_to_acc acc num =
    let new_acc = acc + num in
    let rec get_actual x =
      if x >= 100 then (
        Int.incr res;
        get_actual (x - 100))
      else x
    in
    get_actual new_acc
  in
  let sub_from_acc acc num =
    let new_acc = acc - num in
    if Int.equal acc 0 then Int.decr res;
    let rec get_actual x =
      if Int.equal x 0 then (
        Int.incr res;
        x)
      else if x < 0 then (
        Int.incr res;
        get_actual (x + 100))
      else x
    in

    get_actual new_acc
  in
  let _ =
    List.fold input ~init:50 ~f:(fun acc (dir, num) ->
        let new_acc =
          match dir with L -> sub_from_acc acc num | R -> add_to_acc acc num
        in
        new_acc)
  in
  printf "%d\n" !res

let parse_input lines =
  let open Re in
  let regex =
    seq [ group (alt [ char 'L'; char 'R' ]); group (rep1 digit) ] |> compile
  in
  List.map lines ~f:(fun s ->
      let group = exec regex s in

      let dir =
        match Group.get group 1 with "L" -> L | "R" -> R | _ -> assert false
      in
      let num = Group.get group 2 |> Int.of_string in
      (dir, num))

let () = Aoc.run_day solve_1 solve_2 parse_input
