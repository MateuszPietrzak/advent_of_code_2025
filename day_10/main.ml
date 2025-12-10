open! Core
open! Lacaml.S

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

let solve_2 input =
  let rec gen_splits sum length =
    if length > 5 then(    printf "%d\n" length;
    Out_channel.flush stdout);

    if Int.equal length 0 then []
    else if Int.equal length 1 then [ [ sum ] ]
    else
      List.init (sum + 1) ~f:(fun i ->
          gen_splits (sum - i) (length - 1) |> List.map ~f:(fun l -> i :: l))
      |> List.concat
  in
  let apply_split state split ops =
    state
    |> List.mapi ~f:(fun i v ->
        let coeffs = ops |> List.map ~f:(fun op -> op.(i)) in
        let sum =
          List.zip_exn coeffs split
          |> List.map ~f:(fun (a, b) -> a * b)
          |> List.fold ~init:0 ~f:( + )
        in
        v - sum)
  in
  let rec find_best acc ops state index =
    if Int.equal index (List.length state) then acc
    else
      let this_ops = List.filter ops ~f:(fun op -> Int.equal op.(index) 1) in
      let next_ops = List.filter ops ~f:(fun op -> Int.equal op.(index) 0) in
      let to_reach = List.nth_exn state index in

      (* printf "at index %d\n" index; *)
      (* printf "with state "; *)
      (* List.iter state ~f:(printf "%d "); *)
      (* printf "\n"; *)
      (* printf "%d, %d -> %d\n" (List.length this_ops) (List.length next_ops) *)
      (*   to_reach; *)
      (* printf "acc = %d\n" acc; *)
      Out_channel.flush stdout;

      if Int.equal (List.length this_ops) 0 then
        if Int.equal to_reach 0 then find_best acc next_ops state (index + 1)
        else Int.max_value / 2
      else
        let splits = gen_splits to_reach (List.length this_ops) in
        printf "gen_splits\n";
        Out_channel.flush stdout;

        let res =
          splits
          |> List.map ~f:(fun split ->
              let new_state = apply_split state split this_ops in
              let min_val =
                List.fold new_state ~init:Int.max_value ~f:Int.min
              in
              if min_val < 0 then Int.max_value / 2
              else (
                if Int.equal index 0 then (
                  printf "about to go down with state\n";
                  List.iter new_state ~f:(printf "%d ");
                  printf "\nusing split\n";
                  List.iter split ~f:(printf "%d ");
                  printf "\n");
                Out_channel.flush stdout;
                let r' =
                  find_best (acc + to_reach) next_ops new_state (index + 1)
                in
                if Int.equal index 0 then printf "r' = %d\n\n" r';
                r'))
          |> List.fold ~init:Int.max_value ~f:Int.min
        in
        res
  in
  let res =
    List.fold input ~init:0 ~f:(fun acc (_i, b, j) ->
        List.iter j ~f:(fun jl -> printf "%d " jl);
        printf "\n";

        let ops =
          List.map b ~f:(fun button ->
              let res = Array.init (List.length j) ~f:(const 0) in
              List.iter button ~f:(fun it -> res.(it) <- 1);
              res)
        in

        List.iter ops ~f:(fun op ->
            printf "op: ";
            Array.iter op ~f:(fun i -> printf "%d " i);
            printf "\n");

        let r = find_best 0 ops j 0 in
        printf "%d\n\n" r;
        Out_channel.flush stdout;

        acc + r)
  in
  printf "%d\n" res

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
      let joltages =
        Group.get groups 3 |> String.split ~on:',' |> List.map ~f:Int.of_string
      in
      (indicators, buttons, joltages))

let () = Aoc.run_day solve_1 solve_2 parse_input
