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

let solve_2 input =
  let res =
    List.fold input ~init:0 ~f:(fun acc (_i, b, j) ->
        let open Z3 in
        let ctx = mk_context [] in
        let opt = Optimize.mk_opt ctx in

        let zero = Arithmetic.Integer.mk_numeral_i ctx 0 in
        let int_sort = Z3.Arithmetic.Integer.mk_sort ctx in

        let buttons =
          List.mapi b ~f:(fun i _ ->
              let sym = Symbol.mk_string ctx ("b" ^ Int.to_string i) in
              let r = Expr.mk_const ctx sym int_sort in
              let c = Arithmetic.mk_ge ctx r zero in
              Optimize.add opt [ c ];
              r)
        in

        List.mapi j ~f:(fun i joltage ->
            let incrs =
              List.mapi b ~f:(fun j button -> (j, button))
              |> List.filter ~f:(fun (_, button) ->
                  match List.find button ~f:(Int.equal i) with
                  | Some _ -> true
                  | None -> false)
              |> List.map ~f:fst
              |> List.map ~f:(fun j -> List.nth_exn buttons j)
            in
            let target = Arithmetic.Integer.mk_numeral_i ctx joltage in
            let sum = Arithmetic.mk_add ctx incrs in
            Boolean.mk_eq ctx sum target)
        |> Optimize.add opt;

        let presses =
          Expr.mk_const ctx (Symbol.mk_string ctx "presses") int_sort
        in
        let sum = Arithmetic.mk_add ctx buttons in
        let c = Boolean.mk_eq ctx presses sum in

        Optimize.add opt [ c ];

        let _ = Optimize.minimize opt presses in

        assert (match Optimize.check opt with SATISFIABLE -> true | _ -> false);

        let open Option.Monad_infix in
        let r_opt =
          Optimize.get_model opt >>= fun model ->
          Model.evaluate model presses false >>= fun expr ->
          Expr.to_string expr |> Int.of_string_opt
        in

        match r_opt with None -> assert false | Some x -> acc + x)
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
