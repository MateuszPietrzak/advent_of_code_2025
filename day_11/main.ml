open! Core

let key_to_ind key =
  let ar =
    String.to_list key |> List.map ~f:Char.to_int
    |> List.map ~f:(fun x -> x - 97)
    |> List.to_array
  in
  ar.(0) + (26 * ar.(1)) + (26 * 26 * ar.(2))

let solve_1 input =
  let len = 26 * 26 * 26 in
  let in_list = Array.init len ~f:(const 0) in
  let topo = Array.init len ~f:(const 0) in
  let next_val = ref 1 in
  let topo_q = Queue.create () in
  Array.iter input ~f:(fun lst ->
      List.iter lst ~f:(fun ind -> in_list.(ind) <- in_list.(ind) + 1));
  Array.iteri input ~f:(fun i lst ->
      match lst with
      | [] -> ()
      | _ -> if Int.equal in_list.(i) 0 then Queue.enqueue topo_q i);
  while not @@ Queue.is_empty topo_q do
    let ind = Queue.dequeue_exn topo_q in
    topo.(ind) <- !next_val;
    Int.incr next_val;
    List.iter input.(ind) ~f:(fun x ->
        in_list.(x) <- in_list.(x) - 1;
        if Int.equal in_list.(x) 0 then Queue.enqueue topo_q x);
    ()
  done;
  let comb =
    Array.init len ~f:(fun i -> if Int.equal i (key_to_ind "you") then 1 else 0)
  in
  let ordering =
    Array.mapi input ~f:(fun i _ -> (i, topo.(i)))
    |> Array.filter ~f:(fun (i, _) -> List.length input.(i) > 0)
    |> Array.to_list
    |> List.sort ~compare:(fun (_i, o) (_i', o') -> o - o')
  in
  List.iter ordering ~f:(fun (i, _o) ->
      let ths = comb.(i) in
      List.iter input.(i) ~f:(fun x -> comb.(x) <- comb.(x) + ths);
      ());
  printf "%d\n" comb.(key_to_ind "out")

let solve_2 input =
  let len = 26 * 26 * 26 in
  let in_list = Array.init len ~f:(const 0) in
  let topo = Array.init len ~f:(const 0) in
  let next_val = ref 1 in
  let topo_q = Queue.create () in
  Array.iter input ~f:(fun lst ->
      List.iter lst ~f:(fun ind -> in_list.(ind) <- in_list.(ind) + 1));
  Array.iteri input ~f:(fun i lst ->
      match lst with
      | [] -> ()
      | _ -> if Int.equal in_list.(i) 0 then Queue.enqueue topo_q i);
  while not @@ Queue.is_empty topo_q do
    let ind = Queue.dequeue_exn topo_q in
    topo.(ind) <- !next_val;
    Int.incr next_val;
    List.iter input.(ind) ~f:(fun x ->
        in_list.(x) <- in_list.(x) - 1;
        if Int.equal in_list.(x) 0 then Queue.enqueue topo_q x);
    ()
  done;
  let get_res from_key to_key =
    let comb =
      Array.init len ~f:(fun i ->
          if Int.equal i (key_to_ind from_key) then 1 else 0)
    in
    let ordering =
      Array.mapi input ~f:(fun i _ -> (i, topo.(i)))
      |> Array.filter ~f:(fun (i, _) -> List.length input.(i) > 0)
      |> Array.to_list
      |> List.sort ~compare:(fun (_i, o) (_i', o') -> o - o')
    in
    List.iter ordering ~f:(fun (i, _o) ->
        let ths = comb.(i) in
        List.iter input.(i) ~f:(fun x -> comb.(x) <- comb.(x) + ths);
        ());
    comb.(key_to_ind to_key)
  in
  let r1 = get_res "svr" "fft" in
  let r2 = get_res "fft" "dac" in
  let r3 = get_res "dac" "out" in
  printf "%d\n" (r1 * r2 * r3);
  let r1' = get_res "svr" "dac" in
  let r2' = get_res "dac" "fft" in
  let r3' = get_res "fft" "out" in
  printf "%d\n" (r1' * r2' * r3');
  ()

let parse_input lines =
  let len = 26 * 26 * 26 in
  let res = Array.init len ~f:(fun _ -> []) in
  List.iter lines ~f:(fun str ->
      let arr =
        String.split_on_chars str ~on:[ ' '; ':' ]
        |> List.filter ~f:(fun x -> match x with "" -> false | _ -> true)
      in
      match arr with
      | [] -> assert false
      | x :: xs ->
          let ind = key_to_ind x in
          let targets = List.map xs ~f:key_to_ind in
          res.(ind) <- targets);
  res

let () = Aoc.run_day solve_1 solve_2 parse_input
