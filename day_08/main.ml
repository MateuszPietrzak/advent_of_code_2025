open! Core

type point = { x : int; y : int; z : int }

let distance p p' =
  let dx = p.x - p'.x |> Float.of_int |> Float.square in
  let dy = p.y - p'.y |> Float.of_int |> Float.square in
  let dz = p.z - p'.z |> Float.of_int |> Float.square in
  dx +. dy +. dz |> Float.sqrt

let solve_1 input =
  let arr = input |> List.to_array in
  let indices = List.init (List.length input) ~f:(fun x -> x) in
  let cnt = List.length input in
  let edges =
    List.cartesian_product indices indices
    |> List.filter ~f:(fun (i, i') -> not @@ Int.equal i i')
    |> List.filter ~f:(fun (i, i') -> i < i')
    |> List.sort ~compare:(fun (i1, i2) (i1', i2') ->
        let p1 = arr.(i1) in
        let p2 = arr.(i2) in
        let p1' = arr.(i1') in
        let p2' = arr.(i2') in
        if Float.( < ) (distance p1 p2 -. distance p1' p2') 0.0 then -1 else 1)
    |> List.to_array
  in
  let parent = Array.init cnt ~f:(fun i -> i) in
  let size = Array.init cnt ~f:(fun _i -> 1) in
  let rec find x =
    if not @@ Int.equal parent.(x) x then (
      parent.(x) <- find parent.(x);
      parent.(x))
    else x
  in
  let union x y =
    let x' = find x in
    let y' = find y in
    if Int.equal x' y' then ()
    else
      let x'' = if size.(x') < size.(y') then y' else x' in
      let y'' = if size.(x') < size.(y') then x' else y' in
      parent.(y'') <- x'';
      size.(x'') <- size.(x'') + size.(y'');
      ()
  in
  for i = 0 to 999 do
    let a, b = edges.(i) in
    union a b
  done;
  let sizes =
    List.init cnt ~f:(fun i -> i)
    |> List.filter ~f:(fun i -> Int.equal i (find i))
    |> List.map ~f:(fun i -> size.(i))
    |> List.sort ~compare:Int.descending
    |> List.to_array
  in
  printf "%d\n" (sizes.(0) * sizes.(1) * sizes.(2));
  ()

let solve_2 input =
  let arr = input |> List.to_array in
  let indices = List.init (List.length input) ~f:(fun x -> x) in
  let cnt = List.length input in
  let edges =
    List.cartesian_product indices indices
    |> List.filter ~f:(fun (i, i') -> not @@ Int.equal i i')
    |> List.filter ~f:(fun (i, i') -> i < i')
    |> List.sort ~compare:(fun (i1, i2) (i1', i2') ->
        let p1 = arr.(i1) in
        let p2 = arr.(i2) in
        let p1' = arr.(i1') in
        let p2' = arr.(i2') in
        if Float.( < ) (distance p1 p2 -. distance p1' p2') 0.0 then -1 else 1)
    |> List.to_array
  in
  let parent = Array.init cnt ~f:(fun i -> i) in
  let size = Array.init cnt ~f:(fun _i -> 1) in
  let rec find x =
    if not @@ Int.equal parent.(x) x then (
      parent.(x) <- find parent.(x);
      parent.(x))
    else x
  in
  let union x y =
    let x' = find x in
    let y' = find y in
    if Int.equal x' y' then ()
    else
      let x'' = if size.(x') < size.(y') then y' else x' in
      let y'' = if size.(x') < size.(y') then x' else y' in
      parent.(y'') <- x'';
      size.(x'') <- size.(x'') + size.(y'');
      ()
  in
  let res = ref 0 in
  Array.iter edges ~f:(fun (a, b) ->
      if not @@ Int.equal (find a) (find b) then (
        union a b;
        if Int.equal !res 0 then
          if Int.equal size.(find a) 1000 then res := arr.(a).x * arr.(b).x));
  printf "%d\n" !res;
  ()

let parse_input lines =
  let open Re in
  let r =
    seq
      [
        group (rep1 digit);
        char ',';
        group (rep1 digit);
        char ',';
        group (rep1 digit);
      ]
    |> compile
  in
  lines
  |> List.map ~f:(fun line ->
      let groups = exec r line in
      let d1 = Group.get groups 1 |> Int.of_string in
      let d2 = Group.get groups 2 |> Int.of_string in
      let d3 = Group.get groups 3 |> Int.of_string in
      { x = d1; y = d2; z = d3 })

let () = Aoc.run_day solve_1 solve_2 parse_input
