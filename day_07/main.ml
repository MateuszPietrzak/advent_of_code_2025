open! Core

type cell = Start | Empty | Split
type mask_elem = Laser | NoLaser

let solve_1 input =
  let start_id, _ =
    input |> List.hd_exn
    |> Array.findi_exn ~f:(fun _i -> function Start -> true | _ -> false)
  in
  let width = input |> List.hd_exn |> Array.length in
  let mask = Array.init width ~f:(const NoLaser) in
  mask.(start_id) <- Laser;
  let res =
    input |> List.tl_exn
    |> List.fold ~init:0 ~f:(fun acc row ->
        let r =
          Array.foldi row ~init:0 ~f:(fun i acc elem ->
              match elem with
              | Start | Empty -> acc
              | Split -> (
                  match mask.(i) with
                  | NoLaser -> acc
                  | Laser ->
                      mask.(i) <- NoLaser;
                      mask.(i - 1) <- Laser;
                      mask.(i + 1) <- Laser;
                      acc + 1))
        in
        acc + r)
  in
  printf "%d\n" res

let solve_2 input =
  let start_id, _ =
    input |> List.hd_exn
    |> Array.findi_exn ~f:(fun _i -> function Start -> true | _ -> false)
  in
  let width = input |> List.hd_exn |> Array.length in
  let height = input |> List.length in
  let mask = Array.init height ~f:(fun _ -> Array.init width ~f:(const 0)) in
  mask.(0).(start_id) <- 1;
  let layer = ref 1 in
  input |> List.tl_exn
  |> List.iter ~f:(fun row ->
      Array.iteri row ~f:(fun i elem ->
          match elem with
          | Start | Empty ->
              mask.(!layer).(i) <- mask.(!layer).(i) + mask.(!layer - 1).(i)
          | Split ->
              mask.(!layer).(i - 1) <-
                mask.(!layer).(i - 1) + mask.(!layer - 1).(i);
              mask.(!layer).(i + 1) <-
                mask.(!layer).(i + 1) + mask.(!layer - 1).(i));
      Int.incr layer);
  printf "%d\n" (Array.fold mask.(height - 1) ~init:0 ~f:( + ))

let parse_input lines =
  lines
  |> List.map ~f:String.to_array
  |> List.map ~f:(fun row ->
      row
      |> Array.map ~f:(function
        | '.' -> Empty
        | '^' -> Split
        | 'S' -> Start
        | _ -> assert false))

let () = Aoc.run_day solve_1 solve_2 parse_input
