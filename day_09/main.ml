open! Core

let solve_1 input =
  let cart = List.cartesian_product input input in
  let res =
    List.fold cart ~init:0 ~f:(fun acc ((d1, d1'), (d2, d2')) ->
        let area = (Int.abs (d1 - d2) + 1) * (Int.abs (d1' - d2') + 1) in
        Int.max acc area)
  in
  printf "%d\n" res

type point = { x : int; y : int }
type edge_kind = Vertical | Horizontal
type edge = { kind : edge_kind; p : point; p' : point }

let solve_2 input =
  let arr = input |> List.to_array |> Array.map ~f:(fun (x, y) -> { x; y }) in
  let len = List.length input in
  let edges =
    Array.mapi arr ~f:(fun i p ->
        let p' = arr.(Int.( % ) (i + 1) len) in
        match p.x - p'.x with
        | 0 -> { kind = Vertical; p; p' }
        | _ -> { kind = Horizontal; p; p' })
  in
  let cart = List.cartesian_product input input in
  let point_in_box b b' p =
    let left = Int.min b.x b'.x in
    let right = Int.max b.x b'.x in
    let top = Int.max b.y b'.y in
    let bottom = Int.min b.y b'.y in
    p.x > left && p.x < right && p.y < top && p.y > bottom
  in
  let vertical_line_through b b' e =
    let left = Int.min b.x b'.x in
    let right = Int.max b.x b'.x in
    let top = Int.max b.y b'.y in
    let bottom = Int.min b.y b'.y in
    let e_top = Int.max e.p.y e.p'.y in
    let e_bot = Int.min e.p.y e.p'.y in
    e.p.x > left && e.p.x < right && e_top >= top && e_bot <= bottom
  in
  let horiz_line_through b b' e =
    let left = Int.min b.x b'.x in
    let right = Int.max b.x b'.x in
    let top = Int.max b.y b'.y in
    let bottom = Int.min b.y b'.y in
    let e_right = Int.max e.p.x e.p'.x in
    let e_left = Int.min e.p.x e.p'.x in
    e.p.y < top && e.p.y > bottom && e_right >= right && e_left <= left
  in
  let res =
    List.fold cart ~init:0 ~f:(fun acc ((d1, d1'), (d2, d2')) ->
        let p = { x = d1; y = d1' } in
        let p' = { x = d2; y = d2' } in
        let area = (Int.abs (d1 - d2) + 1) * (Int.abs (d1' - d2') + 1) in
        if area <= acc then acc
        else
          let collides =
            Array.fold edges ~init:false ~f:(fun acc edge ->
                let in_box = point_in_box p p' edge.p in
                let in_box' = point_in_box p p' edge.p' in
                let edge_through =
                  match edge.kind with
                  | Horizontal -> horiz_line_through p p' edge
                  | Vertical -> vertical_line_through p p' edge
                in
                if in_box || in_box' || edge_through then true else acc)
          in
          if collides then acc else area)
  in
  printf "%d\n" res

let parse_input lines =
  let open Re in
  let exp =
    seq [ group (rep1 digit); char ','; group (rep1 digit) ] |> compile
  in
  lines
  |> List.map ~f:(fun str ->
      let groups = exec exp str in
      let d = Group.get groups 1 |> Int.of_string in
      let d' = Group.get groups 2 |> Int.of_string in
      (d, d'))

let () = Aoc.run_day solve_1 solve_2 parse_input
