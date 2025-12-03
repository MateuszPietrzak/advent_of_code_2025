open! Core

let solve_1 input =
  let res =
    List.map input ~f:(fun str ->
        let list = String.to_list str in
        let max_char =
          List.take list (List.length list - 1)
          |> List.fold ~init:'0' ~f:Char.max
        in
        let rec find_suf = function
          | [] -> assert false
          | x :: xs -> if Char.equal x max_char then xs else find_suf xs
        in
        let snd_char = list |> find_suf |> List.fold ~init:'0' ~f:Char.max in
        let res = [ max_char; snd_char ] |> String.of_list |> Int.of_string in
        res)
    |> List.fold ~init:0 ~f:( + )
  in
  printf "%d\n" res

let solve_2 input =
  let rec find_next_char chars_left list =
    if Int.equal chars_left 0 then []
    else
      let max_char =
        List.take list (List.length list - chars_left + 1)
        |> List.fold ~init:'0' ~f:Char.max
      in
      let rec find_suf = function
        | [] -> assert false
        | x :: xs -> if Char.equal x max_char then xs else find_suf xs
      in
      max_char :: find_next_char (chars_left - 1) (find_suf list)
  in
  let res =
    List.map input ~f:(fun str ->
        let r = find_next_char 12 (String.to_list str) in
        r)
    |> List.map ~f:(fun list -> list |> String.of_list |> Int.of_string)
    |> List.fold ~init:0 ~f:( + )
  in
  printf "%d\n" res

let parse_input lines = lines
let () = Aoc.run_day solve_1 solve_2 parse_input
