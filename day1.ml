let smod n size =
  let r = n mod size in
  if r < 0 then r + size else r

let solve pos str =
  let dir = String.get str 0 in
  let mov = String.sub str 1 (String.length str - 1) in
  let mov = int_of_string(mov) in
  match dir with
  | 'L' -> smod (pos - mov) 100
  | 'R' -> smod (pos + mov) 100
  | _ -> pos

let lines_from_stdin () =
  let try_read () =
    try Some (input_line stdin) with End_of_file -> None in
  let rec iter acc = match try_read () with
    | Some s -> iter(s :: acc)
    | None -> close_in stdin; List.rev acc in
      iter []

let () =
  let start = 50 in
  let (final, zeroes) =
  lines_from_stdin ()
    |> List.fold_left (fun (pos, count) line ->
        let new_pos = solve pos line in
        let new_count = if new_pos = 0 then count + 1 else count in
        (new_pos, new_count))
      (start, 0) in
    print_int zeroes;
    print_newline ();
