let has_repetition s =
  let len = String.length s in
  if len < 2 || len mod 2 <> 0 then false
  else
    let half = len / 2 in
    let first_half = String.sub s 0 half in
    let second_half = String.sub s half half in
    String.equal first_half second_half

let expand_range start_n end_n =
  List.init (end_n - start_n + 1) (fun i -> start_n + i)

let () =
  let input = In_channel.input_all stdin in
  let invalid_id_count =
    input
    |> String.split_on_char ','
    |> List.map (fun range ->
      match String.split_on_char '-' range with
      | [s;e] -> let start_n = int_of_string (String.trim s) in
                 let end_n = int_of_string (String.trim e) in
                 expand_range start_n end_n
      |_ -> []
      )
    |> List.flatten
    |> List.filter (fun n -> has_repetition (string_of_int n))
    |> List.fold_left (+) 0
  in
  print_int invalid_id_count;
  print_newline ();
