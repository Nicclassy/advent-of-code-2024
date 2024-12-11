let identity x = x

let year = 2024
let day = 11

let first = "first.txt"
let second = "second.txt"

let next_int_gen filename =
  let ic = open_in filename in
  let scanner = Scanf.Scanning.from_channel ic in
  let gen () =
    try
      let next = Scanf.bscanf scanner "%d " identity in
      Some next
    with
    | _ -> close_in ic; None
  in
  gen

let write_int_gen filename = 
  let oc = open_out filename in
  let gen = function 
    | Some value -> output_string oc ((string_of_int value) ^ " ")
    | None -> close_out oc;
  in 
  gen

let parse_numbers row = 
  let write_int = write_int_gen first in
  let rec write_numbers = function
  | number :: numbers ->
    write_int (Some number);
    write_numbers numbers
  | [] -> ()
  in

  row
  |> String.trim
  |> String.split_on_char ' '
  |> List.map int_of_string
  |> write_numbers;
  
  let tmp_oc = open_out second in
  close_out tmp_oc;
  write_int None

let divide s len = 
  let half_len = len / 2 in
  let first = String.sub s 0 half_len in
  let second = String.sub s half_len half_len in
  [int_of_string first; int_of_string second]

let next_state n =
  let s = string_of_int n in
  let len = String.length s in
  if n = 0 then [1]
  else if len mod 2 = 0 then divide s len
  else [n * 2024]

let blink times =
  let rec write_values i repetitions read write next_int write_int = 
    match next_int () with
    | Some v -> (match next_state v with
      | a :: b :: _ -> 
        write_int (Some a);
        write_int (Some b);
        write_values i repetitions read write next_int write_int;
      | [a] -> 
        write_int (Some a);
        write_values i repetitions read write next_int write_int;
      | [] -> ();
    )
    | None -> (
      write_int None;
      loop (i + 1) repetitions;
    )
  and loop i repetitions =
    if i = repetitions then ()
    else begin
      let (read, write) = if i mod 2 = 0 then (first, second) else (second, first) in
      let read_gen = next_int_gen read in
      let write_gen = write_int_gen write in
      write_values i repetitions read write read_gen write_gen;
    end
  in
  loop 0 times

let count_values filename =
  let next_int = next_int_gen filename in
  let rec aux result = 
    match next_int () with
    | Some _ -> aux (result + 1)
    | None -> result 
  in aux 0

let cleanup () =
  if Sys.file_exists first then
    Sys.remove first;
  if Sys.file_exists second then
    Sys.remove second

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    cleanup ();
    parse_numbers input;
    blink 25;
    let result = string_of_int @@ count_values second in
    cleanup ();
    Ok result
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    cleanup ();
    parse_numbers input;
    blink 75;
    let result = string_of_int @@ count_values second in
    cleanup ();
    Ok result
end