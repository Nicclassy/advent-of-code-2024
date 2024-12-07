let year = 2024
let day = 7

let identity x = x

let ( ||| ) a b = int_of_string (string_of_int a ^ string_of_int b)

let first_two_elements = function
  | first :: second :: _ -> (first, second)
  | _ -> failwith "Cannot get first two elements"

let parse_input_row (row : string) : (int * int list) = 
  let (first, rest) = first_two_elements @@ String.split_on_char ':' row in
  let nums = String.split_on_char ' ' @@ String.trim rest in
  (int_of_string first, List.map int_of_string nums)

let parse_input input = 
  input
  |> String.split_on_char '\n'
  |> List.filter (fun x -> x <> "")
  |> List.map parse_input_row
  
let permutations lst r =
  let rec permutations_aux result =
    if List.length result = r then
      [List.rev result]
    else
      match lst with
      | [] -> []
      | _ -> 
        List.fold_left (fun acc x -> 
          acc @ permutations_aux (x :: result)
        ) [] lst
  in
  permutations_aux []

let compute operands operators = 
  match operands with
  | a :: b :: remaining_operands -> 
    let initial = List.hd operators a b in
    let computation = List.combine (List.tl operators) remaining_operands in
    List.fold_left (fun result (operator, operand) -> operator result operand) initial computation
  | _ -> failwith "Not enough values"

let computation_is_solvable operators row =
  let (test_value, numbers) = row in
  let rec aux = function
    | operator_permutation :: rest -> 
      if compute numbers operator_permutation = test_value then Some (test_value)
      else aux rest
    | [] -> None
  in
  aux (permutations operators ((List.length numbers) - 1))

let total_calibration_result operators rows = 
  rows 
  |> List.map @@ computation_is_solvable operators
  |> List.filter_map identity
  |> List.fold_left (+) 0

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (parse_input input |> total_calibration_result [( + ); ( * )] |> string_of_int)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (parse_input input |> total_calibration_result [( + ); ( * ); ( ||| )] |> string_of_int)
end