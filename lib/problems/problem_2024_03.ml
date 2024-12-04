let year = 2024
let day = 3

let is_digit c =
  let open Base in
  Char.is_digit c

let is_parameterless_fn s cur fn = 
  let fn_len = String.length fn in
  cur + fn_len < String.length s && fn_len |> String.sub s cur = fn

let is_op s op cur =
  let op_len = String.length op in
  let starts_with_op start = 
    start + op_len <= String.length s && String.sub s start op_len = op in

  let rec is_numeric_followed_by_char i seen_digit followed_by = 
    if i >= String.length s then None
    else
      match String.get s i with
      | c when is_digit c -> is_numeric_followed_by_char (i + 1) true followed_by
      | c when c = followed_by -> if seen_digit then Some i else None
      | _ -> None
  in

  let op_end = ref 0 in
  let found_end =
    starts_with_op cur &&
    (let cur = cur + op_len in cur < String.length s && String.get s cur = '(') &&
    (let cur = cur + op_len + 1 in
     match is_numeric_followed_by_char cur false ',' with
      | Some first_num_end ->
        let cur = first_num_end + 1 in
        (match is_numeric_followed_by_char cur false ')' with
          | Some second_num_end ->
            op_end := second_num_end;
            true
          | None -> false)
      | None -> false)
  in
  if found_end then Some (!op_end - 1) else None

let first_pair = function
  | x :: y :: _ -> Some (x, y)
  | _ -> None

let rec sum_ops s op cur sum enabled global_enabled =
  if cur >= String.length s then sum
  else
    let op_len = String.length op in
    let enabled =
      if global_enabled then true
      else if is_parameterless_fn s cur "do()" then true
      else if is_parameterless_fn s cur "don't()" then false
      else enabled
    in
    match is_op s op cur with
    | Some op_end ->
      let substr = String.sub s (cur + op_len + 1) (op_end - cur - op_len) in
      (match String.split_on_char ',' substr |> List.map String.trim |> List.map int_of_string |> first_pair with
        | Some (a, b) -> sum_ops s op (op_end + 1) (sum + if enabled then (a * b) else 0) enabled global_enabled
        | None -> sum_ops s op (cur + 1) sum enabled global_enabled)
    | None -> sum_ops s op (cur + 1) sum enabled global_enabled

module Part_1 = struct
  let run (input : string) : (string, string) result = Ok (true |> sum_ops input "mul" 0 0 true |> string_of_int)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok (false |> sum_ops input "mul" 0 0 true |> string_of_int)
end