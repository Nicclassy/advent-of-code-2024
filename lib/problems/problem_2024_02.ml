open Utils

let year = 2024
let day = 2

let is_descending lst = 
 let rec aux prev rest = 
  match rest with 
  | [] -> true
  | x :: xs -> x <= prev && aux x xs in

  match lst with 
  | [] | [_] -> true
  | x :: xs -> aux x xs

let is_ascending lst = 
  let rec aux prev rest = 
    match rest with 
    | [] -> true
    | x :: xs -> x >= prev && aux x xs in
  
    match lst with 
    | [] | [_] -> true
    | x :: xs -> aux x xs

let adjacency_is_safe lst =
  if List.length lst = 1 then
    true
  else if List.length lst = 0 then
    false
  else
    begin
      let sentinel = -1 in
      let combined = List.combine lst (List.tl lst @ [sentinel]) in
      List.for_all (fun (x, y) -> 
        if y = sentinel then 
          true 
        else
          begin
            let difference = abs (x - y) in
            difference >= 1 && difference <= 3
          end
      ) combined
    end

let list_excluding_index lst index =
  let rec aux i acc = function
    | [] -> List.rev acc
    | _ :: xs when i = index -> aux (i + 1) acc xs
    | x :: xs -> aux (i + 1) (x :: acc) xs
  in
  aux 0 [] lst

let is_safe lst = 
  (is_ascending lst || is_descending lst) && adjacency_is_safe lst

let is_moderately_safe lst =
  let length = List.length lst in
  let rec any_safe orig exclude =
    let perm_without_excl = list_excluding_index orig exclude in
    if is_safe perm_without_excl then 
      true
    else if exclude >= (length - 1) then
      false
    else
      begin
        any_safe orig (exclude + 1)
      end
    
    in (is_safe lst) || (any_safe lst 0)

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    let rows = parse_rows input int_of_string in 
    rows 
    |> List.fold_left (fun acc row -> if is_safe row then acc + 1 else acc) 0
    |> string_of_int
    |> fun count -> Ok count
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    let rows = parse_rows input int_of_string in 
    rows 
    |> List.fold_left (fun acc row -> if is_moderately_safe row then acc + 1 else acc) 0
    |> string_of_int
    |> fun count -> Ok count
end