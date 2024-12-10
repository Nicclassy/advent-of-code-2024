let fst (x, _) = x

let string_of_char c = String.make 1 c

let int_of_char c = Char.code c - Char.code '0'

let print_list fmt = function
  | [] -> print_string "[]"
  | [hd] -> 
    print_string "[";
    Printf.printf fmt hd;
    print_string "]\n"
  | hd :: tl ->
    print_string "[";
    Printf.printf fmt hd;
    List.iter (fun x -> print_string ", "; Printf.printf fmt x) tl;
    print_string "]\n"

let print_hashmap print_key print_value tbl =
  Hashtbl.iter (fun key value ->
    Printf.printf "%s -> %s\n" (print_key key) (print_value value)
  ) tbl

let print_option fmt = function | Some v -> Printf.printf fmt v; print_endline "" | None -> print_endline "None"

let unzip_pairs lst = 
  List.fold_left (fun (first, second) (x, y) -> (x :: first, y :: second)) ([], []) lst

let parse_list_columns input conversion =
  let lines = String.split_on_char '\n' input in
  List.filter_map (fun line -> 
    match Str.split (Str.regexp " +") line with
    | [first_value; second_value] -> Some (conversion first_value, conversion second_value)
    | _ -> None
  ) lines

let separate_and_sort_lists lst = 
  let (lst1, lst2) = unzip_pairs lst in
  let lst1_sorted = List.sort compare lst1 in
  let lst2_sorted = List.sort compare lst2 in
  (lst1_sorted, lst2_sorted)

let parse_row conversion row =
  let split_row = Str.split (Str.regexp " +") row in
  List.map conversion split_row
  
let parse_rows input conversion = 
  let rows = String.split_on_char '\n' input in
  List.map (parse_row conversion) rows

let index_of predicate arr =
  let rec aux i =
    if i >= Array.length arr then None
    else if predicate arr.(i) then Some i
    else aux (i + 1)
  in
  aux 0