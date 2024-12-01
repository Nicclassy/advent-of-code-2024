let unzip_pairs lst = 
  List.fold_left (fun (first, second) (x, y) -> (x :: first, y :: second)) ([], []) lst;;

let parse_list_columns input conversion =
  let lines = String.split_on_char '\n' input in
  List.filter_map (fun line -> 
    match Str.split (Str.regexp " +") line with
    | [first_value; second_value] -> Some (conversion first_value, conversion second_value)
    | _ -> None
  ) lines;;

let separate_and_sort_lists lst = 
  let (lst1, lst2) = unzip_pairs lst in
  let lst1_sorted = List.sort compare lst1 in
  let lst2_sorted = List.sort compare lst2 in
  (lst1_sorted, lst2_sorted);;