open Utils

let year = 2024
let day = 1

module Part_1 = struct
  let total_distance lst1 lst2 = 
    let combined = List.combine lst1 lst2 in
    let distances = List.map (fun (x, y) -> (abs (x - y))) combined in
    List.fold_left (+) 0 distances
 
  let run (input : string) : (string, string) result = 
    let parsed_lists = parse_list_columns input int_of_string in
    let (lst1, lst2) = separate_and_sort_lists parsed_lists in
    Ok (string_of_int (total_distance lst1 lst2))
end

module Part_2 = struct
  let rec propagate_occurence_counts lst counts_map previous acc =
    match lst with 
    | [] -> ()
    | current :: rest -> 
      if current <> previous then
        begin
          Printf.printf "Added %d with count %d\n" previous acc;
          Hashtbl.add counts_map previous acc;
          propagate_occurence_counts rest counts_map current 1
        end
      else
        propagate_occurence_counts rest counts_map current (acc + 1)

    let rec sum_occurences lst counts_map acc =
      match lst with
      | [] -> acc
      | first :: rest -> 
        let count = if Hashtbl.mem counts_map first then first * Hashtbl.find counts_map first else 0 in
        let _ = Printf.printf "count for %d is %d\n" first count in
        sum_occurences rest counts_map (acc + count)

  let run (input : string) : (string, string) result = 
    let parsed_lists = parse_list_columns input int_of_string in 
    let (lst1, lst2) = separate_and_sort_lists parsed_lists in
    let counts_map = Hashtbl.create (List.length lst2) in
    let _ = propagate_occurence_counts lst2 counts_map 0 0 in
    Ok (string_of_int (sum_occurences lst1 counts_map 0))
end