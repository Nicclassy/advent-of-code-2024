open Utils

let year = 2024
let day = 5

let first_two_elements = function
  | a :: b :: _ -> Some (a, b)
  | _ -> None

module Order = struct
  type t = { before : int; after : int }
  let create ord = 
    let (before, after) = String.split_on_char '|' ord |> first_two_elements |> Option.get in 
    { before = int_of_string before; after = int_of_string after }
end

let middle lst =
  let mid = List.length lst / 2 in
  let rec aux i = function
    | cur :: _ when i = mid -> cur
    | _ :: rest -> aux (i + 1) rest
    | _ -> raise (Invalid_argument "No middle found");
  in
  aux 0 lst

let parse_input input = 
  let (ordering_rules, page_updates) = 
    input 
    |> Str.split (Str.regexp "\n\n") 
    |> first_two_elements 
    |> Option.get 
  in
  let ordering_rules = 
    ordering_rules
    |> String.split_on_char '\n'
    |> List.map Order.create
  in
  let page_updates = 
    page_updates
    |> String.split_on_char '\n'
    |> List.filter (fun u -> u <> "")
    |> List.map (fun u -> String.split_on_char ',' u |> List.map int_of_string)
  in
  (ordering_rules, page_updates)

let page_number_is_before update p query_before =
  let rec aux seen_query = function
    | cur :: _ when cur = p -> seen_query
    | cur :: rest -> aux (seen_query || cur = query_before) rest
    | [] -> raise (Invalid_argument "This should not happen")
  in
  aux false update

let page_number_is_ordered ordering_rules update p = 
  let rec aux = function
    | [] -> true
    | order :: _ when order.Order.before = p && page_number_is_before update p order.Order.after -> false
    | _ :: rest -> aux rest
  in
  aux ordering_rules

let update_is_correctly_ordered ordering_rules update = 
  List.for_all (fun p -> page_number_is_ordered ordering_rules update p) update

let is_successor ordering_rules update p =
  List.for_all (fun o ->
    if o.Order.after = p then
      page_number_is_before update o.Order.before p
    else
      true
  ) ordering_rules

let reorder_update ordering_rules update =
  let rec next_successor rules update = function
    | p :: _ when is_successor rules update p -> Some p
    | _ :: rest -> next_successor rules update rest
    | [] -> None
  in
  let rec remove_occurences p = function
    | cur :: rest when p = cur.Order.after || p = cur.Order.before -> remove_occurences p rest
    | cur :: rest -> cur :: remove_occurences p rest
    | [] -> []
  in
  let rec reorder_aux rules update reordered =
    match next_successor rules update reordered with 
    | Some s -> 
      let rules = remove_occurences s rules in
       reorder_aux rules update (reordered @ [s])
    | None -> (rules, reordered)
  in
  reorder_aux ordering_rules update []

let correct_updates_sum ordering_rules page_updates =
  let rec aux sum = function
    | cur :: rest when update_is_correctly_ordered ordering_rules cur -> aux (sum + (middle cur)) rest
    | _ :: rest -> aux sum rest
    | [] -> sum 
  in
  aux 0 page_updates

let incorrect_updates_reordered_sum ordering_rules page_updates =
  let rec aux rules sum = function
    | update :: rest when not (update_is_correctly_ordered ordering_rules update) -> 
      let (rules, reorder) = reorder_update rules update in
      aux rules (sum + (middle reorder)) rest
    | _ :: rest -> aux rules sum rest
    | [] -> sum 
  in
  aux ordering_rules 0 page_updates

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    let (ordering_rules, page_updates) = parse_input input in
    Ok (string_of_int (correct_updates_sum ordering_rules page_updates))
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    let (ordering_rules, page_updates) = parse_input input in
    Ok (string_of_int (incorrect_updates_reordered_sum ordering_rules page_updates))
end