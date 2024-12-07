let year = 2024
let day = 6

exception Invalid_map_char of string * string
exception Invalid_direction of string

module Point = struct
  type t = { x : int; y : int }

  let left = { x = -1; y = 0 }
  let right = { x = 1; y = 0 }
  let up = { x = 0; y = -1 }
  let down = { x = 0; y = 1 }
  let no_block = { x = -1; y = -1 }

  let add p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
  let sub p1 p2 = { x = p1.x - p2.x; y = p1.y - p2.y }

  let switch_direction = function
    | { x = -1; y = 0 } -> up
    | { x = 1; y = 0 } -> down
    | { x = 0; y = -1 } -> right
    | { x = 0; y = 1 } -> left
    | _ -> raise (Invalid_direction "Direction is not up, down, left or right")
end

module Map = struct
  type t = { map : char array array; width : int; height : int }
  let guard = '^'

  let create input =
    let map = input 
      |> String.split_on_char '\n'
      |> List.filter (fun row -> row <> "") in
    let width = map |> List.hd |> String.length in
    let height = map |> List.length in
    let map = Array.of_list @@ List.map (fun row -> row |> String.to_seq |> Array.of_seq) map in
    { map = map; width = width; height = height }

  let get point map block = 
    if point.Point.x < 0 || point.Point.x >= map.width || point.Point.y < 0 || point.Point.y >= map.height then None
    else if point.Point.x = block.Point.x && point.Point.y = block.Point.y then Some 'O'
    else Some map.map.(point.Point.y).(point.Point.x)

  let start_point map = 
    let rec aux row = 
      if row >= map.height then raise (Invalid_argument "Guard was not found")
      else
        let open Utils in match index_of ((=) guard) map.map.(row) with
        | Some index -> { Point.x = index; Point.y = row }
        | None -> aux (row + 1)
      in
    aux 0

  let path start map =
    let visited = Hashtbl.create 16 in
    Hashtbl.add visited start ();
  
    let rec aux pos dir =
      let open Utils in 
      match get pos map Point.no_block with
      | Some value -> (
          match value with
          | '.' | '^' ->
              if not (Hashtbl.mem visited pos) then begin
                Hashtbl.add visited pos ();
                aux (Point.add pos dir) dir
              end else
                aux (Point.add pos dir) dir
          | '#' | 'O' ->
              let pos = Point.sub pos dir in
              let dir = Point.switch_direction dir in
              aux (Point.add pos dir) dir
          | unknown -> raise (Invalid_map_char ("Unknown character", string_of_char unknown)))
      | None -> ()
    in
    aux (Point.add start Point.up) Point.up;
    Hashtbl.fold (fun pos _ acc -> pos :: acc) visited [] 

  let path_has_paradox start block map =
    let visited = Hashtbl.create 16 in
    let rec aux pos dir =
      if Hashtbl.mem visited (pos, dir) then true
      else (
        Hashtbl.add visited (pos, dir) ();
        match get pos map block with
        | Some ('.' | '^') -> aux (Point.add pos dir) dir
        | Some ('#' | 'O') ->
            let pos = Point.sub pos dir in
            let dir = Point.switch_direction dir in
            aux (Point.add pos dir) dir
        | _ -> false)
    in
    aux (Point.add start Point.up) Point.up

  let count_paradoxical_paths map =
    let start = start_point map in
    let standard_path = path start map in
    let rec aux count = function
      | pos :: positions -> 
        let result = if path_has_paradox start pos map then 1 else 0 in
        aux (count + result) positions
      | [] -> count
    in
    aux 0 standard_path
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let map = Map.create input in
    Ok (map |> Map.path @@ Map.start_point map |> List.length |> string_of_int)
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    Ok (Map.create input |> Map.count_paradoxical_paths |> string_of_int)
end