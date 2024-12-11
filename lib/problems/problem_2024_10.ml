open Utils

let year = 2024
let day = 10

module Point = struct
  type t = { x : int; y : int }

  let add p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
  let to_string p = Printf.sprintf "(%d, %d)" p.x p.y
  let print p = print_endline (to_string p)

  let compare p1 p2 =
    match compare p1.x p2.x with
    | 0 -> compare p1.y p2.y
    | c -> c
end

module Direction = struct
  include Point
  let left = { x = -1; y = 0 }
  let right = { x = 1; y = 0 }
  let up = { x = 0; y = -1 }
  let down = { x = 0; y = 1 }
  
  let adjacent_straight = [left; right; up; down]
end

module PointSet = Set.Make(Point)

module Map = struct
  type t = { map : int array array; width : int; height : int }

  let lowest = 0
  let highest = 9

  let create input =
    let map = input 
      |> String.split_on_char '\n'
      |> List.filter (fun row -> row <> "") in
    let width = map |> List.hd |> String.length in
    let height = map |> List.length in
    let map = Array.of_list @@ List.map (fun row -> row |> String.to_seq |> Array.of_seq |> Array.map int_of_char) map in
    { map; width; height }

  let in_bounds map point = 
    point.Point.x >= 0 && point.Point.x < map.width && point.Point.y >= 0 && point.Point.y < map.height

  let get map point = 
    if in_bounds map point then Some map.map.(point.Point.y).(point.Point.x)
    else None

  let generator map =
    let x = ref 0 in
    let y = ref 0 in
    let next () =
      if !y = map.height then
        None
      else
        let pos = Point.{ x = !x; y = !y } in
        if !x = map.width - 1 then (
          x := 0;
          incr y
        )
        else (
          incr x
        );
        Some pos
    in next

  let adjacent_points point map =
    List.filter_map (fun p ->
      let adjacent_point = Point.add point p in
      if in_bounds map adjacent_point then
        (match get map point with
        | Some v when v >= lowest && v <= highest -> Some adjacent_point
        | _ -> None)
      else None
    ) Direction.adjacent_straight

  let lowest_positions map = 
    let next_position = generator map in
    let positions = Queue.create () in
    let rec loop () =
      let position = next_position () in
      match Option.bind position (get map) with
      | Some score when score = lowest -> 
        Queue.add (Option.get position) positions;
        loop ()
      | Some _ -> loop ()
      | None -> ()
      in
    loop ();

    positions 
    |> Queue.to_seq 
    |> List.of_seq

  let count_trailheads distinct map =
    let rec traverse_trailhead cur position visited =
      let rec traverse_aux cur adj = 
        let score = Option.get @@ get map adj in
        if PointSet.mem adj !visited then 0
        else if cur = highest - 1 && score = highest then 
          if (not distinct) then
            let () = visited := PointSet.add adj !visited in 1
          else 1
        else if cur = score - 1 then traverse_trailhead (cur + 1) adj visited
        else 0
      and traverse_loop cur pos = 
        let adjacent = adjacent_points pos map in
        List.fold_left (fun acc adj -> acc + traverse_aux cur adj) 0 adjacent
      in
      traverse_loop cur position
    in
    let rec traverse_iter = function
      | pos :: rest -> traverse_iter rest + traverse_trailhead 0 pos (ref PointSet.empty)
      | [] -> 0
    in

    traverse_iter @@ lowest_positions map
end

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (
      input 
      |> Map.create 
      |> Map.count_trailheads false
      |> string_of_int
    )
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (
      input 
      |> Map.create 
      |> Map.count_trailheads true
      |> string_of_int
    )
end