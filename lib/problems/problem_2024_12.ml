let year = 2024
let day = 12

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
  type t = 
    | Left
    | Right
    | Up
    | Down

  let left = Point.{ x = -1; y = 0 }
  let right = Point.{ x = 1; y = 0 }
  let up = Point.{ x = 0; y = -1 }
  let down = Point.{ x = 0; y = 1 }
  
  let adjacent = [left; right; up; down]
  let tagged_adjacent = [(left, Left); (right, Right); (up, Up); (down, Down)]
end

module PointSet = Set.Make(Point)

module Map = struct
  type t = { map : char array array; width : int; height : int }

  let create input =
    let map = input 
      |> String.split_on_char '\n'
      |> List.filter (fun row -> row <> "") in
    let width = map |> List.hd |> String.length in
    let height = map |> List.length in
    let map = Array.of_list @@ List.map (fun row -> row |> String.to_seq |> Array.of_seq) map in
    { map; width; height }

  let in_bounds map point = 
    point.Point.x >= 0 && point.Point.x < map.width && point.Point.y >= 0 && point.Point.y < map.height

  let get map point = map.map.(point.Point.y).(point.Point.x)

  let get_opt map point = 
    if in_bounds map point then Some (get map point)
    else None

  let adjacent_points map point ~allow_out_of_bounds = 
    if not allow_out_of_bounds && not (in_bounds map point) then []
    else
      Direction.adjacent
      |> List.map (fun p -> Point.add p point) 
      |> List.filter (fun p -> allow_out_of_bounds || in_bounds map p)

  let tagged_adjacent_points map point ~allow_out_of_bounds = 
    if not allow_out_of_bounds && not (in_bounds map point) then []
    else
      Direction.tagged_adjacent
      |> List.map (fun (p, tag) -> (Point.add p point, tag)) 
      |> List.filter (fun (p, _) -> allow_out_of_bounds || in_bounds map p)

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

  let region_points map point seen_points =
    let value = get map point
    and todo = Queue.create ()
    and result = Hashtbl.create 32 in
    let rec add_points = function
      | x :: xs -> 
        Queue.add x todo;
        add_points xs
      | [] -> ()
    in
    let process_point p =
      if get map p = value && not (Hashtbl.mem seen_points p) then (
        add_points (adjacent_points map p ~allow_out_of_bounds:false);
        Hashtbl.add seen_points p ();
        Hashtbl.add result p ()
      )
    in
    let rec process_points () =
      if Queue.is_empty todo then ()
      else begin
        let first = Queue.take todo in
        process_point first;
        process_points ()
      end
    in

    Queue.add point todo;
    process_points ();
    Hashtbl.fold (fun key _ acc -> key :: acc) result []

  let area points = List.length points

  let perimeter map first_point points =
    let value = get map first_point
    and perimeter_points = Hashtbl.create 32 in
    let process_adjacent_point p = 
      let valid_value = not (in_bounds map p) || get map p <> value in
      if valid_value && not (Hashtbl.mem perimeter_points p) then
        Hashtbl.add perimeter_points p ()
    in
    let rec process_adjacent_points = function
      | x :: xs ->
        process_adjacent_point x;
        process_adjacent_points xs
      | [] -> ()
    in
    let rec loop = function
      | p :: rest -> 
        process_adjacent_points @@ adjacent_points map p ~allow_out_of_bounds:true;
        loop rest
      | [] -> ()
    in
    let count_adjacent_points p =
      let rec aux sum = function
        | x :: xs ->
          if get map x = value && List.mem x points then aux (sum + 1) xs
          else aux sum xs
        | [] -> sum
      in
      if not (in_bounds map p) || List.length points = 1 then 1
      else aux 0 (adjacent_points map p ~allow_out_of_bounds:false)
    in
    loop points;
    Hashtbl.fold (fun p _ acc -> acc + (count_adjacent_points p)) perimeter_points 0
  
  let fencing_price map =
    let seen_points = Hashtbl.create 32
    and plant_regions = Hashtbl.create 32
    and next_point = generator map in
    let rec map_loop () = 
      match next_point () with
      | Some point -> 
        if not (Hashtbl.mem seen_points point) then begin
          let region_points = region_points map point seen_points in
          Hashtbl.add plant_regions point region_points;
          map_loop ()
        end else map_loop ()
      | None -> ()
    in
    map_loop ();
    Hashtbl.fold (fun first_point points acc ->
      let region_area = area points in
      let region_perimeter = perimeter map first_point points in
      let region_fence_price = region_area * region_perimeter in
      acc + region_fence_price
    ) plant_regions 0
end

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    let map = Map.create input in
    let price = Map.fencing_price map in
    Ok (string_of_int price)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end