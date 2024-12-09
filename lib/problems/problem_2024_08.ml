let year = 2024
let day = 8

let combinate f lst =
  let rec loop tortoise = function
    | hare :: rest -> 
      f tortoise hare;
      loop tortoise rest
    | [] -> ()
  and aux = function
    | tortoise :: rest -> 
      loop tortoise rest;
      aux rest
    | [] -> ()
  in
  aux lst

module Point = struct
  type t = { x : int; y : int }

  let add p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
  let sub p1 p2 = { x = p1.x - p2.x; y = p1.y - p2.y }

  let two_collinear_points p1 p2 =
    let (dx, dy) = (p2.x - p1.x, p2.y - p1.y) in
    [{ x = p2.x + dx; y = p2.y + dy }; { x = p1.x - dx; y = p1.y - dy }]

  let collinear_points pred p1 p2 = 
    let points = Queue.create () in
    let (dx, dy) = (p2.x - p1.x, p2.y - p1.y) in
    let rec loop pos (dx, dy) =
      Queue.add pos points;
      let (x, y) = (pos.x + dx, pos.y + dy) in
      let next_collinear = { x; y } in
      if pred next_collinear then (
        loop next_collinear (dx, dy);
      )
    in
    loop p2 (dx, dy);
    loop p1 (-dx, -dy);

    points 
    |> Queue.to_seq 
    |> List.of_seq
end

module Direction = struct
  let left = Point.{ x = -1; y = 0 }
  let right = Point.{ x = 1; y = 0 }
  let up = Point.{ x = 0; y = -1 }
  let down = Point.{ x = 0; y = 1 }
end

module City = struct
  type t = { map : char array array; width : int; height : int }

  let create input =
    let rows = 
      input
      |> String.trim
      |> String.split_on_char '\n' in
    let map = Array.of_list @@ List.map (fun row -> row |> String.to_seq |> Array.of_seq) rows in
    let height = Array.length map in
    let width = Array.length @@ Array.get map 0 in
    { map = map; width = width; height = height }

  let in_bounds city p =
    let (y, x) = Point.(p.y, p.x) in
    x >= 0 && x < city.width && y >= 0 && y < city.height

  let get (y, x) city = 
    if not (in_bounds city Point.{y; x}) then None
    else Some (city.map.(y).(x))

  let generator city =
    let x = ref 0 in
    let y = ref 0 in
    let next () =
      if !y = city.height then
        None
      else
        let pos = (!y, !x) in
        if !x = city.width - 1 then (
          x := 0;
          incr y
        )
        else (
          incr x
        );
        Some pos
    
    in next

  let positions_by_antenna city =
    let table = Hashtbl.create 32 in
    let next_city_position = generator city in
    let rec aux () = 
      match next_city_position () with
      | Some (y, x) -> (
        let pos = Point.{ x = x; y = y } in
        match get (y, x) city with
        | Some c when c <> '.' -> (
            let points = try Hashtbl.find table c with Not_found -> [] in
            Hashtbl.replace table c (pos :: points)
          );
          aux ()
        | _ -> aux ();
      )
      | None -> ()
    in
    aux ();
    table

    let count_antinodes (counter : Point.t -> Point.t -> Point.t list) ~unrestricted city =
      let antinodes = Hashtbl.create 256
      and antenna_positions = positions_by_antenna city in

      let process_antinode_position antenna pos =
        match get Point.(pos.y, pos.x) city with
        | Some v when v <> antenna || unrestricted -> 
          if Hashtbl.find_opt antinodes pos = None then 
            Hashtbl.add antinodes pos ();
        | _ -> ()
      in
      let compute_antinode_locations antenna p1 p2 =
        let rec compute_antinode_location = function
        | antinode_pos :: rest -> 
          process_antinode_position antenna antinode_pos;
          compute_antinode_location rest
        | _ -> ()
        in
        let antinode_locations = counter p1 p2 in
        compute_antinode_location antinode_locations;
      in
      Hashtbl.iter (fun antenna positions -> combinate (compute_antinode_locations antenna) positions) antenna_positions;
      Hashtbl.length antinodes
end

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (input 
      |> City.create 
      |> City.count_antinodes Point.two_collinear_points ~unrestricted:false 
      |> string_of_int
    )
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    let city = City.create input in
    Ok (city 
      |> City.count_antinodes (Point.collinear_points @@ City.in_bounds city) ~unrestricted:true 
      |> string_of_int
    )
end