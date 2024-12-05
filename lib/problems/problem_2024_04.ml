let year = 2024
let day = 4

module Point = struct
  type t = { x : int; y : int }
  let add p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }

  let left = { x = -1; y = 0 }
  let right = { x = 1; y = 0 }
  let up = { x = 0; y = -1 }
  let down = { x = 0; y = 1 }

  let left_down = { x = -1; y = 1 }
  let right_down = { x = 1; y = 1 }
  let left_up = { x = -1; y = -1 }
  let right_up = { x = 1; y = -1 }
end

module Search = struct
  type search = { s : string list }

  let create str = 
    { s = String.split_on_char '\n' str |> List.filter (fun row -> row <> "") }
  let height s = List.length s.s
  let width s = List.hd s.s |> String.length

  let in_bounds p s = 
    p.Point.x >= 0 && p.Point.x < width s && p.Point.y >= 0 && p.Point.y < height s

  let get p s = 
    if p.Point.x < 0 || p.Point.x >= width s || p.Point.y < 0 || p.Point.y >= height s then 
      None
    else 
      let row = List.nth s.s p.y in
      Some (String.get row p.x)

  let is_word start word dir s =
    let rec aux cur i =
      if i = String.length word then 
        true
      else
        match get cur s with
        | None -> false
        | Some c ->
          if c <> word.[i] then false
          else aux (Point.add cur dir) (i + 1)
    in
    aux start 0

  let is_xmas center s =
    let p1 = Point.add center Point.left_up in
    let p2 = Point.add center Point.right_up in
    let p3 = Point.add center Point.left_down in
    let p4 = Point.add center Point.right_down in

    let valid_diagonal (positions, letters) =
      List.for_all2
        (fun pos letter -> match get pos s with Some c when c = letter -> true | _ -> false)
        positions letters
    in

    valid_diagonal ([p1; center; p4], ['M'; 'A'; 'S']) ||
    valid_diagonal ([p2; center; p3], ['M'; 'A'; 'S'])

  let count_matches word dir s =
    let rec count_from_point x y acc =
      if y >= height s then 
        acc
      else if x >= width s then 
        count_from_point 0 (y + 1) acc
      else
        let result = if is_word { Point.x = x; Point.y = y } word dir s then 1 else 0 in
        count_from_point (x + 1) y (acc + result)
    in
    count_from_point 0 0 0

  let count_xmas s =
    let rec aux x y acc =
      if y >= height s then acc
      else if x >= width s then aux 0 (y + 1) acc
      else
        let center = { Point.x = x; Point.y = y } in
        let count = if is_xmas center s then 1 else 0 in
        aux (x + 1) y (acc + count)
    in
    aux 0 0 0
end

module Part_1 = struct
  let run (input : string) : (string, string) result =
    let word = "XMAS" in
    let s = Search.create input in
    let directions = [
      Point.right; Point.left; Point.down; Point.up;
      Point.right_down; Point.left_down; Point.right_up; Point.left_up
    ] in
    let matches =
      List.fold_left (fun acc dir -> acc + Search.count_matches word dir s) 0 directions
    in
    Ok (string_of_int matches)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (Search.create input |> Search.count_xmas |> string_of_int)
end