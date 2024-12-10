open Utils

let year = 2024
let day = 9

module Disk = struct
  type t = { 
    state : int array;
    indices: (int, (int * int)) Hashtbl.t
  }

  let space = -1

  let from_diskmap diskmap =
    let state_builder = Queue.create () in
    let indices = Hashtbl.create 16 in
  
    let process_block offset count block id =
      let block_size = int_of_char block in
      let is_file = (count mod 2) = 0 in
      let next_value = if is_file then id else space in
  
      for _ = 1 to block_size do
        Queue.add next_value state_builder
      done;

      if is_file then Hashtbl.replace indices id (offset, offset + block_size - 1);
  
      (offset + block_size, if is_file then id + 1 else id)
    in
  
    let rec process_blocks index count id offset =
      if index = String.length diskmap then ()
      else
        let block = String.get diskmap index in
        let (offset, next_id) = process_block offset count block id in
        process_blocks (index + 1) (count + 1) next_id offset
    in
    process_blocks 0 0 0 0;
  
    let state = 
      state_builder 
      |> Queue.to_seq
      |> Array.of_seq
    in
    { state; indices }

  let print disk =
    for i = 0 to Array.length disk.state - 1 do
      let value = Array.get disk.state i in 
      print_string (if value = space then "." else string_of_int value)
    done;
    print_endline ""

  let move_block disk l r = 
    let temp = disk.state.(l) in
    disk.state.(l) <- disk.state.(r);
    disk.state.(r) <- temp

  let move_whole_block disk left_start right_id =
    let (right_start, right_end) = Hashtbl.find disk.indices right_id in
    let left_ptr = ref left_start in
    for i = right_start to right_end do
      disk.state.(!left_ptr) <- disk.state.(i);
      disk.state.(i) <- space;
      incr left_ptr;
    done

  let next_available_move_space disk right_id =
    let (right_start, right_end) = Hashtbl.find disk.indices right_id in
    let len = right_end - right_start + 1 in
    let rec find_space in_space space_start cur last_valid =
      if cur = -1 then 
        last_valid
      else begin
        let value = disk.state.(cur) in
        if in_space && space_start - cur >= len then
          if value = space then
            find_space true space_start (cur - 1) (Some cur)
          else
            find_space false space_start (cur - 1) (Some (cur + 1))
        else if value <> space then
          find_space false space_start (cur - 1) last_valid
        else if (not in_space) && value = space then
          find_space true cur (cur - 1) last_valid
        else
          find_space in_space space_start (cur - 1) last_valid
      end
    in
    find_space false (-1) right_end None

  let move_blocks disk =
    let left = ref 0 in
    let right = ref (Array.length disk.state - 1) in
    while !left <= !right do
      while !left <= !right && Array.get disk.state !left <> space do
        incr left
      done;
      while !left <= !right && Array.get disk.state !right = space do
        decr right
      done;

      if !left <= !right then
        move_block disk !left !right;
    done

  let move_blocks_descending disk =
    let rec calculate_max_id i state =
      let value = state.(i) in
      if value <> space then value
      else calculate_max_id (i - 1) state
    in

    let rightmost_id = calculate_max_id (Array.length disk.state - 1) disk.state in
    let right_id = ref rightmost_id in
    while !right_id >= 0 do
      match next_available_move_space disk !right_id with
      | Some start -> 
        move_whole_block disk start !right_id;
        decr right_id;
      | None -> 
        decr right_id;
    done

  let checksum disk =
    let rec aux index result =
      if index = Array.length disk.state then result
      else 
        let value = Array.get disk.state index in
        let product = if value <> space then index * value else 0 in
        aux (index + 1) (result + product)
    in
    aux 0 0
end

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    let disk = 
      input
      |> String.trim
      |> Disk.from_diskmap 
    in
    Disk.move_blocks disk;
    Ok (disk |> Disk.checksum |> string_of_int)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    let disk = 
      input
      |> String.trim
      |> Disk.from_diskmap 
    in
    Disk.move_blocks_descending disk;
    Ok (disk |> Disk.checksum |> string_of_int)
end