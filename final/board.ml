(* Student name: Rohan Mirchandani *)
(* CMS cluster login name: rmirchan *)

open Storage

(*
 * Helper functions.
 *)

(* Return `true` if a loc is valid on a board of size
 * `nrows` rows by `ncols` columns. *)
let ok_loc nrows ncols (row, col) =
  row >= 0 && row < nrows && col >= 0 && col < ncols

(* Raise an `Invalid_argument` exception due to a bad location.
 * `name` is the name of the function, and `cause` is the
 * reason for the argument being bad. *)
let bad_loc name cause (row, col) =
  let msg = 
    Printf.sprintf "%s: %s;  row = %d col = %d%!" 
      name cause row col
  in
    invalid_arg msg


(*
 * The board module type and module.  
 * It represents the state of the knight's tour solution.
 *)

let h2 n = match n with
  | true -> 1
  | false -> 0

(* Used for appending locations to k_move_list *)
let h3 n loc = match n with
  | true -> [loc]
  | false -> []




module type Board =
  sig
    type loc = Loc.t
    type t

    val make                    : int -> int -> t
    val get_dimensions          : t -> int * int
    val get_last                : t -> loc
    val get_index               : t -> loc -> int option
    val get_reachable           : t -> loc -> int option
    val get_loc_counts_from_loc : t -> loc -> (loc * int) list
    val place                   : t -> loc -> t
    val undo                    : t -> t
    val is_solved               : t -> bool
    val get_placed              : t -> loc list
  end

module Make (S: Storage) : Board =
  struct
    type loc = Loc.t

    type t = 
      {
        nrows      : int;
        ncols      : int;
        size       : int;       (* total # of squares on board *)
        placed     : loc list;  (* locations of all knights placed on board *)
        last_index : int;       (* index of last knight placed *)
        indices    : S.t;
        reachable  : S.t
      }

    (* Helper functions. *)

    let check_bounds board loc = 
      ok_loc board.nrows board.ncols loc

  (* Gets number of reachable states *)
  let get_reach nrows ncols (r,c) =
    h2 (ok_loc nrows ncols (r - 2, c - 1)) +
    h2 (ok_loc nrows ncols (r - 2, c + 1)) +
    h2 (ok_loc nrows ncols (r - 1, c + 2)) +
    h2 (ok_loc nrows ncols (r + 1, c + 2)) +
    h2 (ok_loc nrows ncols (r + 2, c + 1)) +
    h2 (ok_loc nrows ncols (r + 2, c - 1)) +
    h2 (ok_loc nrows ncols (r + 1, c - 2)) +
    h2 (ok_loc nrows ncols (r - 1, c - 2))

    let init_reachable nrows ncols =
      let r_board = S.make nrows ncols in
      let rec iter b r c = 
        match () with 
        |_ when r = nrows -> b (* Reached the end of the board *)
        |_ when c = ncols - 1 && r < nrows -> 
            (* Reach the end of row, move to next row and set col to 0 *)
            iter (S.set b (r,c) (get_reach nrows ncols (r, c))) (r + 1) 0
        |_ -> iter (S.set b (r,c) (get_reach nrows ncols (r, c))) r (c + 1)

      in
      iter r_board 0 0 

    (* Interface functions. *)

    let make nrows ncols = 
      {
        nrows      = nrows;
        ncols      = ncols;
        size       = nrows * ncols;
        placed     = [];
        last_index = 0;
        indices    = S.make nrows ncols;
        reachable  = init_reachable nrows ncols
      }

    let get_dimensions board =
      (board.nrows, board.ncols)

    let get_last board =
      match board.placed with
        | [] -> raise Not_found
        | h :: _ -> h

    let get_index board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_index" "location off board" loc
      else
        S.get board.indices loc

    let get_reachable board loc = 
      if not (check_bounds board loc) then
        bad_loc "get_reachable" "location off board" loc
      else
        S.get board.reachable loc

    (*Returns a list of UNOCCUPIED locations a Knight's move away from loc *)
    let k_move_list board (r,c) = 
      h3 (S.has_loc board.reachable (r - 2, c - 1)) (r - 2, c - 1) @
      h3 (S.has_loc board.reachable (r - 2, c + 1)) (r - 2, c + 1) @
      h3 (S.has_loc board.reachable (r - 1, c + 2)) (r - 1, c + 2) @
      h3 (S.has_loc board.reachable (r + 1, c + 2)) (r + 1, c + 2) @
      h3 (S.has_loc board.reachable (r + 2, c + 1)) (r + 2, c + 1) @
      h3 (S.has_loc board.reachable (r + 2, c - 1)) (r + 2, c - 1) @
      h3 (S.has_loc board.reachable (r + 1, c - 2)) (r + 1, c - 2) @
      h3 (S.has_loc board.reachable (r - 1, c - 2)) (r - 1, c - 2)


    let tuple_maker reachable loc = 
        match (S.get reachable loc) with
        | Some(a) -> (loc, a)
        | _ -> (loc, 0)
    


    let get_loc_counts_from_loc board loc = 
      if check_bounds board loc = false then invalid_arg "not valid board location"
      else
        List.map (tuple_maker board.reachable) (k_move_list board loc)

    (* Updates the reachable table given that you just places a knight at location
        and you have a list of locations you can reach *)
    let rec update_reach r_board r_list = 
        match r_list with
        | [] -> r_board
        | h::t -> update_reach (S.set r_board (fst h) ((snd h) - 1)) t


    let place board loc = 
      let ind = board.indices in
      let pl = board.placed in
      let li = board.last_index in
      let reach_board = board.reachable in

      (* Check if it is valid on the board*)
      if check_bounds board loc = false || S.has_loc ind loc == true 
          then invalid_arg "can't place knight here"
     
     else 
    
         
      (* Now will check if it is a valid move based on last piece *)   
      if List.length pl > 0 then 
      
      (if List.mem loc (k_move_list board (List.hd pl)) == false 
          then invalid_arg "can't place knight here"
      else (

          (* Get list of reachable locations *)
      let reachable_list = get_loc_counts_from_loc board loc in
      (*Call helper function to update reachable board *)
      let new_board = update_reach reach_board reachable_list in
      (* Set original location to null *)
      let newest_reach = S.remove new_board loc in

      let new_placed = loc :: pl in

      { board with 
        placed = new_placed; 
        last_index = (li + 1);
        indices    = S.set ind loc (li + 1);
        reachable  = newest_reach
        }

      )

      )
        
      else (
      (* Get list of reachable locations *)
      let reachable_list = get_loc_counts_from_loc board loc in
      (*Call helper function to update reachable board *)
      let new_board = update_reach reach_board reachable_list in
      (* Set original location to null *)
      let newest_reach = S.remove new_board loc in

      let new_placed = loc :: pl in

      { board with 
        placed = new_placed; 
        last_index = (li + 1);
        indices    = S.set ind loc (li + 1);
        reachable  = newest_reach
        }
      )

      (* Adds one to each reachable positions*)
    let rec update_reach2 r_board r_list = 
        match r_list with
        | [] -> r_board
        | h::t -> update_reach2 (S.set r_board (fst h) ((snd h) + 1)) t

    (* Gets number of reachable values (not only for empty board)*)

     let get_reach_num board (r,c) =
    h2 (S.has_loc board.reachable (r - 2, c - 1)) +
    h2 (S.has_loc board.reachable (r - 2, c + 1)) +
    h2 (S.has_loc board.reachable (r - 1, c + 2)) +
    h2 (S.has_loc board.reachable (r + 1, c + 2)) +
    h2 (S.has_loc board.reachable (r + 2, c + 1)) +
    h2 (S.has_loc board.reachable (r + 2, c - 1)) +
    h2 (S.has_loc board.reachable (r + 1, c - 2)) +
    h2 (S.has_loc board.reachable (r - 1, c - 2))

    let cut_head lst = 
    match lst with 
    | [] -> []
    | h::t -> t



    let undo board = 
      let ind = board.indices in
      let pl = board.placed in
      let li = board.last_index in
      let reach_board = board.reachable in
      if List.length pl = 0 then board 
      else
        ( 
        (* Head = last position*)
        let loc = List.hd pl in
        

        (* Get the number of reachables for last location *)
        let new_reachable_val = get_reach_num board loc in
        (* Update the last entry to its nyumber of reachable locations *)
        let reach_board = S.set reach_board loc new_reachable_val in

        (* List of reachable places from last position *)
        let reachable_list = get_loc_counts_from_loc board loc in
        (* Update those positions, adding one reachability this time *)
        let new_board = update_reach2 reach_board reachable_list in

        let new_placed = cut_head pl in
        

         { board with 
        placed = new_placed; 
        last_index = li - 1;
        indices    = S.remove ind loc;
        reachable  = new_board
        }
      )
      


    let is_solved board = board.last_index = board.size
    let get_placed board = List.rev board.placed
  end
