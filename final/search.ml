(* Student name: Rohan Mirchandani *)
(* CMS cluster login name: rmirchan *)

open Storage
open Board
open Utils

exception Solution_not_found

module type Searcher =
  sig
    val search : int -> int -> int -> int -> bool -> (Loc.t list) option
  end

module Make (S : Storage) : Searcher =
  struct
    module B = Board.Make(S)
    module P = Print(B)

    (* Helper functions go here. *)

    (* Gets the mininimum reachabiloity value of t the (loc, r) list *)
    let min_reach lst = 
      let r_vals = snd (List.split lst) in
      List.nth (List.sort (fun f s -> f - s) r_vals) 0


    let search nrows ncols start_row start_col print =
      (*Helper fucntion iether returnx None or Some (solved board) *)
      let rec rec_search board = 
        if B.is_solved board then Some (board)
        else

          let loc = B.get_last board in
          let reachable_locs = B.get_loc_counts_from_loc board loc in

          if reachable_locs = [] then None
          else
            (* Get minimum reachability value *)
            let min_value = min_reach reachable_locs in
            (* Get the list of (location, min_value) *)
            let min_reach_list = List.filter 
            (fun x -> (snd x) = min_value) reachable_locs in
            (* Get a random location from the min location list*)
            let random_min_loc = fst (List.nth min_reach_list (Random.int 
                         (List.length min_reach_list))) in
            (*Place a knight at that location on the board*)
            let new_board = B.place board random_min_loc in

            rec_search new_board

      in

      (* Create board and place initial knight *)
      let b = B.make nrows ncols in
      let result = rec_search (B.place b (start_row, start_col)) in 
      match result with
      | None -> None
      | Some board when print = false -> Some (B.get_placed board)
      | Some board when print = true -> 
        begin
        P.print_board board true;
        Some (B.get_placed board)
        end



            









  end

