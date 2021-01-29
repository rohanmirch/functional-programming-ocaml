(* klotski.ml: core functionality of the Klotski game. *)
(* Student name:                *)
(* CMS cluster login name:      *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Pervasives.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Pervasives.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

let is_solved b = 
    let map = b.pieces in
    let solve_piece = LocSet.of_list [(3,1); (3,2); (4,1); (4,2)] in
    let predicate key value = LocSet.equal solve_piece value
    in CharMap.exists predicate map


let compare b1 b2 = 
    let board_1_pieces = LocSetSet.of_list (List.map snd (CharMap.bindings b1.pieces)) in
    let board_1_spaces = b1.unoccupied in
    let board_2_pieces = LocSetSet.of_list (List.map snd (CharMap.bindings b2.pieces)) in
    let board_2_spaces = b2.unoccupied in

    if LocSetSet.compare board_1_pieces board_2_pieces = 0 && 
        LocSet.compare board_1_spaces board_2_spaces = 0 then 0

    else LocSetSet.compare board_1_pieces board_2_pieces


let remove c ({ pieces = p; unoccupied = u } as b) = 
  let removed_pieces = CharMap.find c p in
  let new_board = CharMap.remove c p in
  let new_u = LocSet.union removed_pieces u in
  {pieces = new_board; unoccupied = new_u}


let add (c, p) { pieces = ps; unoccupied = u } = 
  if LocSet.subset p u then
  Some {pieces = CharMap.add c p ps; unoccupied = LocSet.diff u p}
else None

(* Helper functions for make_move*)
let move_up n (a,b)  = (a - n, b)
let move_down n (a,b)  = (a + n, b)
let move_right n (a,b)  = (a, b + n)
let move_left n (a,b)  = (a, b - n)

let rec is_valid (c, d, i) ({pieces = ps; unoccupied = u} as b) =
  (* Checks if new position is unoccupied *) 
  (* Add old position to unoccupied first so unoccupied 
      consiosts of a set of valid spaces *) 
  let piece_positions = CharMap.find c ps in
  let free_spaces = LocSet.union u piece_positions in

  if i = 0 then true
  else

  match d with 
  | Up -> 
      let new_positions = LocSet.map (move_up i) piece_positions in
      (LocSet.subset new_positions free_spaces) && (is_valid (c, d, i-1) b)
  | Down -> 
      let new_positions = LocSet.map (move_down i) piece_positions in
      (LocSet.subset new_positions free_spaces) && (is_valid (c, d, i-1) b)
  | Right -> 
      let new_positions = LocSet.map (move_right i) piece_positions in
      (LocSet.subset new_positions free_spaces) && (is_valid (c, d, i-1) b)
  | Left -> 
      let new_positions = LocSet.map (move_left i) piece_positions in
      (LocSet.subset new_positions free_spaces) && (is_valid (c, d, i-1) b)

let make_move (c, d, i) b =

  if (CharMap.mem c b.pieces) = false then None
else

   if ((is_valid (c, d, i) b) && (i >= 1)) then
   (
    let piece_positions = CharMap.find c b.pieces in 
     (* Removes the orioginal piece to the unoccipied and remove it from board *) 
    let temp_board = remove c b
  in
    match d with 
    | Up -> 
        let new_positions = LocSet.map (move_up i) piece_positions in
        (* Add the new positions to our temporary board *)
        add (c, new_positions) temp_board
        
    | Down -> 
        let new_positions = LocSet.map (move_down i) piece_positions in
        (* Add the new positions to our temporary board *)
        add (c, new_positions) temp_board
        
    | Right -> 
        let new_positions = LocSet.map (move_right i) piece_positions in
        (* Add the new positions to our temporary board *)
        add (c, new_positions) temp_board

    | Left -> 
        let new_positions = LocSet.map (move_left i) piece_positions in
        (* Add the new positions to our temporary board *)
        add (c, new_positions) temp_board
    )

else None


let rec get_moves c b d = 
  (* Returns a list of boards where block (key c) moves in direction
    d *)
  let rec iter n lst = 
    match (make_move (c, d, n) b) with
    | None -> lst
    | Some (new_board) -> iter (n+1) (new_board :: lst)
 in
  iter 1 []

let get_all_moves c b = 
    (get_moves c b Up) @ (get_moves c b Down) @ (get_moves c b Right) @ (get_moves c b Left)


let next b =
  (* Go though every key in b and call helper function on it,
      then add to master list of moves. *)
  (* Get a list of keys*)
  let key_list = (List.map fst (CharMap.bindings b.pieces)) in
  let rec helper k_list b_list = 
    match k_list with 
    | [] -> b_list
    | h :: t -> helper t ((get_all_moves h b) @ b_list)

  in helper key_list []

(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end 

