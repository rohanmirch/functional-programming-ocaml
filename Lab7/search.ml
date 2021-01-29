(* search.ml: search strategies *)
(* Student name:                *)
(* CMS cluster login name:      *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end




module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)
    (*exception Not_Found*)
    let search init = 
      let storage = S.create () in
      let history1 = [init] in
      let visited = DS.empty in
      let new_lists original child = child :: original in
      let push_helper s element = S.push element s in
            let rec iter storage visited = 
        if S.is_empty storage then raise Not_found
        else
          (let recent_hist = S.pop storage in
          let recent_board = List.nth recent_hist 0 in
          
          if DS.mem recent_board visited then iter storage visited
          else 
              (if D.is_solved recent_board then recent_hist
              else (
              
                let children = D.next recent_board in
                (* Create new histories *)
                let new_histories = List.map (new_lists recent_hist) children in

                let visited = DS.add recent_board visited  in
                begin
                List.iter (push_helper storage) new_histories;
                
                iter storage visited
                end

                )          
              )
          )

      in 
      begin
      S.push history1 storage;
      iter storage visited
      end
      











    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

