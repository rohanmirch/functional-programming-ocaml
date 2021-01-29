(* Student name: Rohan Mirchandani *)
(* CMS cluster login name: rmirchan *)
module Loc =
  struct
    type t = int * int

    let compare = Pervasives.compare
  end

module type Storage =
  sig
    type t
    type loc = Loc.t

    val make    : int -> int -> t
    val get     : t -> loc -> int option
    val set     : t -> loc -> int -> t
    val has_loc : t -> loc -> bool
    val remove  : t -> loc -> t
  end

(*
 * Imperative implementation.
 * The data representation is an array of arrays of integers.
 * A null location is represented by the number -1 stored at the location.
 *)
module ImpStorage : Storage =
  struct
    type t   = int array array
    type loc = Loc.t

    let make nrows ncols = 
       if nrows < 1 || ncols < 1 then invalid_arg "make: invalid arguments"
       else Array.make_matrix nrows ncols (-1)


    let get data (row, col) = 
      if row < 0 || col < 0 || row > ((Array.length data) - 1) ||
      col > ((Array.length data.(0)) - 1) then None
      else (
      if data.(row).(col) = -1 then None
      else Some data.(row).(col)
        )

    let set data (row, col) i = 
      if i < 0 || row < 0 || col < 0 then  invalid_arg "set: invalid arguments"
      else 
      begin
      data.(row).(col) <- i;
      data
      end

    let has_loc data (row, col) =
      if get data (row, col) = None then false
      else true

    let remove data (row, col) =
      if has_loc data (row, col) = false then data
      else 
        begin
          (* Set that value to a null value*)
          data.(row).(col) <- -1;
          data

        end

  end

(*
 * Functional implementation.
 * The data representation is a map between locs and integers.
 * A null location is represented by the absence of the loc in the map.
 *)

(*helper helper function that maps true to 1 and 0 to false*)

module FunStorage : Storage =
  struct
    module LocMap = Map.Make(Loc)

    type t = 
      {
        contents : int LocMap.t;
        nrows    : int;
        ncols    : int
      }

    type loc = Loc.t

    let make nrows ncols = 
      if nrows < 1 || ncols < 1 then invalid_arg "make: invalid arguments"
      else {
        contents = LocMap.singleton (-1, -1) (-1);
        nrows    = nrows;
        ncols    = ncols
        }

    
    let get data (row, col) = 
      if row < 0 || col < 0 then None
    else 
      if LocMap.mem (row, col) data.contents = false then None
      else Some (LocMap.find (row,col) data.contents)

    let set data (row, col) i = 
      let board = data.contents in
      if i < 0 || row < 0 || col < 0 || row > data.nrows - 1 || col > data.ncols - 1 
        then  invalid_arg "set: invalid arguments"
      else {data with contents = (LocMap.add (row, col) i board)}

  
    let has_loc data (row, col) = 
      if get data (row, col) = None then false
      else true

    let remove data (row, col) =
      if has_loc data (row, col) = false then data
      else 
        let your_mom = data.contents in
        {data with contents = LocMap.remove (row,col) your_mom}
      
  end

