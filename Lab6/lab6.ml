(* Rohan Mirchandani CS4 Lab6*)

(* A.1 *)

(*   
  
    FRAME 0 (initial environment)
      parent: none
      bindings:
        - : [primitive function -]
        * : [primitive function *]
 
    FUNCTION 0 (let factorial n = ...)
      env: FRAME 0
      param: n
      body: let rec iter .... in iter n 1



	FRAME 1 (let factorial = FUNCTION 0 in ...)
	parent: FRAME 0
	bindings:
		factorial : FUNCTION 0

	FRAME 2 (FUNCTION 0 applied to 3)
	parent: FRAME 0
	bindings:
		n : 3



	-- dummy frame for recursive functions
	-- not sure if it links to frame 0 or 2 ( i now think 2)
	-- dummy value frame for recursive functions

	FRAME 3 (Function call to iter) 
	parent: FRAME 2
	bindings:
		iter : <dummy value>

    FUNCTION 1 (let rec iter ...)
    env: FRAME 3
	param: m
	param: r
	body : if m = 0 then ... iter (m - 1) (r * m)

	
	FRAME 4 (FUNCTION 1 applied to 3 1)
	parent: FRAME 2
		bindings:
			m : 3
			r : 1


	FRAME 5 (FUNCTION 1 applied to 2 3)
	parent: FRAME 2
		bindings:
			m : 2
			r : 3

	FRAME 6 (FUNCTION 1 applied to 1 6)
	parent: FRAME 2
		bindings:
			m : 1
			r : 6

	FRAME 7 (FUNCTION 1 applied to 0 6)
	parent FRAME 2
		bindings:
			m : 0
			r : 6
  
  *) 



(* A.2 *)

 let factorial = 
    let f = ref (fun n -> 0) in

      let g = ref (fun n -> if n = 0 then 1 else n * !f (n-1)) in
      
        begin
        f := !g;
        !g
        end



(* B.1 *)

exception Stat_error of string

let make_stat_1 () = 
	let sum = ref 0. in
	let sumsq = ref 0. in
	let n = ref 0 in

	object

	method append num =
		(* Update the state variables*)
		n := !n + 1;
		sum := !sum +. num;
		sumsq := !sumsq +. (num *. num);

	method mean = 
		if !n = 0 then raise (Stat_error "need at least one value for mean")
		else 
		!sum /. (float_of_int !n)

	method variance = 
		if !n = 0 then raise (Stat_error "need at least one value for variance")

		else
		(!sumsq -. (!sum *. !sum) /. float_of_int !n) /. (float_of_int !n)

	method stdev = 
		if !n = 0 then raise (Stat_error "need at least one value for stdev")
		else
		sqrt ((!sumsq -. (!sum *. !sum) /. float_of_int !n) /. (float_of_int !n))
		

	method clear = 
		n := 0;
		sum := 0.;
		sumsq := 0.;

	end


(* B.2 *)




let make_stat_2 () = 
	let sum = ref 0. in
	let sumsq = ref 0. in
	let n = ref 0 in

	object(self)

	method append num =
		(* Update the state variables*)
		n := !n + 1;
		sum := !sum +. num;
		sumsq := !sumsq +. (num *. num);

	method mean = 
		if !n = 0 then raise (Stat_error "need at least one value for mean")
		else 
		!sum /. (float_of_int !n)

	method private _variance = 
		(!sumsq -. (!sum *. !sum) /. float_of_int !n) /. (float_of_int !n)
	
	method variance = 
		if !n = 0 then raise (Stat_error "need at least one value for variance")
		else
		self#_variance

	method stdev = 
		if !n = 0 then raise (Stat_error "need at least one value for stdev")
		else
		sqrt self#_variance
		

	method clear = 
		n := 0;
		sum := 0.;
		sumsq := 0.;

	end



(* C.1 *)

 module type PRIORITY_QUEUE =
    sig
      exception Empty
  
      type elem      (* Abstract type of elements of queue. *)
      type t         (* Abstract type of queue. *)
  
      val empty      : t                (* The empty queue.         *)
      val is_empty   : t -> bool        (* Check if queue is empty. *)
      val insert     : t -> elem -> t   (* Insert item into queue.  *)
      val find_min   : t -> elem        (* Return minimum element.  *)
      val delete_min : t -> t           (* Delete minimum element.  *)
      val from_list  : elem list -> t   (* Convert list to queue.   *)
    end



  module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
    struct
    exception Empty
      type elem = int
      type t = Leaf | Node of int * int * t * t
      let empty = Leaf
      let is_empty m = 
      	if m = Leaf then true
      	else false

      (* Helper functions *)
      let rank a = match a with
      | Node (_, rank, _, _) -> rank
      | Leaf -> 0

      let comp m a b = 
      	if rank a > rank b 
      	then Node (m, (rank a) + 1, b, a)
        else Node (m, (rank a) + 1, a, b)

      let rec merge lt rt = 
      	match (lt, rt) with 
      	| _ when is_empty rt -> lt
      	| _ when is_empty lt -> rt
      	| (Node (el, lrank, ll, lr), Node (er, rrank, rl, rr)) ->
      		if el < er then comp el ll (merge lr rt)
      		else comp er rl (merge lt rr)
      	| _ -> Leaf


      let insert tree element = 
       merge (Node (element, 0, Leaf, Leaf)) tree


      let find_min m = 
      	match m with 
      	| Leaf -> raise Empty
      	| Node (value, rank, t1, t2) -> value
      
      let delete_min t = 
      match t with 
      | Leaf -> raise Empty
      | Node (e, r, lt, rt) -> merge lt rt

      let from_list l = 
      	let rec iter lst tree = 
      		match lst with 
      		| [] -> tree
      		| h :: t -> iter t (insert tree h) 
      	in
      	iter l Leaf

    end 

let heap_sort lst =
	let q = PriorityQueue.from_list lst in
	let rec iter l queue = 
		match l with 
		| _ when PriorityQueue.is_empty queue -> l
		| _ -> 

			iter ((PriorityQueue.find_min queue) :: l) (PriorityQueue.delete_min queue) in

		List.rev (iter [] q)

(* C.2 *)


  type comparison = LT | EQ | GT
  
  (* Signature for ordered objects. *)
  module type ORDERED =
    sig
      type t
      val cmp: t -> t -> comparison
    end
  



 module MakePriorityQueue (Elt : ORDERED) 
    : (PRIORITY_QUEUE with type elem = Elt.t) =
    struct
      exception Empty
      type elem = Elt.t
      (* Instead of integers we have comprable elements *)
      type t = Leaf | Node of elem * int * t * t
      let empty = Leaf
      let is_empty m = 
      	if m = Leaf then true
      	else false


      (* Helper functions *)
      let rank a = match a with
      | Node (_, rank, _, _) -> rank
      | Leaf -> 0

     let comp m a b = 
      	if rank a > rank b 
      	then Node (m, (rank a) + 1, b, a)
        else Node (m, (rank a) + 1, a, b)

      let rec merge lt rt = 
      	match (lt, rt) with 
      	| _ when is_empty rt -> lt
      	| _ when is_empty lt -> rt
      	| (Node (el, lrank, ll, lr), Node (er, rrank, rl, rr)) ->
      		(* Use Elt.cmp to compare ordered elements *)
      		if (Elt.cmp el er) = LT then comp el ll (merge lr rt)
      		else comp er rl (merge lt rr)
      	| _ -> Leaf (* Just included to kill the warning *)


      let insert tree element = 
       merge (Node (element, 0, Leaf, Leaf)) tree


      let find_min m = 
      	match m with 
      	| Leaf -> raise Empty
      	| Node (value, rank, t1, t2) -> value
      
      let delete_min t = 
      match t with 
      | Leaf -> raise Empty
      | Node (e, r, lt, rt) -> merge lt rt

      let from_list l = 
      	let rec iter lst tree = 
      		match lst with 
      		| [] -> tree
      		| h :: t -> iter t (insert tree h) 
      	in
      	iter l Leaf


    end



  module OrderedString =
    struct
      type t = string
      let cmp x y = 
        if x = y then EQ else if x < y then LT else GT
    end

  (* Uses our MakePriotrityQueue to create a new module with orderedString*)
  module StringPQ = MakePriorityQueue(OrderedString)


  (* Redefine heapdort using StringPQ *)

  let heap_sort_2 lst =
	let q = StringPQ.from_list lst in
	let rec iter l queue = 
		match l with 
		| _ when StringPQ.is_empty queue -> l
		| _ -> 

			iter ((StringPQ.find_min queue) :: l) (StringPQ.delete_min queue) in

		List.rev (iter [] q)


(* D.1 *)

(* Making a helper function that is the actual Lazy/Expr *)
type 'a lazy_helper = Lazy of (unit -> 'a) | Expr of 'a
(* Making the actual type a reference to the helper function *)
type 'a lazy_t = ('a lazy_helper) ref


let make_lazy e =  ref (Lazy e)


let force lz = match !lz with
	(* If we get a lazy reference and its of Lazy type *)
	| Lazy a -> 
		(* Evaluates the a function *)
		let exp = a () in
		begin
		(* Sets the value in the lazy reference variable to be an expression *)
		lz := Expr (exp); 
		(* Returns the expression *)
		exp 
		end

	(* If we get an expression we just return the expression)*)
	| Expr a -> a



(* D.2 *)

  (* When you call non recursive function as an argument to y, it 
  gives the correct, full recursive function. *)

  let y = 
    fun f -> 
      (fun z -> z (`Roll z)) 
      (fun (`Roll w) -> f (fun x -> w (`Roll w) x))



 (* D.2.1 *)


 let almost_sum = 
 	fun f ->
 		fun lst -> 
 			match lst with
		 	| [] -> 0
		 	| h :: t ->  h + f t

let sum = y almost_sum


(* D.2.2 *)

(* Re-write iter to take in one argument*)
(*   let factorial n =
    let rec iter a =
    let n = fst a in
    let r = snd a in
      if n = 0
        then r
        else iter (n - 1, n * r)
    in
      iter (n, 1) *)


(* Now convert iter to y-combinator form *)

let almost_iter=
	fun f-> 
		fun a ->
	    let n = fst a in
	    let r = snd a in
	      if n = 0
	        then r
	      else f (n - 1, n * r)

let iter = y almost_iter


let factorial2 n = iter (n,1)



