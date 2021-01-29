  (* name: Rohan Mirchandani *)
  (* login: rmirchan *)


  (* 1.A *)

  (* marge is O(log(n)). If n is odd, you decrement, and recurse on half. 
  	If n is even, you just recurse on half. The fact that you keep
  	recursing on basically half the value gives this a ~log2(n) runtime,
    so it's O(log(n)) *)

  (* 1.B *)

  (* homer is O(n). In the worst-case scenario, you call homer on an odd
  	 number, and then homer will keep calling itself on decrementing 
  	 odd numbers untilit reaches -1. This runtime is linear with the size of
      (1/2)n so we get O(n) runtime. *)


  (* 1.C *)

  (* bart is  O(log(n)). In the worst case scenario, n > 1 (n is greater)
   than both a and b start) so you have to call iter a (b*2) and double b 
   until be reaches n. This takes log2(n). Then, once b >=n, you 
   increase a by a factor of two each time until it also
   reaches n, which also takes log2(n). This means that the totral 
   time complexity is  ~ log2(n) + log2(n) ->  O(log(n)) *)


  (* 1.D *)

  (* Lets first anayze the complexity of each component of merge_sort3:
		- Time comlexity of merge_sort3 = T(N), c1 + c2 = c are constants
		- split in thirds -> O(n) = c1*n
		- sort thirds -> 3*T(N/3)
		-> Merge in order -> O(n) = c2*n

		Total:

		T(n) = c*N + 3*T(n/3)
		T(n) = c*N + 3*(c*n/3 + 3*T(n/9))
		T(n) = c*N + 3*c*n/3 + 9*c*n/9 + ...
		T(n) = c*n + c*n + c*n + ...    -> ther will be log3(n) terms of c*n
		T(1) = 1
		T(N) = O(n) * log(n) = O(n*log(n))
		



  		*)


  (* 2.1 *)

let split3 lst = 
	let rec insert l split_l = 
	match l, split_l with
	| ([], _) -> split_l
	| (h1 :: [], (l1, l2, l3)) -> (l1 @ [h1*1], l2, l3)
	| (h1 :: h2 :: [], (l1, l2, l3)) -> (l1 @ [h1], l2 @ [h2], l3)
	| (h1::h2::h3::t, (l1, l2, l3)) -> 
	                    insert t (l1 @ [h1], l2 @ [h2], l3 @ [h3])

	in 
		insert lst ([],[],[])

let merge3 list1 list2 list3  = 
	let rec merge2 l1 l2 new_lst = 
	match l1, l2 with
		| ([], _) -> new_lst @ l2
		| (_, []) -> new_lst @ l1
		| (h1::t1, h2::t2) when h1 <= h2*1 -> merge2 t1 l2 (new_lst @ [h1])
		| (h1::t1, h2::t2) -> merge2 l1 t2 (new_lst @ [h2])
	in
		merge2 list3 (merge2 list1 list2 []) []


  (* 2.2.a *)

let smallest_index lst = 
	let rec find_min_index l min_val min_index index=
	match l with
	| [] -> min_index
	| h :: t when h < min_val*1 -> find_min_index t h index (index + 1) 
	| h :: t -> find_min_index t min_val min_index (index + 1) 
in 
	match lst with
	|[] -> invalid_arg "smallest_index: not enough elements"
	| h :: t -> find_min_index lst h 0 0

  (* 2.2.b *)

let flip_n n lst =
	let rec iter reverse remain i =
	match i, remain with
	| (0, _) -> reverse @ remain
	| (_, []) -> invalid_arg "flip_n: not enough elements"
	| (i, h :: t ) -> iter ( h(**1*) :: reverse) t (i - 1)

in
	iter [] lst n

  (* 2.2.c *)

let block_sort1 lst = 
	match lst with
	| [] -> []
	| _ -> flip_n ((smallest_index lst) + 1) lst


  (* 2.2.d *)

  (* The block sort process would be an example of generative recursion. 
  	While the
   highest-level function, block_sort, performs in-place
    recursion (which seems sturtcural) on the tail of the list, block_sort1
   calls flip_n, which creates new reverse lists and connects them together, 
   which is generative. This means we are techinically recursing
    on subparts of data we generate. Since the deepest mechanism of block_sort
   is generative, I think the whole function is generative recursion.  *)

 let block_sorti lst = 
 	let rec iter l n = 
 		match block_sort1 l with
 		| [] -> []
 		| h :: t -> [h] @ iter t n

 	in iter lst []

 (* 3.1.a *)

  let linrec is_base on_base splitter combine =
    let rec f x =
      if is_base x then
        on_base x
      else
        (* split using the splitter function *)
        (* recurse on the second item in the tuple returned by splitter, 
           using f *)
        (* combine the results using the combine function *)
        (* TODO *)
        let parts = splitter x in
        let first = fst parts in
        let second = f (snd parts) in
        combine first second
    in f  (* return the resulting recursive function *)

 (* 3.1.b *)

 	let insert_r item =	
    (* two base cases: the empty list 
     * and when the item < the first element of the list *)
    let is_base lst = lst = [] || item <= List.hd lst in
  
    (* for both base cases, put the item on the front of the list *)
    let on_base lst = item :: lst in
  
    (* split the list.  Hint: structural recursion. *)
    let splitter lst = (List.hd lst, List.tl lst) in
  
    (* combine to get the final result *)
    let combine first rest_after_rec = first :: rest_after_rec in
  
      linrec is_base on_base splitter combine


 (* 3.1.c *)
  let insertion_sort =
    (* base case: the list is empty *)
    let is_base lst = lst = [] in
  
    (* if it's a base case, return the empty list *)
    let on_base _ = [] in
  
    (* otherwise, split (hint: structural recursion again) *)
    let splitter lst = (List.hd lst, List.tl lst) in
  
    (* then combine *)
    let combine first rest_after_rec = insert_r first rest_after_rec in
  
      linrec is_base on_base splitter combine
  

 (* 3.2.a *)

 let binrec is_base on_base splitter combine =
    let rec f x =
      if is_base x then
        on_base x
      else
        (* split using the splitter function *)
        (* recurse on the second and third items in the tuple 
           returned by splitter, using f *)
        (* combine the results using the combine function *)
        (* TODO *)
        let fst (a,_,_) = a in
        let snd (_,b,c) = b in 
        let trd (_, _,c) = c in
        let parts = splitter x in
        let first = fst parts in
        let second = f (snd parts) in
        let third = f (trd parts) in

        combine first second third

    in f  (* return the resulting recursive function *)

 (* 3.2.b *)

 let quicksort =
    let is_base lst = lst = [] in
    let on_base _ = [] in
    let splitter lst =
      match lst with
        | [] -> invalid_arg "quicksort: can't split"
        | h :: t -> ([h*1], List.filter (fun x -> x < h) t,
         List.filter (fun x -> x >= h) t)
    in
    let combine pivot lt ge = lt @ pivot @ ge in
      binrec is_base on_base splitter combine

 (* 3.3.a *)

  let tailrec is_base on_base next =
    let rec f inputs =
      if is_base inputs then
      	on_base inputs
      else
      	f (next inputs)

    in f  (* return the tail-recursive function *)

 (* 3.3.b *)

   let insert_i item lst =
    let is_base (_, rest) = rest = [] || item <= List.hd rest in
    let on_base (prev, rest) = prev @ [item] @ rest in
    let next (prev, rest) = (prev @ [List.hd rest], List.tl rest) in
    let iter = tailrec is_base on_base next in
      iter ([], lst)

 (* 3.3.c *)


  let insertion_sort_i lst =
    let is_base (_, rest) = rest = [] in
    let on_base (prev, _) = prev in
    let next (prev, rest) = (insert_i (List.hd rest) prev, List.tl rest) in
    let iter = tailrec is_base on_base next in
      iter ([], lst)



(* 4.1 *)

 type tree =
    | Leaf
    | Node of int * int * tree * tree   (* level, value, left/right subtrees *)

  (* Level of an AA tree. *)
  let level = function
    | Leaf -> 0
    | Node (lvl, _, _, _) -> lvl

 let rec member n t = 
	match t with
	| Leaf -> false
	| Node (_, data, _, _) when  data = n -> true 
	| Node (_, data, _, rt) when data < n -> member n rt
	| Node (_, data, lt, _) when data > n -> member n lt 
	| _ -> false


(* 4.2 *)


let skew t = 
	match t with
	(*Skewable if left child level is the same as parent level*)
	| Node (lvl, v, Node (lvl2, v2, ll, lr), rr) when lvl2 = lvl ->

			Node (lvl2, v2, ll, Node(lvl, v, lr, rr))

	| _ -> t 



let split t = 
	match t with

	| Node (lvl, v, lt, Node(lvl2, v2, rl, rr)) when lvl = lvl2 
												&& (level rr) = lvl2 ->

			Node(lvl2 + 1, v2, Node(lvl, v, lt, rl), rr)

	| _ -> t



  let rec insert item t =
    match t with
      | Leaf -> Node (1, item, Leaf, Leaf)
      | Node (lvl, v, l, r) ->
      	if v = item then t
      	else

      		if item > v then  split (skew (Node (lvl, v, l, insert item r)))
      		else split (skew (Node (lvl, v, insert item l , r)))


















