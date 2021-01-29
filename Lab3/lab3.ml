(*Rohan Mirchandnani  CS4 Lab 3*)

(*A.1*)

type point = {x : float; y : float}
type segment = {startp : point; endp : point}


let midpoint_segment {startp = p1; endp = p2} = 
	{x = (p1.x +. p2.x) /. 2.0; y = (p1.y +. p2.y) /. 2.0} 

let segment_length {startp = p1; endp = p2} = 
	abs_float (sqrt ((p1.x -. p2.x)**2. +. (p1.y -. p2.y)**2.))

let print_point {x = x; y = y} =
	begin
	Printf.printf "(%g, %g)" x y
	end	

(*Abstraction functions*)
let make_point x y  = {x = x; y = y}
let make_segment a b = {startp = a; endp = b}
let get_coords {x = x; y = y} = (x, y)
let get_points {startp = p1; endp = p2} = (p1, p2)


(*A.2*)

type rectangle = {p1 : point; p2 : point}
type rectangle2 = {upx : float ; lowx : float; upy : float ; lowy : float}

(*Abstractions for rectangle implementation 1*)
let rectangle_lower_segment {p1 = a; p2 = b} = 
	make_segment a {x = b.x ; y = a.y} 

let rectangle_upper_segment {p1 = a; p2 = b} = 
	make_segment {x = a.x ; y = b.y} b

let rectangle_left_segment {p1 = a; p2 = b} = 
	make_segment a {x = a.x ; y = b.y}

let rectangle_right_segment {p1 = a; p2 = b} = 
	make_segment b {x = b.x ; y = a.y} 

let rectangle_perimeter {p1 = a; p2 = b} =
	segment_length (rectangle_lower_segment {p1 = a; p2 = b}) +.
	segment_length (rectangle_upper_segment {p1 = a; p2 = b}) +.
	segment_length (rectangle_left_segment {p1 = a; p2 = b}) +. 
	segment_length (rectangle_right_segment {p1 = a; p2 = b})

let rectangle_area {p1 = a; p2 = b} =
	segment_length (rectangle_lower_segment {p1 = a; p2 = b}) *.
	segment_length (rectangle_left_segment {p1 = a; p2 = b})

(*Accerssor functions for 2nd implementation*)

let rectangle_lower_segment2 {upx = a ; lowx = b; upy = c ; lowy = d} =
	make_segment (make_point a d) (make_point b d)

let rectangle_upper_segment2 {upx = a ; lowx = b; upy = c ; lowy = d} =
	make_segment (make_point a c) (make_point b c)

let rectangle_left_segment2 {upx = a ; lowx = b; upy = c ; lowy = d} =
	make_segment (make_point a c) (make_point a d)

let rectangle_right_segment2 {upx = a ; lowx = b; upy = c ; lowy = d} =
	make_segment (make_point b c) (make_point b d)

let rectangle_perimeter2 {upx = a ; lowx = b; upy = c ; lowy = d} =
	segment_length (rectangle_lower_segment2 {upx = a ; lowx = b; upy = c ; lowy = d}) +.
	segment_length (rectangle_upper_segment2 {upx = a ; lowx = b; upy = c ; lowy = d}) +.
	segment_length (rectangle_left_segment2 {upx = a ; lowx = b; upy = c ; lowy = d}) +. 
	segment_length (rectangle_right_segment2 {upx = a ; lowx = b; upy = c ; lowy = d})

let rectangle_area2 {upx = a ; lowx = b; upy = c ; lowy = d} =
	segment_length (rectangle_lower_segment2 {upx = a ; lowx = b; upy = c ; lowy = d}) *.
	segment_length (rectangle_left_segment2 {upx = a ; lowx = b; upy = c ; lowy = d})

(* for testing purposes *)

let make_rectangle {x = x1 ; y = y1} {x = x2 ; y = y2} =
	{p1 = make_point x1 y1; p2 = make_point x2 y2}

let make_rectangle2 a b c d =
	{upx = a ; lowx = b; upy = c ; lowy = d}

(*A.3*)
let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)

(*make pair x y  returns a function fun m -> m x y, which takes in a function m 
 and applies it to x and y. Thus, we get
 first (fun m -> m x y)  =  (fun m -> m x y) (fun x y -> x). In this case,
 we are passing in (fun x y -> x) as m in (fun m -> m x y), so we apply the
 (fun x y -> x) to x and y and get x.*)

(*substitution model for second (make_pair 1 2) 
evaluate second (make_pair 1 2)
	evaluate make_pair 1 2
		1 -> 1
		2 -> 2
		make_pair -> fun x y -> fun m -> m x y
		apply make_pair -> fun x y -> fun m -> m x y to 1 , 2
		substitute 1 for x and 2 for y in make_pair -> fun x y -> fun m -> m x y
		fun m -> m 1 2
evaluate second (fun m -> m 1 2)
	second -> fun second z -> z (fun x y -> y)
	apply fun second z -> z (fun x y -> y) to (fun m -> m 1 2)
	substitute (fun m -> m 1 2) for z in z (fun x y -> y) 
	evaluate (fun m -> m 1 2) (fun x y -> y)
		(fun x y -> y) -> (fun x y -> y)
		substitute (fun x y -> y) for m in m 1 2
		evaluate (fun x y -> y) 1 2
		1 -> 1
		2 -> 2
		 apply (fun x y -> y)  to 1 2
		 substitute 1 for x and 2 for y in y
		return 2
*)

(*A.4*)

let pow a b = int_of_float ((float_of_int a)**(float_of_int b))

let rec int_log a c = 
	match c with 
	| 1 -> 0 
	| _ when c mod a <> 0 -> 0
	| _ when c mod a = 0 -> 1 + int_log a (c/a)


let make_pairi x y = (pow 2 x) * (pow 3 y)
let firsti z =  int_log 2 z
let secondi z = int_log 3 z 


(*A.5*)

(*Abstraction layer*)

let zero = []

let is_zero = function
| [] -> true
| () :: _ -> false

let succ u = () :: u


let prev lst = 
	match lst with
	|[] -> invalid_arg "can't have unary nums less than 0"
	| h :: t -> t

let rec integer_to_unary n =
	if n = 0 then []
	else () :: integer_to_unary (n - 1)


let rec unary_to_integer lst = 
	match lst with 
	| [] -> 0
	| h :: t -> 1 + unary_to_integer t 

let unary_add lst1 lst2 =
	lst1 @ lst2

(*New definition*)
  type nat = Zero | Succ of nat
  
  let zero' = Zero
  
  let is_zero' = function
    | Zero -> true
    | Succ _ -> false
  
  let succ' u = Succ u
  
  let prev' n = 
	match n with
	| _ when is_zero' n -> invalid_arg "can't have unary nums less than 0"
	| Succ x -> x
	
	
	(* 
	No, apart from name changes and empty lists to Zero, the basic function
	  forms don't need to be changed ocaml.*)

  let rec integer_to_unary' = function
	|0 -> zero'
	|a -> succ' (integer_to_unary' (a - 1))
	
  let unary_to_integer' a = 
	let rec iter n x =
		if (integer_to_unary' x) = a then x
		else iter n (x + 1)
	in
		iter a 0

  let rec unary_to_integer' a =
  	match a with
  	| Zero -> 0
  	| Succ b -> 1 + unary_to_integer' b

		

  let unary_add' a b = 
	integer_to_unary' ((unary_to_integer' a) + (unary_to_integer' b))
   

(*A.6*)

let zerof = fun s -> fun z -> z 
let add1 n = fun s -> fun z -> s (n s z)
let one  = fun s -> fun z -> s z
let two = fun s -> fun z -> s (s z)
let three = fun s -> fun z -> s (s (s z))
let four = fun s -> fun z -> s (s (s (s z)))
let five = fun s -> fun z -> s (s (s (s (s z))))
let six = fun s -> fun z -> s (s (s (s (s (s z)))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))


let add m n s z = m s (n s z)

let church_to_integer m = 
	m (fun n -> n + 1) 0

(*A.7*)


(*
val church_to_integer : ((int -> int) -> int -> 'c) -> 'c 
church_to_integer zerof = 0 becuase zerof = 'a -> 'b -> 'b
which means the input it takes in, which is 
(fun n -> n + 1) in church_to_integer, is ignored and it simply 
returns the second argument, which is 0. 0 is 'b which is an int,
so it returns the int 0. a' is a function, b' is an int, and 'c' is an int.

church_to_integer one = 1 because 
one  = ('a -> 'b) -> 'a -> 'b = <fun>.
This means that a' and b' are ints, which means that c'
is also an int in church_to_integer = ((int -> int) -> int -> 'c) -> 'c.
*)


(*B.1*)

let rec last_sublist = function 
	| [] -> invalid_arg "last_sublist: empty list"
	| [a] -> [a]
	| h :: t -> last_sublist t


(*B.2*)
let  reverse lst = 
	let rec iter lst new_lst =
	match lst with
	| [] -> new_lst
	| h::t -> iter t (h :: new_lst)
 in
 iter lst []


(*B.3*)
let rec square_list = function
	| [] -> []
	| h :: t -> (h * h) :: square_list t

let square_list2 items = List.map (fun x -> x * x) items

(*B.4*)

(*The reason why its in reverse is he keeps appending h^2 to 
   and empty list because answer = [] to start. he keeps popping
   off the head of his list and making it the first value of 
   answer, so his answer list will be backwards.
  His second implementation doesn't work because the ::
  opeartor requires the list as the second parameter and 
  the value as the first. He has it the other way.
 We modify his code :
	
	let square_list items =
    let rec iter things answer =
      match things with
        | [] -> answer
        | h :: t -> iter t (answer @ [(h * h)])
    in iter items []
The resulting function would not be space efficient becuase
lists are immutable so (answer @ [(h * h)] creates a new list every
iteration.
*)

(*B.5.1*)
let count_negative_numbers lst = 
	let rec iter lst count =
	match lst with
	| [] -> count
	| h :: t when h < 0 -> iter t (count + 1) 
	| h :: t -> iter t count
in
	iter lst 0


(*B.5.2*)

let power_of_two_list n = 
	let rec iter n lst =
	match n with
	| 0 -> 1 :: lst
	| _ -> iter (n - 1) ((pow 2 n) :: lst)

in
	match n with
	|0 -> []
	|_ -> iter (n - 1) []

			
(*B.5.3*)

let prefix_sum lst = 
	let rec iter lst new_lst sum =
	match lst with
	| [] -> new_lst
	| h :: t -> (sum + h) :: iter t new_lst (sum + h)

in 
	iter lst [] 0


(*B.6*)

let deep_reverse lst = 
	let rec iter lst new_lst =
	match lst with
	| [] -> new_lst
	| h::t -> iter t (reverse h :: new_lst)
 in
 iter lst []


(*B.7*)


type 'a nested_list = 
    | Value of 'a
    | List of 'a nested_list list

let rec deep_reverse_nested lst = 
	let rec iter lst new_lst =
		match lst with
		| [] -> List new_lst
		| Value h :: t -> iter t (Value h :: new_lst)
		| List h :: t -> iter t ( iter h [] :: new_lst)

	in 
	match lst with
	| Value x -> Value x
	| List x -> iter x []




