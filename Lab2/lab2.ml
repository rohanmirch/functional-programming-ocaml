(*Rohan Mirchandani CS4 Lab 2*)

open Num
let ni = num_of_int

(*A.1.2*)
(*The tree-recursive fibonacci algotihm is O(N) space complexity.
  When it evaluates fib (n - 1) + fib (n - 2), it will keep 
  evaluative the fib(n - 1) calls until it reaches the base case, then
  it pops the answers back up the evaluation stack, which can only
  have up to N pending operations (max depth of tree brach).*)

(*A.2.a*)
(*p is applied 5 times.*)
(*A.2.b*)
(*We need A/.1 = 3^n, which solves to give us n = ceil(log_3(10A)) (where ceil is the ceiling
  this means that the space complexity is O(log_3(A)).  

  The time complexity is also O(log_3(A)) because its just twice the number of calls.
 *)


(*A.3.1*)

let rec fast_expt b n =
    let is_even m = m mod 2 = 0 in
    let square m = m * m in
    match n with
    | 0 -> 1
    | _ when is_even n -> square (fast_expt b (n / 2)) 
    | _ -> b * fast_expt b (n - 1)



 (*A.3.2*)



(*Instead of making the last recursive call, store the product value as a*)

 let ifast_expt b n = 
    let square m = m * m in
    let is_even m = m mod 2 = 0 in
    let rec iter a b n = 
        match n with
        | 0 -> a
        | _ when is_even n -> iter a (square b) (n / 2)
        | _ -> iter (a * b) b (n - 1) 
    in

    iter 1 b n



(*A.4*)

let rec  fast_mult a b =
    let double n = n + n in
    let halve n = n / 2 in 
    match b with
    | 0 -> 0
    | _ when b mod 2 = 0 -> double (fast_mult a (halve b)) 
    | _ -> a + fast_mult a (b - 1)


(*A.5*)


 let ifast_mult b n = 
    let is_even m = m mod 2 = 0 in
    let double n = n + n in
    let halve n = n / 2 in
    
    let rec iter a b n = 
        match n with
        | 0 -> a
        | _ when is_even n -> iter a (double b) (halve n)
        | _ -> iter (a + b) b (n - 1) 
    in

    iter 0 b n

(*A.6*)

(*Worst case space complexity is O(log_2(N) because each time you are dividing n by 2
 until n <= 1, then you go back up the spending iterations stack.
 For time complexity, there are log_2(N) levels of the recursion tree, and each level has
 2^L computations, so 2^log_2(N) = O(N) time complexity.*)


(*A.3.7.1*)
(*This function is linear recursive. Each time last_two is called, iot calls itself and
 recursively updates the tuple. It doesn't brounch out into a tree, and it doesn't keep
track of state variables like a lienar iterative function.*)


(*A.3.7.1*)
(* The space complexity is O(N) because you decrement 1 each time until reach n <1. 
   The time complexity is O(N) too for the same reason.*)


(*B.1.a*)
(* (fun x y -> x * (2 + y)) 20 (2 * 4)*)


(*B.1.b*)
(* (fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0 *)

(*B.1.c*)
(* (fun x -> (fun y -> (fun z -> x * y * z) 3) 2 ) 1 *)

(*B.1.d*)
(* (fun x -> x * x * x ) 3*)


(*B.2*)

(*
Substitution model

desugar expression:

let x = 2 * 10 and y = 3 + 4 in let y = 14 in (fun z-> x * y * z) 22
let x = 2 * 10 and y = 3 + 4 in fun y -> (fun z -> x * y * z) 22) 14

(fun x y -> ( fun y -> ( fun z -> x * y * z) 22) 14) (2 * 10)  
eliminate outer y = 3 + 4 because of sheilding : 
(fun x -> ( fun y -> ( fun z -> x * y * z) 22) 14)  (2 * 10) = ...
evaluate (fun z -> x * y * z) 22
	22 -> 22
	substitute 22 in for z -> fun z -> x * y * 22
	(fun z -> x * y * z) bound to fun z -> x * y * 22
	
substitute x * y * 22 for (fun z -> x * y * z) 22 in ...
evaluate (fun x -> ( fun y -> x * y * 22 ) 14) (2 * 10)
evaluate (fun y -> x * y * 22) 14
	14 -> 14
	subsitute 14 for y in (fun y -> x * y * 22) -> x * 14 * 22
	(fun y -> x * y * 22) bound to ( x * 14 * 22) in ...
	
evaluate ( fun x -> (x * 14 * 22)) (2 * 10)
evaluate 2 * 10
	2 -> 2
	10 -> 10
	* -> *
2 * 10 -> 20

evaluate ( fun x -> (x * 14 * 22)) 20
substitute 20 for x in ( fun x -> (x * 14 * 22))
evaluate 20 * 14 *22
	skip obvious steps --> 6160
 

	
		
		
		
		
		
	
	
	

*)





(*B.3*)
(* In his code, when he refers to x in "and y = x * 2", 
 he's not referring to rthe x from the first line because it's "and".
 The same goes for the z and the y. 
 (fun x y z -> z + y + z) 10 (x * 2) (y + 3)
 He needs to use in so x and y carry to the next expressions:

 let x = 10 
  in y = x * 2 
  in z = y + 3 
  in x + y + z
     *)


(*c.1*)

  let isum term a next b =
    let rec iter a result =
      if a >/ b
         then result
         else iter (next a) (result +/ term a)
    in 
      iter a (ni 0)


(*C.2.a*)

let rec product_rec term a next b =
	if a >/ b then (ni 1)
	else (term a) */ product_rec term (next a) next b


let rec factorial_rec n = 
	let term x = x in
	let next x = x +/ (ni 1)
	in
	if n =/ (ni 0) then ni 1
	else
	product_rec term (ni 1) next n




(*C.2.b*)

let product_iter term a next b =
    let rec iter a result =
      if a >/ b
         then result
         else iter (next a) (result */ term a)
    in 
      iter a (ni 1)


let rec factorial_iter n = 
	let term x = x in
	let next x = x +/ ni 1
	in
	if n =/ (ni 0) then (ni 1)
	else
	product_iter term (ni 1) next n
	

let pi_product n = 
	let term a = ( a -/ (ni 1)) */ (a +/ (ni 1)) // (a */ a) in
	let next a = a +/ (ni 2) in
	(ni 4)  */ (product_rec term (ni 3) next n)
	
let pi_approx = float_of_num (pi_product (ni 1000))
	

(*C.3.a*)

let rec accumulate_rec combiner null_value term a next b = 
	if a >/ b then null_value
	else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)


(*C.3.b*)


let accumulate_iter combiner null_value term a next b = 
	let rec iter a result = 
	if a >/ b then result
	else iter (next a) (combiner (term a) result)
	
	in
	
	iter a null_value

let sum term a next b = 
	accumulate_rec ( +/ ) (ni 0) term a next b
	
let product term a next b = 
	accumulate_rec ( */ ) (ni 1) term a next b
(*C.4*)


let compose f g n =
 
 f (g n)
 
(*C.5*)


let rec repeated f n =

	if n = 0 then fun x -> x
	else compose f (repeated f (n-1))
	
(*C.6*)

let smooth dx f n =
 (f (n +. dx) +. f n +. f (n -. dx)) /. 3
 
let nsmoothed dx f n =
	(repeated (smooth dx) n) f


(*D.1*)

let is_prime n =
    
    let rec is_factor a b = 
        if float_of_int a <= sqrt (float_of_int b) then
            if b mod a = 0 then false
            else is_factor (a + 1) b
        else true
    in
    if n < 2 then false
    else is_factor 2 n 


(*D.2*)

(*yeet*)

let smallest_prime_factor n =
    
    let rec is_prime_factor a b = 
        if float_of_int a <= sqrt (float_of_int b) then
            if b mod a = 0 && is_prime a then a
            else is_prime_factor (a + 1) b
        else invalid_arg "no prime factors"

    in
    if n <= 2 || is_prime n then invalid_arg "prime argument"
    
    else is_prime_factor 2 n 








