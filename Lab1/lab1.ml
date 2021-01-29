(*Rohan Mirchandani CS4 Lab1*)

(*A.1.1*)
(*- : int = 10*)

(*A.1.2 *)
(*- : float = 10. *)

(*A.1.3*)
(*- : int = 12 *)

(*A.1.4*)
(* Error: This expression has type float but an expression was expected of type
  int.  This error occurs because the + operator is used instead of the +. operator. *)

(*A.1.5*)
(* Error: This expression has type int but an expression was expected of type
   float. This error occurs because the +. operator is used instead of the + operator, 
   which is used for assing two ints.*)

(*A.1.6*)
(* Error: This expression has type float but an expression was expected of type
   int. You can't use the + opeator to add a float.*)

(*A.1.7*)
(* Error: This expression has type int but an expression was expected of type
         float.  You can't use the +. opeator to add an int*)

(*A.1.8*)
(*- : float = 7.2 *)

(*A.1.9*)
(* - : int = 5*)

(*A.1.10*)
(*- : int = 7 *)

(*A.1.11*)
(*val a : int = 3 *)

(*A.1.12*)
(*val b : int = 4 *)

(*A.1.13*)
(*- : bool = false *)

(*A.1.14*)
(*- : bool = true *)

(*A.1.15*)
(*- : bool = false. This is different from prev. experession because == checks if they 
 the exact same object in memory, not if they are structurally equal. *)

(*A.1.16*)
(*- : (int * int * int) list = [(1, 2, 3)] *)

(*A.1.17*)
(*- : (int * int * int) list = [(1, 2, 3)]. It isn't using list ";" syntax, but using the tuple
 "," syntax. However, it's in list hard brackets so it becomes a list. *)

(*A.1.18*)
(*- : int = 4 *)

(*A.1.19*)
(*Error: Syntax error. Need "&&" instead of "and". *)

(*A.1.20*)
(*- : int = 6 *)


(*A.1.20*)
(*Error: This expression has type int but an expression was expected of type
         unit
       because it is in the result of a conditional with no else branch.

       Ocaml expects if and else conditions to have the same return type, 
      and an unspceified else brach returns type unit by default, so its an error.*)

(*A.2*)

 let sum_of_squares_of_two_largest a b c = 
     if a >= c && b >= c then a*a + b*b
 	 else if a >= b && c >= b then a*a + c*c
 	 else b*b + c*c


(*A.3*)
(*
This function take in two integers, a and b. It then returns a + |b|. 
If b is positibe, then a + |b| = a + b so it applies the positive operator(+) to and a and b.
If b is negative, then a + |b| = a - b so it applies the positive operator(-) to and a and b.
*)

(*B.1*)
(*Using the applicative-order evaluation, the values of test's arguments are evaluiated and
 substituied into the fucntion. However, (p ()) is itself an infitinely recursive function
 so it will keep evaluating to itself infinitely: p () -> p (), so the interpreter 
 will run (test 0 (p)) forever. With the normal-order evaluation, the arguments are substiruted in
 to test : if (0 = 0 then 0 else p()), which evaluates to 0 because 0 = 0 so it will be 0.*)


(*B.2*)
(*When new_if (is_good_enough guess x) guess (sqrt_iter (improve guess x) x) is called, 
  since new_if is a function, all arguments must be evaluated, so sqrt_iter (improve guess x) x
  is evaluated, which just does the same thing, and sqrt_iter (improve guess x) x is evaluated again,
  which repeats infinitely. Also, the new_if function is never evaluated, so it never knows if the guess is good
  enough.  If she used a normal if-else statement instead, the boolean expression is evaluated first, then it evaluates
  the coreespoinfing true/false branch of the if statement, so if the guess is good enough, 
  guess is returned and there is no infinite loop.*)


 (*B.3*)
(*do later #####*)
(*

add_a 2 5
2 -> 2
5 -> 5
add_a -> fun add_a a b -> if a = 0 then b else inc(add_a (dec a) b)
apply fun add_a a b -> if a = 0 then b else inc(add_a (dec a) b) to 2, 5
substitute 2 for a and 5 for b in if a = 0 then b else inc(add_a (dec a) b)
if 2 = 0 then 5 else inc(add_a (dec 2) 5)
evaluate 2 = 0
  2 -> 2
  0 -> 0
  = -> =
  apply = to 2, 0 -> false
if false then 5 else inc(add_a (dec 2) 5)
	inc (add_a (dec 2) 5)
	(dec 2)
		2 -> 2
		dec -> dec
		apply dec to 2 -> 1
	inc (add_a 1 5)
    (add_a 1 5)
	1 -> 1
	5 -> 5
	add_a -> fun add_a a b -> if a = 0 then b else inc(add_a (dec a) b)
  apply fun add_a a b -> if a = 0 then b else inc(add_a (dec a) b) to 1, 5
  substitute 1 for a and 5 for b in if a = 0 then b else inc(add_a (dec a) b)
	if 1 = 0 then 5 else inc(add_a (dec 1) 5)
  evaluate 1 = 0
    1 -> 1
    0 -> 0
    = -> =
    apply = to 1, 0 -> false
	if false then 5 else inc(add_a (dec 1) 5)
		inc(add_a (dec 1) 5)
		dec 1
			1 -> 1
			dec -> dec
			apply dec to 1 -> 0
		inc  (add _a 0 5)
		(add_ a 0 5)
		0 -> 0
		5 -> 5
		add_a -> fun add_a a b -> if a = 0 then b else inc(add_a (dec a) b)
    apply fun add_a a b -> if a = 0 then b else inc(add_a (dec a) b) to 0, 5
    substitute 0 for a and 5 for b in if a = 0 then b else inc(add_a (dec a) b)
		if 0 = 0 then 5 else inc( add_a (dec 0) 5)
    evaluate 0 = 0
      0 -> 0
      0 -> 0
      = -> =
      apply = to 0, 0 -> true
		if true then 5 else inc( add_a (dec 0) 5)
		5 -> 5
        
    inc 5
    5 -> 5
    inc -> inc
    apply inc to 5 -> 6
    
(So we know add_a 1 5 -> 6)
inc (6)
6 -> 6
inc -> inc 
apply inc to 6 -> 7

So we know add_a -> inc(add_a 1 5) -> 7, thus, our evaluation comes out to 7.




add_b 2 5
2 -> 2
5 -> 5
add_b -> fun add_b a b -> if a = 0 then b else add_b (dec a) (inc b)
apply add_b -> fun add_b a b -> if a = 0 then b else add_b (dec a) (inc b) to 2 ,5
substitute 2 for a and 5 for b in if a = 0 then b else add_b (dec a) (inc b)
if 2 = 0 then 5 else add_b (dec 2) (inc 5)
evaluate 2 = 0
  2 -> 2
  0 -> 0
  = -> =
  apply = to 2, 0 -> false
if false then 5 else add_b (dec 2) (inc 5)
add_b(dec 2) (inc 5)
	dec 2
	2 -> 2
	dec -> dec
	apply dec to 2 -> 1
    
    inc 5
    5 -> 5
    inc -> inc
    apply inc to 5 -> 6

add_b 1 6
1 -> 1
6 -> 6
add_b -> fun add_b a b -> if a = 0 then b else add_b (dec a) (inc b)
apply add_b -> fun add_b a b -> if a = 0 then b else add_b (dec a) (inc b) to 1 ,6
substitute 1 for a and 6 for b in if a = 0 then b else add_b (dec a) (inc b)
if 1 = 0 then 6 else add_b (dec 1) (inc 6)
evaluate 1 = 0
  1 -> 1
  0 -> 0
  = -> =
  apply = to 1, 0 -> false
if false then 6 else add_b (dec 1) (inc 6)
add_b (dec 1)(inc 6)
    dec 1
    1 -> 1
    dec -> dec
    apply dec to 1 -> 0

    inc 6
    6 -> 6
    inc -> inc
    apply inc to 6 -> 7

add_b 0 7
0 -> 0
7 -> 7
add_b -> fun add_b a b -> if a = 0 then b else add_b (dec a) (inc b)
apply add_b -> fun add_b a b -> if a = 0 then b else add_b (dec a) (inc b) to 0 ,7
substitute 0 for a and 7 for b in if a = 0 then b else add_b (dec a) (inc b)
if 0 = 0 then 7 alse add_b (dec 0)(inc 7)
evaluate 0 = 0
  0 -> 0
  0 -> 0
  = -> =
  apply = to 0, 0 -> true
if true then 7 alse add_b (dec 0)(inc 7)
7 -> 7

The final result we get from dec_b 2 5 is 7.

add_a is linear recursive and add_b is linear iterative


*)



(* This function computes the factorial of the input number,
     which for a number n is equal to n * (n-1) * ... * 1. *)
 let rec factorial n =
     if n = 0 then 1 else n * factorial (n - 1)

(*C.1.a*)

let e_term n = 1.0 /. float_of_int (factorial n)

(*C.1.b*)
let rec e_approximation n = 
	if n = 0 then 1.0
    else  e_approximation (n - 1) +. e_term n

(*C.1.c*)

(*e_approximation 20 gives us 2.71828182845904553 and exp 1.0 gives us
  2.71828182845904509.*)

(*c.1.d*)
(*The result is float = infinity. The factorial fucntion for large numbers like 100 returns 0 
because it exceeds the max int value. This makes the e_term 100 = infinity (n/0 = inf), 
so the answer returned is infinity.*)

(*C.2*)

(*subtracts even or odd amount of times until reach 0*)
let rec is_even x = if x = 0 then true else is_odd (x - 1)
    
    and is_odd x = if x = 0 then false else is_even (x - 1)



(*C.3*)

let rec f_rec n = 
	if n < 3 then n
    else f_rec (n - 1) + 2 * (f_rec (n - 2)) + 3 * (f_rec (n - 3))


(*a represents n-3, b represents n-2, and c represents n-1*)
(*count represents the number itself*)
let rec f1 a b c count = 
    if count = 0 then a
    else f1 b c (c + 2 * b + 3 * a) (count - 1)

(*The idea is to build up from 0, 1, 2  and increment each time*)
let f_iter n = f1 0 1 2 n 


(*C.4*)


let rec pascal_coefficient r i = 
	match (r, i) with
	| (r' ,_) when r' < 1 -> failwith "invalid arguments" 
	| (r' , i') when i' > r' || i' < 1 ->  failwith "invalid arguments" 
	| (1, _) -> 1
    | (2, _) -> 1 
    | (r', i') when r' = i' -> 1 
    | (_, 1) -> 1 
	| _ -> pascal_coefficient (r - 1) (i - 1) + pascal_coefficient (r - 1) i

    











