(* Rohan Mirchandani CS4 Lab 5 *)

(* A.1 *)

let fibonacci n  = 
	let counter = ref 2 in
	let num1 = ref 1 in 
	let prev = ref 1 in
	let temp = ref 0 in
	
	if n <= 1 then n
	else 
		(while !counter < n do 
			counter := !counter + 1;
			temp := !num1;
			num1 := !prev + !num1;
			prev := !temp
		done;
		
		!num1)


let fibonacci2 n  = 
	let num1 = ref 1 in 
	let prev = ref 1 in
	let temp = ref 0 in
	
	if n <= 1 then n
	else 
		(for i = 2 to n-1 do 
			temp := !num1;
			num1 := !prev + !num1;
			prev := !temp
		done;
		
		!num1)


(* A.2 *)

let bubble_sort a = 
	let len = ref (Array.length a) in

	for i = 0 to !len - 2 do
		(for j = 0 to !len - i - 2 do
			(if a.(j) > a.(j + 1) then
				(
					let temp = ref a.(j) in
					a.(j) <- a.(j+1);
					a.(j+1) <- !temp

				))
			done;)
	done


(* B.a *)

  let meters_per_foot = 0.3048

  
  let get_meters len =
    match len with
      | `Meter m -> m
      | `Foot f -> f *. meters_per_foot
      | `Inch i -> i *. meters_per_foot /. 12.0
      
  let length_add a b = `Meter (get_meters a +. get_meters b)

(* B.b *)

let grams_per_slug = 14593.903203

let get_grams w =
	match w with
	| `Gram g -> g
	| `Kilo k -> k *. 1000.
	| `Slug s -> s *. grams_per_slug

let mass_add a b = `Gram (get_grams a +. get_grams b)

let get_seconds s =
	match s with
	| `Second s -> s
	| `Minute s -> s *. 60.
	| `Hour s -> s *. 60. *. 60.
	| `Day s -> s *. 60. *. 60. *. 24.

let time_add a b = `Second (get_seconds a +. get_seconds b)

(* C.c *)

let unit_add a b =
	match a, b with
	| (`Length x, `Length y) -> `Length (length_add x y)
	| (`Mass x, `Mass y) -> `Mass (mass_add x y )
	| (`Time x, `Time y) -> `Time (time_add x y)
	| _ -> failwith "not compatible dimensions"

(* We dont get into a combinatorial explosion when we add more
   unit types. In the unit_add function, simply need to add one more line
   in the match clasue for each new unit type. The same goes for each of the
   get_x functions. *)


(* C.1 *)

let rec make_gram g =
    let as_slug =  g /.14593.903203 in
    let check_compat other = other#unit_type = `Gram || other#unit_type = `Slug

    in
      object
        method get_grams = g
        method get_slugs = as_slug
        method unit_type = `Gram
        method compatible other = check_compat other
        method add other = 
        	if check_compat other then make_gram (g +. other#get_grams)
        	else 
        		failwith "can't add given type to gram bro"
      end





(* C.2 *)

(* Define a number as a message-passing object. *)
  (* "i" is an int. *)
  let rec make_number i =
    object
      method value = i
      method show = string_of_int i
      method is_zero = i = 0
      method is_number = true
      method evaluate _ _ = make_number i  (* must evaluate to an object *)
      method derive _ = make_number 0  (* derivative of a number is 0 *)
    end
  
  (* Define a variable as a message-passing object. *)
  (* "v" is a string. *)
  let rec make_variable v =
    object
      method value = failwith "variable has no numerical value"
      method show  = v
      method is_zero = false
      method is_number = false
      method evaluate v' n =
        if v = v'
          then make_number n
          else make_variable v
      method derive v' =
        if v = v'
          then make_number 1  (* d/dx(x) = 1 *)
          else make_number 0  (* d/dx(y) = 0 *)
    end
  
  (* Define a sum as a message-passing object. *)
  let rec make_sum expr1 expr2 =
    match () with
      | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
      | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
      | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
            make_number (expr1#value + expr2#value)
      | _ ->  (* create a new object representing the sum *)
            object
              method value = failwith "sum expression has no numerical value"
              method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
              method is_zero = false
              method is_number = false
              method evaluate v n = 
                make_sum (expr1#evaluate v n) (expr2#evaluate v n)
              method derive v = 
                make_sum (expr1#derive v) (expr2#derive v)
            end
  
  (* Evaluate a message-passing expression with a number 
     substituted for a variable. *)
  let evaluate expr v n = expr#evaluate v n
    
  (* Return the string representation of an expression. *)
  let show expr = expr#show
  
  (* Return the derivative of an expression. *)
  let differentiate expr v = expr#derive v


  let rec make_product expr1 expr2 = 
  	match () with
  		| _ when expr1#is_zero || expr2#is_zero -> make_number 0 (* 0*n = 0 *)
  	    | _ when expr1#is_number && expr1#value = 1 -> expr2
  		| _ when expr2#is_number && expr2#value = 1 -> expr1 
  		| _ when expr1#is_number && expr2#is_number ->
  			make_number (expr1#value * expr2#value) (* both values*)

  		| () ->
  			object
  				method value = failwith "product expression has no numerical value"
  				method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
  				method is_zero = false
  				method is_number = false
  				method evaluate v n = 
  					make_product (expr1#evaluate v n) (expr2#evaluate v n)
  				method derive v = 
  					make_sum (make_product (expr1#derive v) (expr2))
  							(make_product (expr1) (expr2#derive v))
  				end




(*C.2.b.1*)

(* val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj> *)

(*C.2.b.2*)

(*  
val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj> *)

(*C.2.b.3*)

(* string =
"(((x * (x * y)) + (x * ((x * y) + (x * y)))) + (3 * ((x * (y * y)) + (x * (y * y)))))" *)

(*C.2.b.4*)

(*  - : string =
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))" *)

(*C.2.b.5*)

(* - : string = "558" *)

(*C.2.b.6*)

(* - : string = "396" *)















