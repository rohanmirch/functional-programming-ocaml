(*Rohan Mirchandani CS4 Lab4*)

(*A.1.a*)

type mobile = Mobile of branch * branch  (* left and right branches *)
and branch = 
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch (Mobile (l, _)) = l
let right_branch (Mobile (_, r)) = r

let branch_length c = 
  match c with
  | Weight (a,b) -> a
  | Structure (a,b) -> a

let branch_structure b = 
  match b with
  | Weight (_,w) -> `Weight w
  | Structure (_,m) -> `Structure m


(*A.1.b*)
let rec branch_weight1 b = 
  match b with
  | Weight (l,w) -> w
  | Structure (l,m) -> total_weight1 m
and 
total_weight1 = function 
| Mobile (b1,b2) ->  branch_weight1 b1 + branch_weight1 b2


let rec branch_weight2 b = 
  match b with
  | Weight (l,w) -> w
  | Structure (l,m) -> total_weight2 m
and 
total_weight2 m =
 branch_weight2 (left_branch m) + branch_weight2 (right_branch m)



(*A.1.c*)
let rec is_balanced m = 
  let helper b = (branch_length b) * (branch_weight1 b) in
  match (branch_structure (left_branch m), branch_structure (right_branch m)) with
  | (`Weight _, `Weight _) -> helper (left_branch m) = helper (right_branch m)
  | (`Weight _, `Structure s) -> (helper (left_branch m) = helper (right_branch m)) && is_balanced s
  | (`Structure s, `Weight _) -> (helper (left_branch m) = helper (right_branch m)) && is_balanced s
  | (`Structure s1, `Structure s2 ) -> (is_balanced s1) && (is_balanced s2) &&
                                        (helper (left_branch m) = helper (right_branch m))


(*A.1.D*)

type mobile'  = { left: branch'; right: branch' }
  and  branch'  = Branch' of int * contents
  and  contents = Weight' of int | Structure' of mobile'

(*Abstraction layer*)

let make_mobile' b1 b2 = {left = b1; right = b2} 
let make_weight' l i = Branch' (l, Weight' i)
let make_structure' l m = Branch' (l, Structure' m)

(*Using field punning*)
let left_branch' { left =  b} = b
let right_branch' {right = b } = b

let branch_length' (Branch' (a,c)) = a

let branch_structure' (Branch' (a, c)) =
  match c with
  | Weight' i -> `Weight i
  | Structure' m -> `Structure m


let rec branch_weight' (Branch' (a,c)) = 
  match c with
  | Weight' i -> i
  | Structure' m -> total_weight' m

and 
total_weight' {left = b1; right = b2} = branch_weight' b1 + branch_weight' b2


let rec is_balanced' m = 
  let helper b = (branch_length' b) * (branch_weight' b) in
  match (branch_structure' (left_branch' m), branch_structure' (right_branch' m)) with
  | (`Weight _, `Weight _) -> helper (left_branch' m) = helper (right_branch' m)
  | (`Weight _, `Structure s) -> (helper (left_branch' m) = helper (right_branch' m)) && is_balanced' s
  | (`Structure s, `Weight _) -> (helper (left_branch' m) = helper (right_branch' m)) && is_balanced' s
  | (`Structure s1, `Structure s2 ) -> (is_balanced' s1) && (is_balanced' s2) &&
                                        (helper (left_branch' m) = helper (right_branch' m))


(*A.2*)

type tree = Tree of elem list
  and elem = 
    | Num of int
    | Sub of tree



let rec square_tree tr = 
  let rec square_list lst=
    match lst with
    | [] -> []
    | (Sub x) :: t -> Sub (square_tree x) :: square_list t
    | (Num x) :: t -> Num (x * x) :: square_list t
in
  match tr with
  | Tree x -> Tree (square_list x)

let rec square_tree' (Tree tr) = 
  let rec square_list value =
    match value with
    | (Sub x) -> Sub (square_tree' x) 
    | (Num x) -> Num (x * x) 
in
     Tree (List.map square_list tr)

(*A.3*)


let rec tree_map f tr = 
  let rec map_list lst=
    match lst with
    | [] -> []
    | (Sub x) :: t -> Sub (tree_map f x) :: map_list t
    | (Num x) :: t -> Num (f x) :: map_list t
in
  match tr with
  | Tree x -> Tree (map_list x)

  
(*A.4*)
  let rec subsets = function
    | [] -> [[]]
    | h :: t -> let rest = subsets t in
        rest @ (List.map (fun x -> h :: x) rest)

(* Basically, each time the recursive function is called, we are reparating the first value from the
   tail of the list, generating all sublists of the tail, and appendinfg the head to all those sublists.
   This means we have got all the sublists with the head. Now, we call the same recursive function
   on the tail, so we take the next value of the head, and append it to all the remaining sublists.
   This recursive process repeats until we are left with an empty sublist as out tail, in which we return
   [[]].
*)

(*A.5*)


  let rec accumulate op initial sequence = 
    match sequence with
      | [] -> initial
      | h :: t -> op h (accumulate op initial t)
    
  let map p sequence = 
    accumulate (fun x r -> p x :: r ) [] sequence
  
  let append seq1 seq2 = 
    accumulate (fun x r -> x :: r) seq2 seq1
  
  let length sequence = 
    accumulate (fun x r -> 1 + r) 0 sequence



(*A.6*)


  let rec accumulate_n op init seqs =
    match seqs with
      | [] -> failwith "empty list"
      | [] :: _ -> []   (* assume all sequences are empty *)
      | h :: t -> accumulate op init (map List.hd seqs) :: accumulate_n op init (map List.tl seqs)



(*A.7*)



 let rec map2 f x y =
    match (x, y) with
      | ([], []) -> []
      | ([], _) -> failwith "unequal lists"
      | (_, []) -> failwith "unequal lists"
      | (h1::t1, h2::t2) -> (f h1 h2) :: map2 f t1 t2

  let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)
  
  (*Need a function that takes in each row of m given by the map funcrtion*)
  let matrix_times_vector m v = map (fun m_row -> (dot_product v m_row)) m
  
  let transpose mat = accumulate_n (fun x y -> x :: y) [] mat
  
  let matrix_times_matrix m n =
    let cols = transpose n in
       map (fun m_row -> (matrix_times_vector cols m_row)) m



(******* Part B lol *****)



(*B.1*)

let rec filter predicate sequence =
match sequence with
  | [] -> []
  | h :: t when predicate h -> h :: filter predicate t
  | _ :: t -> filter predicate t


let rec quicksort lst cmp =

  match lst with
  | [] -> []
  | h :: t -> (quicksort (filter (fun x -> cmp x h) t) cmp) @ [h]
             @ (quicksort (filter (fun x -> not (cmp x h)) t) cmp)


(* B.2 *)

(* We know that quicksort is a generative recursive function because we recurse on 
subparts of data we generate. In this case, we separate our original list by cmp
and call quicksort on each partition. *)

(* B.3 *)

(* Ben's version produces an infinite recursion loop because it skips the case
   where you pass a list with a single value into the function. This means it will keep merge 
   sorting a list of length 1 and never stop. *)

(* B.4 *)

  let rec insert_in_order new_result a_list cmp = 
    match a_list with 
      | [] -> [new_result]  
      | h :: t when cmp new_result h -> new_result :: a_list
      | h :: t ->  h :: insert_in_order new_result t cmp
  
  let rec insertion_sort a_list cmp =
    match a_list with
      | [] -> []
      | h :: t -> (insert_in_order h  (insertion_sort t cmp) cmp)

(* This is an example of structural recursion because we are sroting the list by recursing
  on natural subparts of the list. *)

(*C.1*)

  type expr =
    | Int of int           (* constant *)
    | Var of string        (* variable *)
    | Add of expr * expr   (* expr1 + expr2 *)
    | Mul of expr * expr   (* expr1 * expr2 *)
    | Pow of expr * int    (* expr^n *)


let rec simplify1 = function
    | Int (a) -> Int (a)
    | Var (a) -> Var (a)
  | Add (Int (a), Int (b)) -> Int (a + b)
  | Mul (Int (a), Int (b)) -> Int (a * b)
    | Pow (Int(a), b) -> Int (int_of_float ((float_of_int a) ** (float_of_int b)))
  | Add (a, Int (0)) -> simplify1 a
  | Add (Int (0), a) -> a
  | Mul (a, Int (0)) -> Int (0)
  | Mul (Int (0), a) -> Int (0)
  | Mul (a, Int (1)) -> a
  | Mul (Int (1), a) -> a
    | Pow (_, 0) -> Int (1) 
  | Pow (a, 1) -> a
  | Add (a, b) -> Add (simplify1 a, simplify1 b)
  | Mul (a, b) -> Mul (simplify1 a, simplify1 b)
  | Pow (a, b) -> Pow (simplify1 a, b)


 let rec simplify expr =
    let e = simplify1 expr in
      if expr = e
        then expr
        else simplify e

(*C.2*)

let rec deriv e x = 
  match e with
  | Int (a) -> Int (0)
  | Var (a) when a <> x -> Int (0)
  | Var (a) when a = x -> Int (1)
  | Add (a, b) -> Add (deriv a x, deriv b x)
  | Mul (a, b) -> Add ( Mul ((deriv a x) , b), Mul ((deriv b x) , a) )
  | Pow (a, b) -> Mul ( Mul (Int(b), Pow (a, b - 1)), deriv a x)


 let derivative expr var =
    let e = simplify expr in
    let d = deriv e var in
      simplify d


