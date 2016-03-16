(*
  Cours "Typage et Analyse Statique"
  Université Pierre et Marie Curie
	Author: Manyanda Chitimbo �2016
  Original author: Antoine Miné 2015
*)

(* 
   The constant domain
 *)

open Abstract_syntax_tree
open Value_domain
  
module Parity = (struct

  
  (* types *)
  (* ***** *)
	
  (* type of abstract values *)
  type t =
		| PAIR 
		| IMPAIR  (* the set is a single value (constant) *)
    | BOT         (* the set is empty (not reachable) *)
    | TOP         (* the set of all integers (not constant)  *)


  (* interface implementation *)
  (* ************************ *)


  (* unrestricted value *)
  let top = TOP

  (* bottom value *)
  let bottom = BOT

  (* constant *)
  let const c = if Z.is_even c then PAIR else IMPAIR

  (* interval *)
  let rand x y =
    if x=y then const x 
    else if x<y then TOP
    else BOT

	(* constant 
  let value c is_first = match c with
	| PAIR -> Z.zero
	| IMPAIR -> Z.one *)

  (* arithmetic operations *)

  let neg a = a

  let add a b = match a,b with
	| PAIR,PAIR | IMPAIR,IMPAIR -> PAIR
	| TOP,_ | _,TOP -> TOP
	| BOT,x | x,BOT -> x
	| _ -> IMPAIR

  let sub = add

  let mul a b = 
		match a,b with 
		| PAIR,PAIR | PAIR, IMPAIR -> PAIR
		| IMPAIR,IMPAIR -> IMPAIR
		| _ -> TOP
		
  let modulo  = sub  
	
  let div a b = TOP

  (* set-theoretic operations *)
  
  let join a b = match a,b with
  | BOT,x | x,BOT -> x
  | TOP,x | x,TOP -> TOP
	| PAIR, IMPAIR -> TOP
  | _ -> a

  let meet a b = match a,b with
  | BOT,x | x,BOT -> BOT
	| PAIR, IMPAIR -> BOT
  | TOP,x | x,TOP -> x
  | _ -> a

  (* no need for a widening as the domain has finite height; we use the join *)
  let widen = join
	
	(* no need for a widening as the domain has finite height; we use the join *)
  let narrow = join


 (* comparison operations (filters) *)
 (* TODO *)
  let eq a b = if a = b then a,b else BOT,BOT

  let neq a b = if a != b then a,b else BOT,BOT
      
  let geq a b = a,b
      
  let gt a b = a,b


  (* subset inclusion of concretizations *)
  let subset a b = match a,b with
  | BOT,_ | _,TOP -> true
  | _ -> false

  (* check the emptyness of the concretization *)
  let is_bottom a = a=BOT

  (* prints abstract element *)
  let print fmt x = match x with
  | BOT -> Format.fprintf fmt "Bottom "
  | TOP -> Format.fprintf fmt "Top "
  | PAIR -> Format.fprintf fmt "Pair "
	| IMPAIR -> Format.fprintf fmt "Impair "


  (* operator dispatch *)
        
  let unary x op = match op with
  | AST_UNARY_PLUS  -> x
  | AST_UNARY_MINUS -> neg x

  let binary x y op = match op with
  | AST_PLUS     -> add x y
  | AST_MINUS    -> sub x y
  | AST_MULTIPLY -> mul x y
  | AST_DIVIDE   -> div x y
	| AST_MODULO   -> modulo x y

  let compare x y op = match op with
  | AST_EQUAL         -> eq x y
  | AST_NOT_EQUAL     -> neq x y
  | AST_GREATER_EQUAL -> geq x y
  | AST_GREATER       -> gt x y
  | AST_LESS_EQUAL    -> let y',x' = geq y x in x',y'
  | AST_LESS          -> let y',x' = gt y x in x',y'
        


  let bwd_unary x op r = match op with
  | AST_UNARY_PLUS  -> meet x r
  | AST_UNARY_MINUS -> meet x (neg r)

        
  let bwd_binary x y op r = match op with

  | AST_PLUS ->
      (* r=x+y => x=r-y and y=r-x *)      
      meet x (sub r y), meet y (sub r x)

  | AST_MINUS ->
      (* r=x-y => x=y+r and y=x-r *)
      meet x (add y r), meet y (sub y r)     
  | AST_MULTIPLY 
  | AST_DIVIDE 
	| AST_MODULO ->
      x,y
					      
end : VALUE_DOMAIN)

    
