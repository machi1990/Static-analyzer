(*
  Cours "Typage et Analyse Statique"
  UniversitÃ© Pierre et Marie Curie
  Author: Manyanda Chitimbo ©2016
*)


(* 
  Abstract interpreter by induction on the syntax.
  Parameterized by an abstract domain.
*)
open Abstract_syntax_tree
open Abstract_syntax_printer
open Domain
open Interpreter

module Trace_Interprete(D : DOMAIN) =
(struct
 	type t = D.t
		
  let filter (a:t) (e:bool_expr ext) (r:bool) : t =

    (* recursive exploration of the expression *)
    let rec doit a (e,x) r = match e with

    (* boolean part, handled recursively *)
    | AST_bool_unary (AST_NOT, e) -> 
        doit a e (not r)
    | AST_bool_binary (AST_AND, e1, e2) ->
        (if r then D.meet else D.join) (doit a e1 r) (doit a e2 r)
    | AST_bool_binary (AST_OR, e1, e2) -> 
        (if r then D.join else D.meet) (doit a e1 r) (doit a e2 r)
    | AST_bool_const b ->
        if b = r then a else D.bottom ()
          
    (* arithmetic comparison part, handled by D *)
    | AST_compare (cmp, (e1,_), (e2,_)) ->
        (* utility function to negate the comparison, when r=false *)
        let inv = function
        | AST_EQUAL         -> AST_NOT_EQUAL
        | AST_NOT_EQUAL     -> AST_EQUAL
        | AST_LESS          -> AST_GREATER_EQUAL
        | AST_LESS_EQUAL    -> AST_GREATER
        | AST_GREATER       -> AST_LESS_EQUAL
        | AST_GREATER_EQUAL -> AST_LESS
        in
        let cmp = if r then cmp else inv cmp in
        D.compare a e1 cmp e2

    in
    doit a e r
		
	(* TODO Partitioning evaluation*)
	let bottom_key = "BOTTOM";;
	let true_key = "TRUE";;
	let false_key = "FALSE";;


	module PATH  = Mapext.Make(String);;	
	let history = PATH.add bottom_key (D.init()) PATH.empty;;

	let print_element ext key domaine =  
			Format.printf "%s: %a@\n" 
      ((string_of_extent ext)^"  "^key) D.print_all domaine; ()
			
	let print_history ext history = PATH.iter (print_element ext) history; ()
	
	(*Not sound*)		
	let find key history = if (PATH.mem key history) then (PATH.find key history) else D.bottom()
	
	let fold history = let acc = ref ( find bottom_key history) in
										acc :=	(D.join !acc (find true_key history));
										acc :=	(D.join !acc (find false_key history));
										!acc
		
					 	
	let rec eval_stat_paths (history) ((s,ext):stat ext) = 
		let r = match s with    
    | AST_block (decl,inst) ->
        let add x =
          List.fold_left
            (fun a ((_,v),_) -> D.add_var a v)
            x decl
        in let mapped = PATH.map add history in					
        let mapped = List.fold_left eval_stat_paths mapped inst in
				let del x = List.fold_left (fun a ((_,v),_) -> D.del_var a v) x decl 
				in PATH.map del mapped
				
		| AST_assign ((i,_),(e,_)) ->
      let f x = D.assign x i e in
			PATH.map f history	
				
		| AST_assert (e,p) ->
				let f x = let filtered = filter x (e,p) false in 
				if not (D.is_bottom filtered) then (error p "Assertion error.");
				filter x (e,p) true in
				PATH.map f history;
				
    | AST_print l ->
       let print_dom key x = 
					let l' = List.map fst l in
        Format.printf "%s: %a@\n"
          ((string_of_extent ext)^"  "^key) (fun fmt v -> D.print fmt x v) l'; 
					in PATH.iter print_dom history; 
				
        history
          
    | AST_HALT ->
        let f x = D.bottom() in
				PATH.map f history
				
		| AST_if (e,s1,Some s2) ->
				let union = fold history in
        let t = fold (eval_stat_paths (PATH.add bottom_key (filter union e true ) PATH.empty) s1) in
        let f = fold (eval_stat_paths (PATH.add bottom_key (filter union e false) PATH.empty) s2) in
				let b = D.join t f in  
				let mapped = PATH.add true_key t history in
				let mapped = PATH.add false_key f mapped in
				PATH.add bottom_key b mapped;
		      
    | AST_if (e,s1,None) ->
				let union = fold history in
        let t = fold (eval_stat_paths (PATH.add bottom_key (filter union e true ) PATH.empty) s1) in
        let f = (filter union e false) in
				let b = D.join t f in  
				let mapped = PATH.add true_key t history in
				let mapped = PATH.add false_key f mapped in
				PATH.add bottom_key b mapped;
						
	  | AST_while (e,s) -> 
			let trace_subset x m = (D.subset (find bottom_key m) (find bottom_key x)) 
				&& (D.subset (find true_key m) ( find true_key x))
				&& (D.subset (find false_key m) ( find false_key x))	
			in		
		  let rec fix f x = 
        let fx = f x in
        if trace_subset x fx then fx
        else fix f fx
      in
			
			let f h = 
				let doit x key = let unroll = ref !loop_unrolling and delay = ref !widen_delay and
				narrowing = ref !narrowing_value and	a = find key history in	
				if !unroll = 0 then ( if !delay = 0 then 
						let widened = PATH.add key (D.widen a ( fold (eval_stat_paths ( PATH.add key (filter x e true) PATH.empty) s))) PATH.empty in
						if !narrowing = 0 then widened 
						else (
								narrowing := !narrowing - 1;
								let narrowed = D.narrow ( fold (eval_stat_paths(PATH.add key(filter x e true) PATH.empty ) s)) (fold widened) 
								in PATH.add key narrowed PATH.empty
						)
				else (
					delay := !delay - 1;
					let joined = D.join a (fold (eval_stat_paths ( PATH.add key (filter x e true) PATH.empty ) s)) 
					in PATH.add key joined PATH.empty
				)) else ( 
					unroll := !unroll - 1;
					eval_stat_paths (PATH.add key (filter x e true) PATH.empty ) s
					) in 
					let res = PATH.add bottom_key (fold (doit (find bottom_key h) bottom_key)) PATH.empty in
					let res = if PATH.mem true_key h then (PATH.add true_key (fold (doit ( find true_key h) true_key)) res) else res in
					let res = if PATH.mem false_key h then PATH.add false_key (fold (doit (find false_key h) false_key)) res else res in 
					res 
					in let inv = fix f history in 
					let filtered = PATH.add bottom_key (filter (find bottom_key inv) e false) PATH.empty in
					let filtered = if PATH.mem true_key inv then PATH.add true_key (filter (find true_key inv) e false) filtered else filtered in
					if PATH.mem false_key inv then PATH.add false_key (filter (find false_key inv) e false) filtered else filtered
	 in
		
		(* tracing, useful for debugging *)
    if !trace then print_history ext r;
		
		r			
																
  let rec eval_prog (l:prog) : unit = let _ = List.fold_left eval_stat_paths (history) l in ()

end : INTERPRETER)