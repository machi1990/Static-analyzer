(*
  Cours "Typage et Analyse Statique"
  Université Pierre et Marie Curie
  Author: Manyanda Chitimbo �2016
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
	
																			
	let append_map_to map target = if PATH.is_empty target then map else (
			if PATH.is_empty map then target else (
					let apply key m m1 = if PATH.mem key m then (
							if PATH.mem key m1 then (
									let v = find key m and v1 = find key m1 in
									let joined = D.join v v1 in PATH.add key joined m1
								) else (
									PATH.add key (find key m) m1
								)
						) else m1 in
					let res = apply bottom_key map target in 
					let res  = apply true_key map res in 
					apply false_key map res
				)
		)
					 	
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
				&& (D.subset (find false_key m) ( find false_key x))	in 
				
				(*Fixed point calculation *)
				let rec fix f x delay unroll narrowing = 
        let fx = f x delay unroll narrowing in
        if trace_subset x fx then fx
        else (
					let minus_1 x = if x > 0 then x-1 else 0 in  
					let unroll = minus_1 unroll and
							delay = minus_1 delay and
							narrowing = minus_1 narrowing in
					fix f fx delay unroll narrowing
					)
      in
			
			let f h delay unroll narrowing = 
				let doit x key = 
					let evaluated =  (eval_stat_paths ( PATH.add key (filter x e true) PATH.empty) s) in	
    				if unroll = 0 then ( 
							if delay = 0 then 
    						let widened = PATH.map (fun m -> D.widen x m) evaluated in
    						if narrowing = 0 then widened else (
    							PATH.mapi (fun k v -> D.narrow v (find k widened)) evaluated
    						) else (
      					PATH.map (fun m -> D.join x m) evaluated )) 
    					else ( 
      					evaluated
    					) 
					in let res = if PATH.mem bottom_key h then doit (find bottom_key h) bottom_key else PATH.empty in 
					let res = if PATH.mem false_key h then (
						let doneit = doit ( find false_key h) false_key in append_map_to doneit res 
					) else res in let res = if PATH.mem true_key h then let doneit = doit (find true_key h) true_key 
					in append_map_to doneit res else res in res 
					in let inv = fix f history !widen_delay !loop_unrolling !narrowing_value 
					in let filtered = PATH.add bottom_key (filter (find bottom_key inv) e false) PATH.empty in
					let filtered = if PATH.mem true_key inv then PATH.add true_key (filter (find true_key inv) e false) filtered else filtered 
					in if PATH.mem false_key inv then PATH.add false_key (filter (find false_key inv) e false) filtered else filtered
	 	in
		
		(* tracing, useful for debugging *)
		if !trace then print_history ext r;
		
		r			
																
  let rec eval_prog (l:prog) : unit = let _ = List.fold_left eval_stat_paths (history) l in ()

end : INTERPRETER)