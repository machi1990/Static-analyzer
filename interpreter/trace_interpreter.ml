(*
  Cours "Typage et Analyse Statique"
  Université Pierre et Marie Curie
  Author: Manyanda Chitimbo �2016
  Original author: Antoine Miné 2015
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
		
	(*TODO trace partitioning evaluation here*)	
	let rec eval_stat (a:t) ((s,ext):stat ext) : t = 
    let r = match s with    

    | AST_block (decl,inst) ->
        let a =
          List.fold_left
            (fun a ((_,v),_) -> D.add_var a v)
            a decl
        in
        let a = List.fold_left eval_stat a inst in
        List.fold_left
          (fun a ((_,v),_) -> D.del_var a v)
          a decl
        
    | AST_assign ((i,_),(e,_)) ->
        D.assign a i e
          
    | AST_if (e,s1,Some s2) ->
				let t = eval_stat (filter a e true ) s1 in
        let f = eval_stat (filter a e false) s2 in
        D.join t f
          
    | AST_if (e,s1,None) ->
        let t = eval_stat (filter a e true ) s1 in
        let f = filter a e false in
        D.join t f
          
    | AST_while (e,s) -> 
			let unroll = ref !loop_unrolling and
					delay = ref !widen_delay and
					narrowing = ref !narrowing_value in
						
        let rec fix (f:t -> t) (x:t) : t = 
          let fx = f x in
          if D.subset fx x then fx
          else fix f fx
        in
				
				let f x = if !unroll = 0 then (
					if !delay = 0 then 
							let widened = D.widen a (eval_stat (filter x e true) s) in
							if !narrowing = 0 then widened 
							else (
									narrowing := !narrowing - 1;
									D.narrow (eval_stat (filter x e true) s) widened 
							)
					else (
						delay := !delay - 1;
						D.join a (eval_stat (filter x e true) s)
					)) else ( 
						unroll := !unroll - 1;
						eval_stat (filter x e true) s
						) in 
         let inv = fix f a in 
					filter inv e false

    | AST_assert (e,p) ->
				let filtered = filter a (e,p) false in 
				if not (D.is_bottom filtered) then (error p "Assertion error.");
				filter a (e,p) true
    | AST_print l ->
        let l' = List.map fst l in
        Format.printf "%s: %a@\n"
          (string_of_extent ext) (fun fmt v -> D.print fmt a v) l';
        a
          
    | AST_HALT ->
        D.bottom ()
          
    in
    
    (* tracing, useful for debugging *)
    if !trace then 
      Format.printf "stat trace: %s: %a@\n" 
        (string_of_extent ext) D.print_all r;
    r


	(* Partitioning evaluation*)
	module PATH  = Mapext.Make(String);;	
	let history = PATH.add "bottom" (D.init()) PATH.empty;;

	let print_element ext key domaine =  
			Format.printf "%s: %a@\n" 
      ((string_of_extent ext)^"  "^key) D.print_all domaine; ()
			
	let print_history ext history = PATH.iter (print_element ext) history; ()
		
		
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
				let find key = if (PATH.mem key history) 
				then (PATH.find key history) else D.bottom() in
				let acc = ref (find "bottom") in 
						acc := (D.join (find "true") !acc);
						acc := (D.join (find "false") !acc);
        let t = eval_stat (filter !acc e true ) s1 in
        let f = eval_stat (filter !acc e false) s2 in
				let b = D.join t f in  
				let mapped = PATH.add "true" t history in
				let mapped = PATH.add "false" f mapped in
				PATH.add "bottom" b mapped;
		      
    | AST_if (e,s1,None) ->
				let find key = if (PATH.mem key history) 
				then (PATH.find key history) else D.bottom() in
				let acc = ref (find "bottom") in 
						acc := (D.join (find "true") !acc);
						acc := (D.join (find "false") !acc);
        let t = eval_stat (filter !acc e true ) s1 in
        let f = (filter !acc e false) in
				let b = D.join t f in  
				let mapped = PATH.add "true" t history in
				let mapped = PATH.add "false" f mapped in
				PATH.add "bottom" b mapped;
						
	  | AST_while (e,s) -> 
		let unroll = ref !loop_unrolling and
				delay = ref !widen_delay and
				narrowing = ref !narrowing_value in 
			let a = PATH.find "bottom" history in	
      let rec fix (f:t -> t) (x:t) : t = 
        let fx = f x in
        if D.subset fx x then fx
        else fix f fx
      in
			
			let f x = if !unroll = 0 then (
				if !delay = 0 then 
						let widened = D.widen a (eval_stat (filter x e true) s) in
						if !narrowing = 0 then widened 
						else (
								narrowing := !narrowing - 1;
								D.narrow (eval_stat (filter x e true) s) widened 
						)
				else (
					delay := !delay - 1;
					D.join a (eval_stat (filter x e true) s)
				)) else ( 
					unroll := !unroll - 1;
					eval_stat (filter x e true) s
					) in 
       let inv = fix f a in 
				filter inv e false; history	
				
		| _ -> history in
		
		(* tracing, useful for debugging *)
    if !trace then print_history ext r;
		
		r			
																
  let rec eval_prog (l:prog) : unit = let _ = List.fold_left eval_stat_paths (history) l in ()

end : INTERPRETER)