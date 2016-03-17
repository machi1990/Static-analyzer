(*
  Cours "Typage et Analyse Statique"
  UniversitÃ© Pierre et Marie Curie
	Authors: Manyanda Chitimbo ©2016
					Larbi Youcef Momo © 2016
  Original author: Antoine MinÃ© 2015
*)

(* 
   The constant domain
 *)

open Abstract_syntax_tree
open Value_reduction
open Parity_domain
open Interval_domain

module ParityIntervalsReduction = 
( struct
	module A = Parity
	module B = Intervals
	
	type t = A.t * B.t
	
	let do_reduce p i =
		match i with
		| a,b ->
		(
		let x = if not (A.subset (A.const a) p) then Z.add a Z.one else a
		and	y = if not (A.subset (A.const b) p) then Z.sub b Z.one else b
		in if Z.gt x y then A.bottom,B.bottom
				else 
					(
						if Z.equal x y then (A.const x,B.const x)
						else 
							(p,B.rand x y) 
						)
				)
	
	open Value_domain
	let reduce ((a,b):t): t = match B.value b with
	| Interval_Val(x,y) -> (
		match x,y with 
		| INT first, INT second -> do_reduce a (first,second)
		| INT first, POS_INF -> a,b
		| NEG_INF, INT second -> a,b
		| NEG_INF, NEG_INF -> a,b
		| POS_INF, POS_INF -> a,b
		| NEG_INF, POS_INF -> a,b
		| POS_INF, _ | _, NEG_INF -> A.bottom,B.bottom
		)
	| TOP_	-> A.top, B.top
	| _ -> A.bottom, B.bottom
		
end : VALUE_REDUCTION)

    
