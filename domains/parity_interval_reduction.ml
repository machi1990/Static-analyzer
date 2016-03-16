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
open Value_reduction
open Parity_domain
open Interval_domain

module ParityIntervalsReduction = 
( struct
	module A = Parity
	module B = Intervals
	
	type t = A.t * B.t
	
	let reduce ((a,b):t) : t = 
		if b = B.bottom then A.bottom,B.bottom  
		else (
				if b = B.top then A.top,B.top
				else
					a,b
		)
	
	(* TODO check for infinity bounds *)
	(*let reduce (p,i) =
		match i with
		| a,b ->
		let d = Q.to_bigint a and e = Q.to_bigint b in
		(
		let x = if not (A.subset (A.const d) p) then Z.add d Z.one else d
		and	y = if not (A.subset (A.const e) p) then Z.sub e Z.one else e
		in if Z.gt x y then A.bottom,B.bottom
				else 
					(
						if Z.equal x y then (A.const x,B.const x)
						else 
							(p,B.rand x y) 
						)
				) *)
end : VALUE_REDUCTION)

    
