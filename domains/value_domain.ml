(*
  Cours "Typage et Analyse Statique"
  Université Pierre et Marie Curie
	Manyanda Chitimbo � 2016
	Larbi Youcef Momo � 2016
  Antoine Miné 2015
*)

(* 
  Signature of abstract domains representing sets of mathematical integers.
  For instance: constants or intervals.
 *)

open Abstract_syntax_tree

(*A bound to be used in Parity_interval reduced product *)
type bound = INT of Z.t 
						| POS_INF
						| NEG_INF

(*An interval to bes used in Parity_interval reduced product*)														
type interval_value = Interval_Val of bound*bound
										| TOP_
										| BOT_
										
module type VALUE_DOMAIN =
  sig

    (* type of abstract elements *)
    (* an element of type t abstracts a set of mathematical integers *)
    type t
          
    (* unrestricted set: [-oo,+oo] *)
    val top: t
        
    (* empty set *)
    val bottom: t

    (* constant: {c} *)
    val const: Z.t -> t

    (* interval: [a,b] *)
    val rand: Z.t -> Z.t -> t

    (* set-theoretic operations *)
    val join: t -> t -> t
    val meet: t -> t -> t
    
		(*Narrowing operation*)
		val narrow: t -> t -> t
    
		(*create an interval_value given a value of type t of a certain domain*)		
		val value: t -> interval_value
																									  						    
    (* subset inclusion *)
    val subset: t -> t -> bool
        
    (* emptyness testing *)
    val is_bottom: t -> bool
        
    (* print abstract element *)
    val print: Format.formatter -> t -> unit
        
    (* unary operation *)
    val unary: t -> int_unary_op -> t

    (* binary operation *)
    val binary: t -> t -> int_binary_op -> t
        
    (* widening, for loops *)
    val widen: t -> t -> t

		(*val value : t -> bool -> Z.t*)
		
    (* comparison *)
    (* [compare x y op] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       a safe, but not precise implementation, would be:
       compare x y op = (x,y)
     *)
    val compare: t -> t -> compare_op -> (t * t)



    (* 
       the following, more advanced operations are useful to handle
       complex tests more precisely
     *)

        
    (* backards unary operation *)
    (* [bwd_unary x op r] returns x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x

       it is safe, as first approximation, to implement it as the identity:
       let bwd_unary x _ _ = x
     *)
    val bwd_unary: t -> int_unary_op -> t -> t

  
     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r

       it is safe, as first approximation, to implement it as the identity:
       let bwd_binay x y _ _ = (x,y)
      *)
    val bwd_binary: t -> t -> int_binary_op -> t -> (t * t)
  
end
      

