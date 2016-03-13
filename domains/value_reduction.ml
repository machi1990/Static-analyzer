open Abstract_syntax_tree
open Value_domain

module type VALUE_REDUCTION =
	sig
		module A : VALUE_DOMAIN
		module B : VALUE_DOMAIN 
		type t = A.t * B.t
		val reduce: t -> t
end