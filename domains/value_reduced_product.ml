open Abstract_syntax_tree
open Value_reduction
open Value_domain

module ReducedProduct(R : VALUE_REDUCTION) = (struct
  
	module A = R.A
	module B = R.B

	type t = R.t 

	let top = R.reduce (A.top,B.top)
	
	let bottom = R.reduce (A.bottom,B.bottom)
	
	let const c = R.reduce (A.const c,B.const c)

	let rand x y = R.reduce (A.rand x y, B.rand x y)

  let join (a,b) (c,d) = R.reduce (A.join a c, B.join b d)
	
  let meet (a,b) (c,d) = R.reduce (A.meet a c, B.meet b d)
	
	let narrow (a,b) (c,d) = R.reduce (A.narrow a c, B.narrow b d)
	  		    
  let subset (a,b) (c,d) = let first = A.subset a c and second = B.subset b d in first && second
        
  let is_bottom a = a = bottom
        
  let print fmt (a,b) = A.print fmt a; B.print fmt b
  
  let unary (a,b) op = R.reduce (A.unary a op,B.unary b op)
	
	let binary (a,b) (c,d) op = R.reduce (A.binary a c op, B.binary b d op)		     
		
  let widen (a,b) (c,d) = R.reduce (A.widen a c, B.widen b d)
	
	let compare (a,b) (c,d) op = 
		let (r1,r2) = A.compare a c op and (r3,r4) = B.compare b d op  
		in (R.reduce (r1,r3)), (R.reduce (r2,r4))
	
	let bwd_unary (x,x1) op (r,r1) = R.reduce (A.bwd_unary x op r, B.bwd_unary x1 op r1)

  let bwd_binary (x,x1) (y,y1) op (r,r1) = 
		let (a,b) = A.bwd_binary x y op r 
		and (c,d) = B.bwd_binary x1 y1 op r1 in  
		R.reduce (a,c), R.reduce(b,d)
		
end : VALUE_DOMAIN)
