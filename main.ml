(*
  Cours "Typage et Analyse Statique"
  Université Pierre et Marie Curie
  Author: Manyanda Chitimbo �2016
  Original author: Antoine Miné 2015
*)


module ConcreteAnalysis =
  Interpreter.Interprete(Concrete_domain.Concrete)
    
module ConstantAnalysis =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
       (Constant_domain.Constants))
    
module TraceConstantAnalysis =
  Trace_interpreter.Trace_Interprete
    (Non_relational_domain.NonRelational
       (Constant_domain.Constants))

module IntervalAnalysis =
  Interpreter.Interprete
    (Non_relational_domain.NonRelational
       (Interval_domain.Intervals))
			
module TraceIntervalAnalysis =
  Trace_interpreter.Trace_Interprete
    (Non_relational_domain.NonRelational
       (Interval_domain.Intervals))

module ParityAnalysis =
  Interpreter.Interprete
	(Non_relational_domain.NonRelational
    (Parity_domain.Parity))						
																		
module ReducedProductAnalysis =
  Interpreter.Interprete
	(Non_relational_domain.NonRelational
    (Value_reduced_product.ReducedProduct
       (Parity_interval_reduction.ParityIntervalsReduction)))			
			
(* parse and print filename *)
let doit filename =
  let prog = File_parser.parse_file filename in
  Abstract_syntax_printer.print_prog Format.std_formatter prog

    
(* default action: print back the source *)
let eval_prog prog =
  Abstract_syntax_printer.print_prog Format.std_formatter prog


(* entry point *)
let main () =
  let action = ref eval_prog in
  let files = ref [] in
  (* parse arguments *)
  Arg.parse
    (* handle options *)
    ["-trace", Arg.Set Interpreter.trace, "";
     "-concrete", Arg.Unit (fun () -> action := ConcreteAnalysis.eval_prog),"";
     "-constant", Arg.Unit (fun () -> action := ConstantAnalysis.eval_prog),"";
		 "-interval", Arg.Unit (fun () -> action := IntervalAnalysis.eval_prog),"";
		 "-parity", Arg.Unit (fun () -> action := ParityAnalysis.eval_prog),"";
		 "-reduced", Arg.Unit (fun () -> action := ReducedProductAnalysis.eval_prog),"";	 	
		 "-partition-interval", Arg.Unit (fun () -> action := TraceIntervalAnalysis.eval_prog),"";
		 "-partition-constant", Arg.Unit (fun () -> action := TraceConstantAnalysis.eval_prog),"";	 	 
		 "-delay", Arg.Int (fun n -> if n > -1 then Interpreter.widen_delay := n),"";
		 "-unroll", Arg.Int (fun n -> if n > -1 then Interpreter.loop_unrolling := n),"";
		 "-narrow", Arg.Int (fun n -> if n > -1 then Interpreter.narrowing_value := n),"";	 
   ]
    (* handle filenames *)
    (fun filename -> files := (!files)@[filename])
    "";
  List.iter
    (fun filename ->
      let prog = File_parser.parse_file filename in
      !action prog
    )
    !files
    
let _ = main ()
