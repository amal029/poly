open Batteries;;

exception Error of string;;

let usage_msg = "Usage: polyc [options] <filename>\nsee -help for more options" in
try
  let compile = ref true in
  let file_name = ref "" in
  let decompile_flag = ref false in
  let version = ref false in
  let dot = ref false in
  let llvm = ref false in
  let graph_part = ref false in
  let graph_tile = ref false in
  let gtn = ref 10 in
  let load_modules = ref [] in
  let vectorize = ref false in
  let vipr = ref false in
  let slots = ref false in
  let output = ref "" in
  let march = ref "x86_64" in
  let () = Arg.parse [("-stg-lang", Arg.Set decompile_flag, " Decompile to stg-lang");
		      ("-O3", Arg.Set vectorize, " Vectorize code");
		      ("-slots", Arg.Set slots, " Use slots instead of named vars in llvm code [default = false]");
		      ("-march", Arg.String (fun x -> march := x), " Set the march for compilation, available, x86_64, shave, x86_64-gnu-linux [default = x86_64, which is apple darwin]");
		      ("-vipr", Arg.Set vipr, " Input VIPR code for parsing and code generation");
		      ("-graph-part", Arg.Set graph_part, 
		       " Produce the stream graph for vectorization and partitoning on heterogeneous architecture using Zoltan" );
		      ("-graph-tile", Arg.Set graph_tile, " Option with -graph-part, used to tile the loops");
		      ("-gtn", Arg.Int (fun x -> gtn := x), " The size of the vector tile [default=<vector-length>/10]");
		      ("-g", Arg.Set dot, "  Produce Dot files in directory output and output1 for debugging");
		      ("-llvm", Arg.Set llvm, " Produce llvm bitcode in file <file>.ll");
		      ("-l", Arg.String (fun x -> load_modules := x::!load_modules), " Load the explicitly full named .bc files (>= llvm-3.2)");
		      ("-o", Arg.Set_string output, " The name of the output file");
		      ("-v", Arg.Set version, "  Get the compiler version")] (fun x -> file_name := x) usage_msg in

  if !version then begin print_endline "Poly compiler version alpha"; compile := false end

  else 

    if (!vipr) then
      let in_chan = open_in !file_name in
      let () = print_endline "....Lexing and parsing VIPR file..." in
      let lexbuf = Lexing.from_channel in_chan in
      let ast = ViprParser.ast ViprLexer.lexer lexbuf in
      (* Close the input channel *)
      let () = close_in in_chan in
      let ast = Vipr2poly.process ast in
      let () = print_endline "....Building the call graph..." in
      let fcfg = Fcfg.check_ast ast in
      let fcfg = 
	if (!vectorize) then
	  let () = print_endline ".....Performing loop interchange and fission...." in
	  LoopInterchange.interchange fcfg 
	else fcfg in
      let () = print_endline ".....Building CFG..." in
      let cfg = VecCFG.check_fcfg !vectorize fcfg in
      let r1 = (Str.regexp "/") in
      let slist = Str.split r1 !file_name in
      let slist = Str.split (Str.regexp "\\.") (List.hd (List.rev slist)) in
      let llvm_file = (List.hd slist) in
      let llvm_file = if not (!output = "") then !output else llvm_file in
      if !dot then
	Dot.build_program_dot cfg "output/output.dot" else ();
      if !llvm then
	let () = print_endline "...Performing constant propogation..." in
	let tbl = Constantfolding.Constantpropogation.propogate !vipr Language.CFG.Null cfg in
	let () = print_endline "....Performing constant folding..." in
	let cfg = Constantfolding.Constantfolding.fold !vipr tbl cfg in
	let () = print_endline ".....Generating LLVM IR..." in
	let () = MyLlvm.compile !march !slots !vipr !load_modules llvm_file cfg in
	(* Make some system calls to complete the process *)
	if Sys.os_type = "Unix" || Sys.os_type = "Cygwin" then
	  (* We can make some sys calls *)
	  let _ = Sys.command ("opt -internalize -loop-unroll -memcpyopt -globalopt -inline -vectorize -bb-vectorize-req-chain-depth=2 -die -globaldce -strip -adce -O3 " 
			       ^llvm_file^ ".bc -o " ^ llvm_file^ ".bc") in	
	  let _ = Sys.command ("llvm-dis " ^ llvm_file ^".bc -o " ^ llvm_file ^".ll") in
	  let _ = Sys.command ("sed -ie 's/@main/@MAIN/' " ^ llvm_file ^".ll") in
	  let _ = Sys.command ("rm -rf *.lle") in ()
	else raise (Error "Currently the compiler is only supported on Unix platforms or Cygwin")
      else ();
    else
      (* Initialize the error reporting structures *)
      let in_chan = open_in !file_name in
      let () = print_endline "....Lexing and parsing..." in
      let lexbuf = Lexing.from_channel in_chan in
      let ast = Parser.ast Lexer.lexer lexbuf in
      (* Close the input channel *)
      let () = close_in in_chan in
      (* The first type inference: simple ML type inference engine*)
      let () = print_endline "...ML type inference....." in
      let ast = Type_inference.Simple.infer_ast ast in
      let () = print_endline "....Building the call graph..." in
      let fcfg = Fcfg.check_ast ast in
      let () = print_endline ".....Building CFG..." in
      let cfg = Cfg.check_fcfg fcfg in
      (* Debugging the generated cfg *)
      if !dot then
	let () = print_endline ".....Printing the cfg dot file in directory output..." in
	let () = Dot.build_program_dot cfg "output/output.dot" in () else ();
      let () = print_endline "...Performing constant propogation..." in
      let tbl = Constantfolding.Constantpropogation.propogate !vipr Language.CFG.Null cfg in
      if !dot then
	(* Clean the output directory *)
	(* let () = FileUtil.rm ["../output";"../output1"] in *)
	(* let () = FileUtil.mkdir "output" ~parent:true in *)
	(* let () = FileUtil.mkdir "output1" ~parent:true in *)
	let () = print_endline ".....Printing the cfg dot file in directory output..." in
	let () = Dot.build_program_dot cfg "output/output.dot" in ()
      else ();
      let () = print_endline "....Performing constant folding..." in
      let cfg1 = Constantfolding.Constantfolding.fold !vipr tbl cfg in
      (* Make the llvm backend code here *)
      let () = print_endline "....First order dependent type inference..." in
      let cfgt = Type_inference.First_order.infer_filternode false cfg1 in
      (* If decompile option is given then just decompile to andrew lang*)
      (* First we need to remove the "/" *)
      let r1 = (Str.regexp "/") in
      let slist = Str.split r1 !file_name in
      (* Now we need to split the last value using "."*)
      let slist = Str.split (Str.regexp "\\.") (List.hd (List.rev slist)) in
      (* The first string should be my target name *)
      let llvm_file = (List.hd slist) in
      let llvm_file = if not (!output = "") then !output else llvm_file in
      let file_name = ((List.hd slist) ^ ".xml") in
      (* By default do not always produce llvm IR *)
      let cfgt =
	if !vectorize then
	  let () = print_endline "....Decompiling to AST......" in
	  let ast = DecompiletoAST.decompile cfgt in
	  let () = print_endline "....Vectorizing......" in
	  let fcfgv = Fcfg.check_ast ast in
	  let fcfgv = LoopInterchange.interchange fcfgv in
	  (* Now call the vectorization function on this *)
	  VecCFG.check_fcfg !vectorize fcfgv
	else 
	  (* let ast = DecompiletoAST.decompile cfgt in *)
	  (* let fcfg = Fcfg.check_ast ast in *)
	  (* Cfg.check_fcfg fcfg in  *)
	  cfgt in
      if !dot then
	let () = Dot.build_program_dot cfgt "output1/output1.dot" in ()
      else ();
      if !llvm then
	let () = print_endline ".....Generating LLVM IR..." in
	let () = MyLlvm.compile !march !slots !vipr !load_modules llvm_file cfgt in
	(* Make some system calls to complete the process *)
	if Sys.os_type = "Unix" || Sys.os_type = "Cygwin" then
	  (* We can make some sys calls *)
	  let _ = Sys.command ("opt -internalize -loop-unroll -memcpyopt -globalopt -inline -vectorize -bb-vectorize-req-chain-depth=2 -die -globaldce -strip -adce -O3 " 
				  ^llvm_file^ ".bc -o " ^ llvm_file^ ".bc") in
	  let _ = Sys.command ("llvm-dis " ^ llvm_file ^".bc -o " ^ llvm_file ^".ll") in
	  let _ = Sys.command ("rm -rf *.lle") in ()
	else raise (Error "Currently the compiler is only supported on Unix platforms or Cygwin")
      else ();
      if !decompile_flag then
	let () = print_endline "....Decompiling to AST......" in
	let ast = DecompiletoAST.decompile cfgt in
	let () = print_endline "....Decompiling to stg-lang......" in
	let andrew_ast = DecompiletoAndrewLang.get_andrew_program ast in
	let () = Andrewxml.ast_xml ("output/"^ (file_name)) andrew_ast in ()
      else ();
      if !graph_part then
	let () = print_endline "......Graph part decompiling to AST....." in
	let ast = DecompiletoAST.decompile cfgt in
	let () = print_endline "....Producing the stream graph......" in
	let (stream_graph,metis_graph,og) = MyStream.build_stream_graph !graph_tile !gtn ast in
	let () = print_endline "....Writing the metis graph file......" in
	(*let () = MetisDriver.generate_metis_file "1" "011" (llvm_file ^ ".grf") metis_graph in*)
	if !output = "" then
	  let () = MetisDriver.generate_metis_file "2" "011" (llvm_file ^ ".our.grf") og in
	  let () = Stream_dot.build_program_dot (llvm_file ^ ".dot") stream_graph in ()
	else 
	  let () = MetisDriver.generate_metis_file "2" "011" (!output) og in
	  let () = Stream_dot.build_program_dot (llvm_file ^ ".dot") stream_graph in ()
      else ()

with
  | End_of_file -> exit 0
  | Sys_error  _ -> print_endline usage_msg
  | _ as s -> raise s
