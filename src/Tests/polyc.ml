open Batteries;;

let usage_msg = "Usage: polyc [options] <filename>\nsee -help for more options" in
try
  let file_name = ref "" in
  let decompile_flag = ref false in
  let version = ref false in
  let dot = ref false in
  let llvm = ref false in
  let graph_part = ref false in
  let load_modules = ref [] in
  let () = Arg.parse [("-stg-lang", Arg.Set decompile_flag, " Decompile to stg-lang");
		      ("-graph-part", Arg.Set graph_part, 
		       " Produce the stream graph for vectorization and partitoning on heterogeneous architecture using Zoltan" );
		      ("-g", Arg.Set dot, "  Produce Dot files in directory output and output1 for debugging");
		      ("-llvm", Arg.Set llvm, " Produce llvm bitcode in file output.ll");
		      ("-l", Arg.String (fun x -> load_modules := x::!load_modules), " Load the explicitly full named .bc files (>= llvm-3.2)");
		      ("-v", Arg.Set version, "  Get the compiler version")] (fun x -> file_name := x) usage_msg in
  if !version then print_endline "Poly compiler version alpha"
  else 
    (* Initialize the error reporting structures *)
    let in_chan = open_in !file_name in
    let () = print_endline "....Lexing and parsing..." in
    let lexbuf = Lexing.from_channel in_chan in
    let ast = Parser.ast Lexer.lexer lexbuf in 
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
    let tbl = Constantfolding.Constantpropogation.propogate Language.CFG.Null cfg in
    if !dot then
      (* Clean the output directory *)
      let () = FileUtil.rm ["../output";"../output1"] in
      let () = FileUtil.mkdir "output" ~parent:true in
      let () = FileUtil.mkdir "output1" ~parent:true in
      let () = print_endline ".....Printing the cfg dot file in directory output..." in
      let () = Dot.build_program_dot cfg "output/output.dot" in ()
    else ();
    let () = print_endline "....Performing constant folding..." in
    let cfg1 = Constantfolding.Constantfolding.fold tbl cfg in
    (* Make the llvm backend code here *)
    if !dot then
      let () = Dot.build_program_dot cfg1 "output1/output1.dot" in ()
    else ();
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
    let file_name = ((List.hd slist) ^ ".xml") in
    (* By default do not always produce llvm IR *)
    if !llvm then
      let () = print_endline ".....Generating LLVM IR..." in
      let () = MyLlvm.compile !load_modules llvm_file cfgt in ()
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
      let (stream_graph,metis_graph) = MyStream.build_stream_graph ast in
      let () = Stream_dot.build_program_dot (llvm_file ^ ".dot") stream_graph in
      let () = MetisDriver.generate_metis_file (llvm_file ^ ".grf") metis_graph in ()
    else ();
    (* Close the input channel *)
    let () = close_in in_chan in ()
with
  | End_of_file -> exit 0
  | Sys_error  _ -> print_endline usage_msg
  | _ as s -> raise s
