let usage_msg = "Usage: polyc <filename>" in
try
  let file_name = ref "" in
  let () = Arg.parse [] (fun x -> file_name := x) usage_msg in
  let in_chan = open_in !file_name in
  let () = print_endline "Step 1....Lexing and parsing..." in
  let lexbuf = Lexing.from_channel in_chan in
  let ast = Parser.ast Lexer.lexer lexbuf in 
  (* The first type inference: simple ML type inference engine*)
  let () = print_endline "Step 2...ML type inference....." in
  let ast = Type_inference.Simple.infer_ast ast in
  let () = print_endline "Step 3....Building the call graph..." in
  let fcfg = Fcfg.check_ast ast in
  let () = print_endline "Step 4.....Building CFG..." in
  let cfg = Cfg.check_fcfg fcfg in
  (* Debugging the generated cfg *)
  (* let () = Dot.build_program_dot cfg "output/output.dot" in *)
  let () = print_endline "Step 5...Performing constant propogation..." in
  let tbl = Constantfolding.Constantpropogation.propogate Language.CFG.Null cfg in
  let () = Dot.build_program_dot cfg "output/output.dot" in
  let () = print_endline "Step 6....Performing constant folding..." in
  let cfg1 = Constantfolding.Constantfolding.fold tbl cfg in
  let () = Dot.build_program_dot cfg1 "output1/output1.dot" in
  let () = print_endline "Step 7....First order dependent type inference..." in
  let _ = Type_inference.First_order.infer_filternode cfg1 in
  let () = close_in in_chan in ()
with
  | End_of_file -> exit 0
  | _ as s -> raise s
