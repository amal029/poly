let usage_msg = "Usage: polyc <filename>" in
try
  let file_name = ref "" in
  let () = Arg.parse [] (fun x -> file_name := x) usage_msg in
  let in_chan = open_in !file_name in
  let lexbuf = Lexing.from_channel in_chan in
  let ast = Parser.ast Lexer.lexer lexbuf in 
  (* The first type inference: simple ML type inference engine*)
  let ast = Type_inference.Simple.infer_ast ast in
  let fcfg = Fcfg.check_ast ast in
  let _ = Cfg.check_fcfg fcfg in
  let () = close_in in_chan in ()
with
  | End_of_file -> exit 0
  | _ as s -> raise s
