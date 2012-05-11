let usage_msg = "Usage: polyc <filename>" in
try
  let file_name = ref "" in
  let () = Arg.parse [] (fun x -> file_name := x) usage_msg in
  let in_chan = open_in !file_name in
  let lexbuf = Lexing.from_channel in_chan in
  let _ = Parser.ast Lexer.lexer lexbuf in 
  let () = close_in in_chan in ()
with
  | End_of_file -> exit 0
  | _ -> () 
