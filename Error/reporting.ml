(*

Author: Avinash Malik

Purpose: Error reporting and assoicated things

Date: Sun Jun  3 17:17:17 IST 2012

*)

open Language
open Language

let parse_stmt_line_nums = Hashtbl.create (50)
(* For error reporting *)
let add_stmt_lnum (s:stmt) (num:(int*int)) = 
  let () = print_endline ("Adding lnum :" ^ (match num with (x,y) -> (string_of_int x) ^ " " ^ (string_of_int y) )) in
  Hashtbl.add parse_stmt_line_nums s num
let get_stmt_lnum (s:stmt) : (int*int) = Hashtbl.find parse_stmt_line_nums s
let get_stmt_lnum_length () = Hashtbl.length parse_stmt_line_nums
let init () = ()
