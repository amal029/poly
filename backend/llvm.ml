open Language
open Language
open CFG
open Llvm
open Llvm_scalar_opts

exception Error of string
exception Internal_compiler_error of string

let contxext = global_context ()
let the_module = create_module contex "my jit"
let builder = builder context

(* Build the optimization passes *)
let the_fpm = PassManager.create_function the_module
(* Do simple "peephole" optimizations and bit-twiddling optzn. *)
let () = add_instruction_combination the_fpm
(* reassociate expressions. *)
let () =  add_reassociation the_fpm
(* Eliminate Common SubExpressions. *)
let () = add_gvn the_fpm
(* Simplify the control flow graph (deleting unreachable blocks, etc). *)
let () =   add_cfg_simplification the_fpm
let _ =  PassManager.initialize the_fpm

let fcall_names = Hashtbl.create 10
let func_name_counter = ref 1

let llvm_topnode = function
  | Topnode (fcall,name,cfg_list) -> 
    let func_name = name ^ (string_of_int func_name_counter) in
    (*Increment the func_name counter *)
    let () = func_name_counter := func_name_counter + 1 in
    
  | Null -> raise (Internal_compiler_error "Hit a Null fcall expression while producing llvm IR")

let rec llvm_filter_node = function
  | Filternode (topnode, ll) -> 
    let () = List.iter llvm_filter_node ll in 
    let () = llvm_topnode in
    if Llvm_bitwriter.write_bitcode_file the_module "output.ll" then ()
    else raise (Error ("Could not write the llvm module to output.ll file"))

