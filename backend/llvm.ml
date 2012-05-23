open Language
open Language
open CFG
open Llvm


let contxext = global_context ()
let the_module = create_module contex "my jit"
let builder = builder context



let rec llvm_filter_node = function
  | Filternode (topnode, ll) -> List.iter llvm_filter_node ll; llvm_topnode
