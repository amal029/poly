open Language
open Language
open StreamGraph

(*

  IMP: This file parses the AST constructed after decompiling the
  intermediate CFG Graph !!

  Purpose: This file compiles the Pico programs into stream graphs Next,
  the stream graphs are projected onto the CFG and LLVM backends for
  profiling. Finally, the graph partitoning algorithm is used to
  partition the streams graphs onto heterogeneous CPU/GPU mixtures.
  
  Once that is done, we then combine it with the full program polyhedral
  data-structures and generate partitioned code for LLVM backend.

  Author: Avinash Malik

  Sun Aug 12 14:55:37 IST 2012

*)

exception Error of string
exception Internal_compiler_error of string

(* Currently the simple expressions in a loop are considered constant *)
let get_new_num_instr curr = function
  | ColonExpr (x,y,z,lc) -> (match (x,y,z) with | (Const(_,x,_),Const(_,y,_),Const(_,z,lc)) -> curr*((int_of_string x + int_of_string y + 1)/(int_of_string z))
      | _ -> raise (Error ((Reporting.get_line_and_column lc) ^ " currently loop expressions need to be resolved to a constatn only!!")))
  | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " loop expression not a colon-expression"))

let rec replace_empty_actor child = function
  | Seq (x,y,z,c) -> Seq (x,y,z, replace_empty_actor child c)
  | TaskSplit (x,y,z,c) -> TaskSplit(x,y,z,(List.map (fun x -> replace_empty_actor c)))
  | TaskJoin (x,y,z,c) -> TaskJoin(x,y,z,(List.map (fun x -> replace_empty_actor c)))
  | EmptyActor -> child

let process_filter filters num_instr num_vec = function
  | Filter (x,y,z,stmt) -> process_list filters num_instr num_vec [stmt]

let rec process_stmt list filters num_instr num_vec = function
  | Assign (_,x,lc) as s ->
    (* Here we need to check if a call is being made to a different filter!! *)
    let child = process_list filter num_instr num_vec list in
    (match x with
      | FCall (x,_) -> 
	(* If this is the case then we need to go into the filter and make its node *)
	(try process_filter filters num_instr num_vec (List.find (fun x -> (match x with | Call (x,_,_,_) -> true)) filters)
	 with Not_found -> raise (Error ((Reporting.get_line_and_column lc) ^ " filter unbound")))
      | _ -> Seq (s,num_instr,num_vec,child))
  | VarDecl _ | Escape _ | Noop as s -> 
    let child = process_list filters num_instr num_vec list in
    Seq (s,num_instr,num_vec,child)
  | CaseDef (x,lc) -> 
  (* Build a task parallel actor, because every branch of if-else is
     separate of each other*)
    let child = process_list filters num_instr num_vec list in
    let stmts = (match x with | Case (x,y,_) -> (List.map (fun x -> match x with Clause (_,x,_) -> x) x) @ [(match y with | Otherwise x -> x)]) in
    let mes = List.map (fun x -> process_stmt [] filters num_instr num_vec x) stmts in
    let myjoin = ref TaskJoin (s,num_instr,num_vec,child) in
    let mes = List.map (fun x -> replace_empty_actor myjoin x) mes in
    TaskSplit (s,num_instr,num_vec,mes)
  | Block (x,lc) -> 
    (* Every block needs to be done separately *)
    let child = process_list filters num_instr num_vec list in
    let me = process_list filters num_instr num_vec x in
    replace_empty_actor child me
  | For (x,y,stmt) -> 
    (* A for loop only increments num_instr *)
    (* FIXME: for now we consider the loop bounds to be constants, but
       this can be relaxed with some effort in the compiler*)
    let child = process_list filters num_instr num_vec list in
    let ni = get_new_num_instr num_instr y in
    let me = process_stmt [] filters ni num_vec stmt in
    (* Replace the end of me *)
    replace_empty_actor child me 
  | Par (x,expr,stmt) -> 
    (* Here we do the exact same as the for stmt, but increment the num_vec instead *)
    let child = process_list filters num_instr num_vec list in
    let nv = get_new_num_instr num_instr y in
    let me = process_stmt [] filters num_instr nv stmt in
    (* Replace the end of me *)
    replace_empty_actor child me
  | Split (x,lc) as s -> 
    let child = process_list filters num_instr num_vec list in
    let stmts = (match x with | Block x -> x | _ -> raise (Internal_compiler_error ((Reporting.get_line_and_column lc) ^ " Split is not of Block type!!"))) in
    let mes = List.map (fun x -> process_stmt [] filters num_instr num_vec x) stmts in
    let myjoin = ref TaskJoin (s,num_instr,num_vec,child) in
    let mes = List.map (fun x -> replace_empty_actor myjoin x) mes in
    TaskSplit (s,num_instr,num_vec,mes)

and process_list filters num_instr num_vec = function
  | h::t -> process_stmt filters t num_instr num_vec h
  | [] -> EmptyActor

let rec process_main filters = function
  | DefMain (x,y,lc) -> process_filter filters 1 1 x

and process_filters filters = function
  | Def (x,y,lc) -> process_filter filters x


let build_stream_graph = function
  | Program x -> 
    (* First collect all the filters defined in the program *)
    let filters = List.find_all (fun x -> (match x with Def _ -> true | _ -> false)) x in
    (* Now process the stream graph starting from the main filter, just like FCFG is built *)
    try 
      let main = List.find (fun x -> (match x with DefMain _ -> true | _ -> false)) x in
    with Not_found -> raise (Error "No main function defined")
    (* Now start processing from the main filter *)
    let topactor = process_main filters main
