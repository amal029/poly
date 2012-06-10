open Language
open Language

exception Recursion of string
exception Error of string

let nested_names = Stack.create();;
let tbl = Hashtbl.create (25);;

let get_tfs = function
  | Def (t,lc) | DefMain (t,lc) -> 
    (match t with Filter (x,_,_,_) -> 
      match x with Symbol (x,lc) -> 
	(try
	   let _ = Hashtbl.find tbl x in
	   raise (Error (("Filter: " ^ x) ^ " multiply defined" ^ Reporting.get_line_and_column lc))
	 with
	   | Not_found -> Hashtbl.add tbl x t))
  | _ -> ()
let rec get_filters = function
  | Program x -> get_fs x
and get_fs = function
  | h::t -> get_tfs h; get_fs t
  | [] -> ()

let check_for_recursion g = Stack.iter (fun x -> if x = g 
  then raise (Recursion (("Filter " ^ g) ^ (" has a path to itself")))) nested_names

let rec check_expr mystmt = function
  |  FCall x ->
    let (name,lc) = (match x with Call(x,_,_) -> match x with Symbol (x,lc) -> (x,lc)) in
    (try
       let filter = (Hashtbl.find tbl name) in 
       let ll = main filter in
       check_for_recursion name;
       [FCFG.Node (mystmt, filter, ll)];
     with 
       | Not_found -> (failwith ((Reporting.get_line_and_column lc) ^ ("Filter: " ^ name) ^ (" not found"))))
  | _ -> []

and check_stmt = function
  | Assign (_,y,_) as s -> check_expr s y
  | Block (x,_) -> check_block x
  | For (_,_,x,_) -> check_stmt x
  | Par (_,_,x,_) -> check_stmt x
  | CaseDef (x,_) ->  check_case x
  | _ -> []
and check_case = function
  | Case (x,y) -> List.flatten (List.map (fun x -> (match x with Clause (_,x) -> check_stmt x)) x) @ (match y with Otherwise x -> check_stmt x)
and check_block = function
  | h::t -> ((check_stmt h) @ (check_block t))
  | [] -> []

and main = function
  | Filter (n,_,_,stmt) -> (match n with Symbol (n,_) -> Stack.push n nested_names); 
    let ret = check_stmt stmt in
    let _ = Stack.pop nested_names in
    ret (* return the checked list back *)

let toplevelstmt = function
  | DefMain (x,_) -> main x
  | _ -> []


let get_main m = function
  | DefMain (x,_) -> m := Some x
  | _ -> ()

let rec get_main_ref m = function
  | h::t -> get_main m h; get_main_ref m t
  | [] -> ()

(* This thing gives the starting node *)
let rec check_ast = function
  | Program x as s -> get_filters s; 
    let m = ref None in
    get_main_ref m x;
    let mm = match !m with None -> failwith "Main not defined" | Some x -> x in
    let retl = check_program x in
    FCFG.Node (Noop, mm, List.rev(retl))
and check_program = function
  | h::t -> ((toplevelstmt h) @ (check_program t));
  | [] -> [] 
