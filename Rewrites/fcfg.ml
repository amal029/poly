open Language
open Language

exception Recursion of string

let nested_names = Stack.create();;
let tbl = Hashtbl.create (25);;

let get_tfs = function
  | Def t | DefMain t -> 
    (match t with Filter (x,_,_,_) -> 
      match x with Symbol x -> Hashtbl.add tbl x t)
  | _ -> ()
let rec get_filters = function
  | Program x -> get_fs x
and get_fs = function
  | h::t -> get_tfs h; get_fs t
  | [] -> ()

let check_for_recursion g = Stack.iter (fun x -> if x = g 
  then raise (Recursion (("Filter " ^ g) ^ (" has a path to itself")))) nested_names

let rec check_expr = function
  |  FCall x as s -> 
    let name = (match x with Call(x,_) -> match x with Symbol x -> x) in
    (try 
       let filter = (Hashtbl.find tbl name) in 
       let ll = main filter in
       check_for_recursion name;
       [FCFG.Node (s, filter, ll)];
     with 
       | Not_found -> (failwith (("Filter: " ^ name) ^ (" not found"))))
  | _ -> []

and check_stmt = function
  | Assign (_,y) -> check_expr y
  | Block x -> check_block x
  | For (_,_,x) -> check_stmt x
  | Par (_,_,x) -> check_stmt x
  | _ -> []
and check_block = function
  | h::t -> ((check_stmt h) @ (check_block t))
  | [] -> []

and main = function
  | Filter (n,_,_,stmt) -> (match n with Symbol n -> Stack.push n nested_names); 
    let ret = check_stmt stmt in
    let _ = Stack.pop nested_names in
    ret (* return the checked list back *)

let toplevelstmt = function
  | DefMain x -> main x
  | _ -> []


let get_main m = function
  | DefMain x -> m := Some x
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
    FCFG.Node (Main, mm, retl)
and check_program = function
  | h::t -> ((toplevelstmt h) @ (check_program t));
  | [] -> [] 
