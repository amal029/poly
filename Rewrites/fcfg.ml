open Language
open Language

exception Recursion of string
exception Error of string

let nested_names = Stack.create();;
let tbl = Hashtbl.create (25);;

let get_tfs = function
  | Def (t,r,lc) | DefMain (t,r,lc) -> 
    (match t with Filter (x,_,_,_) -> 
      match x with Symbol (x,lc) -> 
	(try
	   let _ = Hashtbl.find tbl x in
	   raise (Error (("Filter: " ^ x) ^ " multiply defined" ^ Reporting.get_line_and_column lc))
	 with
	   | Not_found -> Hashtbl.add tbl x (t,r)))
  | _ -> ()
let rec get_filters = function
  | Program x -> get_fs x
and get_fs = function
  | h::t -> get_tfs h; get_fs t
  | [] -> ()

let check_for_recursion g = Stack.iter (fun x -> if x = g 
  then raise (Recursion (("Filter " ^ g) ^ (" has a path to itself")))) nested_names

let rec check_expr mystmt = function
  |  FCall (x,extern) ->
    let (name,lc) = (match x with Call(x,_,_) -> match x with Symbol (x,lc) -> (x,lc)) in
    (try
       let (filter,r) = (Hashtbl.find tbl name) in
       let ll = main filter in
       check_for_recursion name;
       [FCFG.Node (mystmt, filter, r, ll)];
     with
       | Not_found -> 
	 if extern then []
	 else (failwith ((Reporting.get_line_and_column lc) ^ ("Filter: " ^ name) ^ (" not found"))))
  | _ -> []

and check_stmt = function
  | Assign (_,y,_) as s -> check_expr s y
  | Block (x,_) -> check_block x
  | For (_,_,x,_) -> check_stmt x
  | Par (_,_,x,_) -> check_stmt x
  | CaseDef (x,_) ->  check_case x
  | _ -> []
and check_case = function
  | Case (x,y,_) -> List.flatten (List.map (fun x -> (match x with Clause (_,x,_) -> check_stmt x)) x) @ (match y with Otherwise (x,_) -> check_stmt x)
and check_block = function
  | h::t -> ((check_stmt h) @ (check_block t))
  | [] -> []

and main = function
  | Filter (n,_,_,stmt) -> (match n with Symbol (n,_) -> Stack.push n nested_names); 
    let ret = check_stmt stmt in
    let _ = Stack.pop nested_names in
    ret (* return the checked list back *)

let toplevelstmt = function
  | DefMain (x,r,_) -> main x
  | _ -> []


let get_main m = function
  | DefMain (x,r,_) -> m := Some (x,r)
  | _ -> ()

let rec get_main_ref m = function
  | h::t -> get_main m h; get_main_ref m t
  | [] -> ()

(* This thing gives the starting node *)
let rec check_ast = function
  | Program x as s -> get_filters s; 
    let m = ref None in
    get_main_ref m x;
    let (mm,r) = match !m with 
      | None -> failwith "Main not defined" 
      | Some (x,r) -> (x,r) in
    let retl = check_program x in
    FCFG.Node (Noop, mm, r, List.rev(retl))
and check_program = function
  | h::t -> ((toplevelstmt h) @ (check_program t));
  | [] -> [] 
