open Batteries
open Buffer
open Language
open NStreamGraph

exception Internal_compiler_error of string

let num_nodes = ref 0
let num_edges = ref 0
let format = "011"
let constraints = "1"

(* The buffer that holds the metis connections *)
let buffer = create 80

(* BFS node list *)
let ll = ref []

(* The parent child associative map *)
let cp = Hashtbl.create 50

let get_edge_child Edge (_,x) = x
let get_edge_weight Edge (w,_) = match w with Some x -> x | None -> 1

let update_num_edge Edge(_,x) = 
  match x with 
    | EmptyActor -> ()
    | _ -> num_edges := !num_edges + 1

let get_node_num node = 
  match index_of node !ll 
    | Some x -> (x+1) 
    | None -> raise (Internal_compiler_error "Cannot associate a node num !!")

let add_to_list = function
  | EmptyActor -> ()
  | _ -> 
    if not (List.exists (fun x -> match x with | Node -> true | _ -> false) !ll) ll := !ll @ [node] else ()

let print_edge_connections i Edge(w,x) = 
  match x with
    | EmptyActor -> ()
    | _ -> add_string (buffer, ((string_of_int i) ^ " " ^ (string_of_int w) ^ " "))

let add_to_hash parent = function
  | Edge (_,x) as s -> (match x with | EmptyActor -> () | _ -> Hashtbl.add cp x (parent, s))

let build_metis_file = function
  | TaskSplit (_,w1,w2,edge_list) as s -> 
    (* First increment the num_nodes *)
    num_nodes := !num_nodes + 1;
    (* Next increment the number of edges *)
    let () = List.iter update_num_edge edge_list in
    (* Next append your own weights *)
    let w2 = if w2 = 0 then 1 else w2 in
    let () = add_string (buffer, ((w1*w2) ^ " ")) in
    (* Add your children in order*)
    let () = List.iter add_to_list (List.map get_edge_child edge_list) in
    (* Add yourself to hashtbl *)
    let () = List.iter add_to_hash s edge_list in
    (* Next add the edges and their weights from this thing *)
    let () = List.iter (fun x -> print_edge_connections (get_node_num (match x with | Edge (_,x) -> x)) x) edge_list in 
    (* Make the edges for your own parents *)
    (try
       let (parent,edge) = Hashtbl.find cp s in
       (* Get parents node_num *)
       (match parent with
	 | EmptyActor -> add_string buffer "\n"
	 | _ -> let pnum = get_node_num parent in print_edge_connections pnum edge; print_string buffer "\n";)
     with
       | Not_found -> raise (Internal_compiler_error " I don't have a parent!!"))

  | Store (x, edge_list) as s -> 
    (* First increment the num_nodes *)
    num_nodes := !num_nodes + 1;
    (* Next increment the number of edges *)
    let () = List.iter update_num_edge edge_list in
    (* Next append your own weights *)
    let () = add_string (buffer, ("0 ")) in
    (* Add your children in order*)
    let () = List.iter add_to_list (List.map get_edge_child edge_list) in
    (* Add yourself to hashtbl *)
    let () = List.iter add_to_hash s edge_list in
    (* Next add the edges and their weights from this thing *)
    let () = List.iter (fun x -> print_edge_connections (get_node_num (match x with | Edge (_,x) -> x)) x) edge_list in
    (* Make the edges for your own parents *)
    (try
       let (parent,edge) = Hashtbl.find cp s in
       (* Get parents node_num *)
       (match parent with
	 | EmptyActor -> add_string buffer "\n"
	 | _ -> let pnum = get_node_num parent in print_edge_connections pnum edge; add_String buffer "\n")
     with
       | Not_found -> raise (Internal_compiler_error " I don't have a parent!!"))

  | EmptyActor -> ()

  | Seq (_,w1,w2,edge) -> 
    (* First increment the num_nodes *)
    num_nodes := !num_nodes + 1;
    (* Next increment the number of edges *)
    let () = update_num_edge edge in
    (* Next append your own weights *)
    let w2 = if w2 = 0 then 1 else w2 in
    let () = add_string (buffer, ((string_of_int (w1*w2)) ^ " ")) in
    (* Add your children in order*)
    let () = add_to_list (get_edge_child edge) in
    (* Add yourself to hashtbl *)
    let () = add_to_hash s edge in
    (* Next add the edges and their weights from this thing *)
    let () = print_edge_connections (!num_nodes + 1) edge in
    (* Make the edges for your own parents *)
    (try
       let (parent,edge) = Hashtbl.find cp s in
       (* Get parents node_num *)
       (match parent with
	 | EmptyActor -> add_string buffer "\n"
	 | _ -> let pnum = get_node_num parent in print_edge_connections pnum edge; add_string buffer "\n")
     with
       | Not_found -> raise (Internal_compiler_error " I don't have a parent!!"))

  | TaskJoin (_,w1,w2,edge) -> 
    (* First increment the num_nodes *)
    num_nodes := !num_nodes + 1;
    (* Next increment the number of edges *)
    let () = update_num_edge edge in
    (* Next append your own weights *)
    let w2 = if w2 = 0 then 1 else w2 in
    let () = add_string (buffer, ((string_of_int (w1*w2)) ^ " ")) in
    (* Add your children in order*)
    let () = add_to_list (get_edge_child edge) in
    (* Add yourself to hashtbl *)
    let () = add_to_hash s edge in
    (* Next add the edges and their weights from this thing *)
    let () = print_edge_connections (!num_nodes + 1) edge in
    (* Make the edges for your own parents *)
    (try
       let (parent,edge) = Hashtbl.find cp s in
       (* Get parents node_num *)
       (match parent with
	 | EmptyActor -> ()
	 | _ -> let pnum = get_node_num parent in print_edge_connections pnum edge)
     with
       | Not_found -> raise (Internal_compiler_error " I don't have a parent!!"))

let rec process_list = function
  | h::t -> build_metis_file h; process_list t
  | [] -> ()

let process file top_node = 
  (* Add a dummy parent for the top_node *)
  let () = Hashtbl.add cp top_node (EmptyActor, Edge(EmptyActor,EmptyActor)) in
  ll := top_node :: !ll; 
  let () = process_list !ll in 
  (* Now append the top line to a buffer and concate the 2 buffers *)
  let b1 = create 80 in
  let () = add_string b1 ((string_of_int !num_nodes) ^ " " ^ (string_of_int !num_edges) ^ " " ^ format ^ " " ^ constraints) in
  let () = add_buffer b1 buffer in
  let ochan = open_out file in
  let ochan = BatIO.output_channel ochan ~cleanup:true in
  let () = output_buffer ochan b1 in
  ignore (close_out ochan)
