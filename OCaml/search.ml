open Toylock

type run_tree = RNode of system * (run_tree ref list) ref


(* requires: ids is a complete list of hosts in curr_sys *)
(* returns: a list of possible next states of curr_sys *)
let rec gen_child_syslist (curr_sys:system) (ids:address list) (acc:system list) : system list =
    match ids with 
    | [] -> acc
    | h :: t -> 
        let acc = 
            if grant_enabled curr_sys h then (node_grant curr_sys h)::acc else acc
        in
        let acc = 
            if accept_enabled curr_sys h then (node_accept curr_sys h)::acc else acc
        in
        gen_child_syslist curr_sys t acc


(* requires: limit_depth > 0, curr_depth >= 0, limit_depth >= curr_depth *)
(* returns: an execution tree with initial state root and depth up to limit_depth *)
let rec dfs (root: system) (curr_depth:int) (limit_depth:int) : run_tree =
    (* Base case *)
    if curr_depth = limit_depth then RNode(root, ref [])
    else
    (* Recursive case *)
    (* First compute the children list of root *)
    (* Return node with value sys, and children are the [dfs(c)] of each c in children *)
    let child_syslist = gen_child_syslist root root.config []  in
    gen_subtrees root child_syslist curr_depth limit_depth []
    and gen_subtrees (parent:system) (syslist:system list) (curr_depth:int) (limit_depth:int) (acc:run_tree ref list) 
        : run_tree =
        (* Given a list of possible next states as syslist, return a list of run_tree 
         * that represents the list of children/subtrees of root 
         *)
        match syslist with
        | [] -> RNode(parent, ref acc)
        | h :: t -> 
            let subtree = ref (dfs h (curr_depth + 1) limit_depth) in
            gen_subtrees parent t curr_depth limit_depth (subtree::acc)


let bfs (q: run_tree ref Queue.t) (limit_depth:int) : unit =
    let count = ref 0 in 
    while not (Queue.is_empty q) && !count <= limit_depth do
        count := !count + 1;
        let node_ref = Queue.pop q in
        match !node_ref with
        | RNode (sys, chidren_ref) -> 
            let child_syslist = gen_child_syslist sys sys.config []  in
            let child_nodelist = 
                List.fold_left 
                    (fun a x -> 
                        let nr = ref (RNode(x, ref [])) in
                        Queue.add nr q; 
                        nr :: a
                    ) [] child_syslist in
            chidren_ref := child_nodelist;
    done
        

(* requires: n>0, limit_depth>0 *)
(* returns: an execution tree *)
let dfs_search (size:int) (limit_depth:int) : run_tree =
    let init_state = init_system size in
    let tree = dfs init_state 0 limit_depth in
    tree


(* requires: n>0, limit_depth>0 *)
(* returns: an execution tree *)
let bfs_search (size:int) (limit_depth:int) : run_tree =
    let init_state = init_system size in
    let root_node = ref (RNode(init_state, ref [])) in
    let q = Queue.create () in
    Queue.add root_node q;
    Printf.printf "Begin bfs search\n%!";
    let _ = bfs q limit_depth in
    !root_node


(* Main function *)

let rec print_tree (t:run_tree) : unit =
    match t with 
    |RNode(root, children) -> 
        let _ = assert ((List.length !children) <= 1) in
        print_endline (to_str_sys root);
        print_endline "";
        match !children with
        | [] -> ()
        | h :: t ->
            print_tree  !h


let _ = 
    let cond = Array.length Sys.argv = 2 in  
    assert (if not cond then print_endline "No depth argument%!"; cond);
    let d = int_of_string Sys.argv.(1) in
    Printf.printf "Toylock run generator, depth = %d\n\n%!" d;
    
    (* Note: dfs goes into stack overflow at depth 200_000.
     * bfs tested up to 10_000_000. *)
    let tree = bfs_search 3 d in
    let _ = print_tree tree in
    print_endline "Done";
    ()
