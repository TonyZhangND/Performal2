open Toylock
open Printf

type run_tree = RNode of system * run_tree list

let rec dfs (root: run_tree) (curr_depth:int) (limit_depth:int) : run_tree =
    (* Printf.printf "dfs depth %d \n" curr_depth; *)
    let sys = match root with |RNode(sys, c) -> sys in 
    (* Base case *)
    if curr_depth = limit_depth then root 
    else
    (* Recursive case *)
    (* First compute the children list of root *)
    
    let rec gen_child_syslist (curr_sys:system) (ids:address list) (acc:system list) : system list =
        (* Given the current system state, return a list of possible next states *)
        match ids with 
        | [] -> acc
        | h :: t -> 
            let acc = 
                if grant_enabled curr_sys h then (node_grant curr_sys h)::acc else acc
            in
            let acc = 
                if accept_enabled curr_sys h then (node_accept curr_sys h)::acc else acc
            in
            acc
    in
    let rec gen_subtrees (syslist:system list) (acc:run_tree list) : run_tree list =
        (* Given a list of possible next states as syslist, return a list of run_tree 
         * that represents the list of children/subtrees of root 
         *)
        match syslist with
        | [] -> acc 
        | h :: t -> 
            let subtree = dfs (RNode(h, [])) (curr_depth + 1) limit_depth in
            gen_subtrees t (subtree::acc)
    in
    (* Return node with value sys, and children are the [dfs(c)] of each c in children *)
    let child_syslist = gen_child_syslist sys sys.config []  in
    RNode(sys, (gen_subtrees child_syslist []))
        

let search (size:int) (limit_depth:int) : run_tree =
    let init_state = init_system(size) in
    let tree = dfs (RNode(init_state, [])) 0 limit_depth in
    tree


(* Main function *)
let _ = 
    print_endline "Toylock run generator";
    let _ = search 3 6 in
    print_endline "Done";