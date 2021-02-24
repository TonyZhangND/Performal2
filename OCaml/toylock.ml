(* Distributed System Primitives*)
type address = int
type message = {
    src:address; 
    dst:address; 
    epoch:int
    }
module AddrMap = Map.Make(struct type t = address let compare : int -> int -> int = compare end)

let is_member e m = 
    try let _ = AddrMap.find e m in
        true
    with Not_found -> false

(* Distributed System Structs *)
type node = {
    id:address; 
    epoch:int; 
    locked:bool; 
    n:int
    }
type network = message AddrMap.t
type cluster = node AddrMap.t
type system = {
    servers:cluster; 
    net:network;
    config:address list (* list of host ids. This info is embedded in servers map, but we
                         * also maintain a list for convenience *)
    }


(* Toylock Specification *)

(* requires: n>0 *)
(* returns: a new system of size n *)
let init_system (n:int) : system =
    let rec init_cluster (n:int) (curr:address) (servers:cluster) (id_list:address list): cluster*address list =
        if curr = 0 then 
            let new_node = {id=curr; epoch=1; locked=true; n=n} in
            ((AddrMap.add curr new_node servers), curr::id_list)
        else 
            let new_node = {id=curr; epoch=0; locked=false; n=n} in
            init_cluster n (curr-1) (AddrMap.add curr new_node servers) (curr::id_list)
    in
    let servs, conf = init_cluster n (n-1) AddrMap.empty [] in
    let netw = AddrMap.empty in
    {servers=servs; net=netw; config=conf}


(* requires: id in ds.cluster *)
(* returns: true iff node id has grant step enabled in ds *)
let grant_enabled (ds:system) (id:address) : bool =
    let n = AddrMap.find id ds.servers in
    n.locked  (* can grant iff n holds the lock *)


(* requires: id in ds.cluster *)
(* returns: new system where node with id takes a grant step from ds *)
let node_grant (ds:system) (id:address) : system =
    let n = AddrMap.find id ds.servers in
    let _ = assert (grant_enabled ds id) in 
    let n' = {id=n.id; epoch=n.epoch; locked=false; n=n.n} in
    let grant_msg = {src=n.id; dst=(n.id+1) mod n.n; epoch=n.epoch+1} in
    let servers' = AddrMap.add id n' ds.servers in
    let net' = AddrMap.add grant_msg.dst grant_msg ds.net in
    {servers=servers'; net=net'; config=ds.config}


(* requires: id in ds.cluster *)
(* returns: true iff node id has accept step enabled in ds *)
let accept_enabled (ds:system) (id:address) : bool =
    let n = AddrMap.find id ds.servers in
    if (is_member id ds.net) then 
        let msg = AddrMap.find id ds.net in
        if msg.epoch > n.epoch then true else false
    else
    false


(* requires: id in ds.cluster *)
(* returns: new system where node with id takes a accept step from ds *)
let node_accept (ds:system) (id:address) : system =
    let n = AddrMap.find id ds.servers in
    let _ = assert (accept_enabled ds id) in 
    let accept_msg = AddrMap.find id ds.net in
    let n' = {id=n.id; epoch=accept_msg.epoch; locked=true; n=n.n} in
    let servers' = AddrMap.add id n' ds.servers in
    {servers=servers'; net=ds.net; config=ds.config}


(* Utilities *)

let to_str_node (n:node) : string =
    Printf.sprintf "%d : {%d, %b}" n.id n.epoch n.locked

let to_str_mgs (msg:message) : string =
    Printf.sprintf "{s=%d, d=%d, ep=%d}" msg.src msg.dst msg.epoch

let to_str_net (net:network) : string =
    let msg_strs = AddrMap.fold (fun k d a -> (to_str_mgs d)::a) net [] in
    String.concat "\n" msg_strs

let to_str_servers (servers:cluster) : string =
    let node_strs = AddrMap.fold (fun k d a -> (to_str_node d)::a) servers [] in
    String.concat "\n" node_strs

let to_str_sys (sys:system) : string =
    let strs = ["Servers:"; to_str_servers sys.servers; "Network:"; to_str_net sys.net] in
    String.concat "\n" strs