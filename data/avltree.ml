[@@@warning "-32-26"]
let avltree_insert_sequential_threshold = ref 100
let avltree_search_sequential_threshold = ref 1
let avltree_search_height_threshold = ref 1

module Make (V: Map.OrderedType) = struct
  module Sequential = struct
    type side = Left | Right

    type 'a node = Leaf | Node of {
      key: V.t;
      mutable nval: 'a;
      (* mutable rl: side; *)
      mutable height: int;
      mutable parent: 'a node;
      mutable left: 'a node;
      mutable right: 'a node
    }

    type 'a tree = {
      mutable root: 'a node
    }

    let key n =
      match n with
      | Leaf -> failwith "Key function: n is a leaf"
      | Node n' -> n'.key

    let left n =
      match n with
      | Leaf -> failwith "Left function: n is a leaf"
      | Node n' -> n'.left

    let right n =
      match n with
      | Leaf -> failwith "Right function: n is a leaf"
      | Node n' -> n'.right

    let height n =
      match n with
      | Leaf -> 0
      | Node n' -> n'.height

    let get_balance n =
      match n with
      | Leaf -> 0
      | Node n' -> height n'.left - height n'.right

    let parent n =
      match n with
      | Leaf -> failwith "Parent function: n is a leaf"
      | Node n' -> n'.parent

    let nval n =
      match n with
      | Leaf -> failwith "Value function: n is a leaf"
      | Node n' -> n'.nval

    let set_height n h =
      match n with
      | Leaf -> ()
      | Node n' -> n'.height <- h

    let set_parent n p =
      match n with
      | Leaf -> ()
      | Node n' -> n'.parent <- p

    let set_child n s c =
      match n with
      | Leaf -> ()
      | Node n' ->
        match s with
        | Left -> (set_parent c n; n'.left <- c)
        | Right -> (set_parent c n; n'.right <- c)

    let expose n =
      match n with
      | Leaf -> failwith "Expose function: n is a leaf"
      | Node n' ->
        set_parent n'.left Leaf;
        set_parent n'.right Leaf;
        let l = n'.left in
        let r = n'.right in
        set_child n Left Leaf;
        set_child n Right Leaf;
        set_height n 1;
        (l, n, r)
        
    let merge_three_nodes nl n nr =
      match n with
      | Leaf -> failwith "Merge three nodes function: n is a leaf"
      | Node _ ->
        set_child n Left nl;
        set_child n Right nr;
        set_height n ((max (height nl) (height nr)) + 1)

    let root_node t = t.root

    let num_nodes t =
      let rec aux n =
        match n with
        | Leaf -> 0
        | Node n' -> 1 + aux n'.left + aux n'.right in
      aux t.root

    let flatten t =
      let rec flatten_aux n =
        match n with
        | Leaf -> []
        | Node n' -> (flatten_aux n'.left) @ [(n'.key, n'.nval)] @ (flatten_aux n'.right) in
      flatten_aux t.root

    (* let rec traverse_aux n =
      match n with
      | Leaf -> ()
      | Node n' -> begin
          let side = if n'.parent != Leaf && n == right (n'.parent) then "Right" else "Left" in
          let k = if n'.parent != Leaf then key n'.parent else -1 in
          Printf.printf "(%d, %d, %d, %s, height: %d), " n'.key n'.nval k side n'.height;
          traverse_aux n'.left;
          traverse_aux n'.right
        end

    let traverse t =
      traverse_aux t.root; Printf.printf "\n" *)

    let new_tree () = {root = Leaf}

    let new_node k v = Node {
      key = k;
      nval = v;
      (* rl = Left; *)
      height = 1;
      parent = Leaf;
      left = Leaf;
      right = Leaf
    }

    let rec search_aux k n =
      match n with
      | Leaf -> None
      | Node n' ->
        if k == n'.key then Some n'.nval
        else if k > n'.key then search_aux k n'.right
        else search_aux k n'.left

    let search k t = search_aux k t.root

    let rotate_left x t =
      let y = right x in
      set_child x Right (left y);
      if left y != Leaf then set_parent (left y) x;
      set_parent y (parent x);
      if parent x = Leaf then t.root <- y
      else if x == left @@ parent x then set_child (parent x) Left y
      else set_child (parent x) Right y;
      set_child y Left x;
      set_height x @@ 1 + max (height @@ left x) (height @@ right x);
      set_height y @@ 1 + max (height @@ left y) (height @@ right y)

    let rotate_right x t =
      let y = left x in
      set_child x Left (right y);
      if right y != Leaf then set_parent (right y) x;
      set_parent y (parent x);
      if parent x = Leaf then t.root <- y
      else if x == right @@ parent x then set_child (parent x) Right y
      else set_child (parent x) Left y;
      set_child y Right x;
      set_height x @@ 1 + max (height @@ left x) (height @@ right x);
      set_height y @@ 1 + max (height @@ left y) (height @@ right y)

    let rec insert_aux new_node current_node t =
      if key new_node == key current_node then ()
      else begin
        let () = if key new_node < key current_node then
          if left current_node == Leaf then
            set_child current_node Left new_node
          else insert_aux new_node (left current_node) t
        else
          if right current_node == Leaf then
            set_child current_node Right new_node
          else insert_aux new_node (right current_node) t in

        let () = set_height current_node @@ max (height (left current_node)) (height (right current_node)) + 1 in
        let balance = get_balance current_node in
        if balance > 1 && key new_node < key (left current_node) then
          rotate_right current_node t
        else if balance < -1 && key new_node > key (right current_node) then
          rotate_left current_node t
        else if balance > 1 && key new_node > key (left current_node) then
          (rotate_left (left current_node) t; rotate_right current_node t)
        else if balance < -1 && key new_node < key (right current_node) then
          (rotate_right (right current_node) t; rotate_left current_node t)
      end

    let insert k v t =
      let new_node = new_node k v in
      if t.root == Leaf then t.root <- new_node
      else insert_aux new_node t.root t

    let rec join_right tl k tr =
      let (l, k', c) = expose tl.root in
      if height c <= height tr.root + 1 then begin
        merge_three_nodes c k tr.root;
        let t' = {root = k} in
        if height t'.root <= height l + 1 then
          (merge_three_nodes l k' t'.root; {root = k'})
        else begin
          rotate_right k t';
          merge_three_nodes l k' t'.root;
          let t'' = {root = k'} in
          rotate_left k' t''; t''
        end
      end
      else begin
        let t' = join_right {root = c} k tr in
        merge_three_nodes l k' t'.root;
        let t'' = {root = k'} in
        if height t'.root <= height l + 1 then t''
        else begin
          rotate_left k' t''; t''
        end
      end

    let rec join_left tl n tr =
      let (c, n', r) = expose tr.root in
      if height c <= height tl.root + 1 then begin
        merge_three_nodes tl.root n c;
        let t' = {root = n} in
        if height n <= height r + 1 then
          (merge_three_nodes n n' r; {root = n'})
        else begin
          rotate_left n t';
          merge_three_nodes t'.root n' r;
          let t'' = {root = n'} in
          rotate_right n' t''; t''
        end
      end
      else begin
        let t' = join_left tl n {root = c} in
        merge_three_nodes t'.root n' r;
        let t'' = {root = n'} in
        if height t'.root <= height r + 1 then t''
        else begin
          rotate_right n' t''; t''
        end
      end

    let join tl n tr =
      if height tl.root > height tr.root + 1 then
        join_right tl n tr
      else if height tr.root > height tl.root + 1 then
        join_left tl n tr
      else begin
        merge_three_nodes tl.root n tr.root; {root = n}
      end

    let rec split t k =
      if t.root = Leaf then ({root = Leaf}, Leaf, {root = Leaf})
      else
        let (l, m, r) = expose t.root in
        if k = key m then ({root = l}, m, {root = r})
        else if k < key m then
          let (ll, b, lr) = split {root = l} k in
          (ll, b, join lr m {root = r})
        else
          let (rl, b, rr) = split {root = r} k in
          (join {root = l} m rl, b, rr)

    let rec verify_height_invariant n =
      match n with
      | Leaf -> true
      | Node n' ->
        let height_diff = abs @@ height n'.left - height n'.right in
        height_diff <= 1 && verify_height_invariant n'.left && verify_height_invariant n'.right
  end

  type 'a t = 'a Sequential.tree

  type ('a, 'b) op =
    | Insert : V.t * 'a -> ('a, unit) op
    | Search : V.t -> ('a, 'a option) op

  type 'a wrapped_op = Mk : ('a, 'b) op * ('b -> unit) -> 'a wrapped_op

  let init () = Sequential.new_tree ()

  let rec binary_search arr target left right =
    if left > right then
      match arr.(left) with
      | (key, _) when key >= target -> (left, arr.(left))
      | _ -> raise Not_found (* No element greater than or equal to the target *)
    else
      let mid = (left + right) / 2 in
      match arr.(mid) with
      | (key, value) when key = target -> (mid, (key, value)) (* Found the target element *)
      | (key, _) when key < target -> binary_search arr target (mid + 1) right
      | _ -> binary_search arr target left (mid - 1)

  (* Section searches depending on node *)
  let rec par_search_aux search_threshold tree_threshold pool t ~keys ~range:(rstart, rstop) =
    let h = Sequential.height @@ Sequential.root_node t in
    let n = rstop - rstart in
    if n <= 0 then ()
    else if n < search_threshold || h < tree_threshold then
      for i = rstart to rstop - 1 do
        let (k, kont) = keys.(i) in kont @@ Sequential.search k t
      done
    else 
      match Sequential.root_node t with
      | Sequential.Leaf -> for i = rstart to rstop - 1 do
          let (k, kont) = keys.(i) in kont @@ Sequential.search k t
        done
      | Node rn ->
        try begin
          let (idx, (k, _)) = binary_search keys rn.key rstart rstop in
          let i = ref idx in
          let ck = ref k in
          if k == Sequential.key @@ Sequential.root_node t then 
            while !ck == k && !i < rstop do
              let (k, kont) = keys.(!i) in kont @@ Sequential.search k t;
              i := !i + 1; ck := k
            done;
          let _ = Domainslib.Task.async pool 
            (fun () -> par_search_aux
              search_threshold tree_threshold pool
              {root = Sequential.right @@ Sequential.root_node t} ~keys ~range:(!i, rstop)) in
          let _ = Domainslib.Task.async pool 
            (fun () -> par_search_aux
              search_threshold tree_threshold pool
              {root = Sequential.left @@ Sequential.root_node t} ~keys ~range:(rstart, !i)) in ()
        end with _ -> for i = rstart to rstop - 1 do
          let (_, kont) = keys.(i) in kont None
        done
    (* else if n > threshold then
      let num_par = n / threshold + if n mod threshold > 0 then 1 else 0 in
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(num_par - 1) ~body:(fun i ->
        par_search_aux threshold pool t ~keys ~range:(rstart + i * threshold, min rstop @@ rstart + (i + 1) * threshold)
      );
    else
      for i = rstart to rstop - 1 do
        let (k, kont) = keys.(i) in kont @@ Sequential.search k t
      done *)
  
  (* Split the tree
  let rec par_search_aux threshold pool t ~keys ~range:(rstart, rstop) =
    let n = rstop - rstart in
    if n <= 0 then ()
    else if n > threshold then
      let num_par = n / threshold + if n mod threshold > 0 then 1 else 0 in
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(num_par - 1) ~body:(fun i ->
        par_search_aux threshold pool t ~keys ~range:(rstart + i * threshold, min rstop @@ rstart + (i + 1) * threshold)
      );
    else
      for i = rstart to rstop - 1 do
        let (k, kont) = keys.(i) in kont @@ Sequential.search k t
      done *)

  (* Split the search operations only *)
  (* let rec par_search_aux threshold pool t ~keys ~range:(rstart, rstop) =
    let n = rstop - rstart in
    if n <= 0 then ()
    else if n > threshold then
      let num_par = n / threshold + if n mod threshold > 0 then 1 else 0 in
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(num_par - 1) ~body:(fun i ->
        par_search_aux threshold pool t ~keys ~range:(rstart + i * threshold, min rstop @@ rstart + (i + 1) * threshold)
      );
    else
      for i = rstart to rstop - 1 do
        let (k, kont) = keys.(i) in kont @@ Sequential.search k t
      done *)

  let par_search ?search_threshold ?tree_threshold ~pool (t: 'a t) keys =
    let search_threshold = match search_threshold with Some t -> t | None -> !avltree_search_sequential_threshold in
    let tree_threshold = match tree_threshold with Some t -> t | None -> !avltree_search_height_threshold in
    Sort.sort pool ~compare:(fun (k, _) (k', _) -> V.compare k k') keys;
    par_search_aux search_threshold tree_threshold pool t ~keys ~range:(0, Array.length keys)

  (* let par_search ?threshold ~pool (t: 'a t) keys =
    let threshold = match threshold with Some t -> t | None -> !avltree_search_sequential_threshold in
    Sort.sort pool ~compare:(fun (k, _) (k', _) -> V.compare k k') keys;
    par_search_aux threshold pool t ~keys ~range:(0, Array.length keys) *)

  let rec par_insert_aux threshold ~pool (t: 'a t) ~inserts ~range:(rstart, rstop) =
    let n = rstop - rstart in
    if n <= 0 then ()
    else if n <= threshold then
      for i = rstart to rstop - 1 do
        let (k, v) = inserts.(i) in
        Sequential.insert k v t
      done
    else
      let mid = rstart + n / 2 in
      let (mk, nv) = inserts.(mid) in
      let (lt, mn, rt) = Sequential.split t mk in
      let nn = match mn with
      | Leaf -> Sequential.new_node mk nv 
      | Node _ -> mn in
      let l = Domainslib.Task.async pool 
        (fun () -> par_insert_aux threshold ~pool lt ~inserts ~range:(rstart, mid)) in
      let r = Domainslib.Task.async pool
        (fun () -> par_insert_aux threshold ~pool rt ~inserts ~range:(mid + 1, rstop)) in
      Domainslib.Task.await pool l; Domainslib.Task.await pool r;
      let (nlt, _, _) = Sequential.split lt (Sequential.key nn) in (* Make sure there's no duplicate *)
      let (_, _, nrt) = Sequential.split rt (Sequential.key nn) in
      let nt = Sequential.join nlt nn nrt in
      t.root <- nt.root

  let par_insert ?threshold ~pool (t: 'a t) inserts =
    let threshold = match threshold with Some t -> t | None -> !avltree_insert_sequential_threshold in
    Sort.sort pool ~compare:(fun (k, _) (k', _) -> V.compare k k') inserts;
    par_insert_aux threshold ~pool t ~inserts ~range:(0, Array.length inserts)

  let run (type a) (t: a t) (pool: Domainslib.Task.pool) (ops: a wrapped_op array) =
    let searches: (V.t * (a option -> unit)) list ref = ref [] in
    let inserts: (V.t * a) list ref = ref [] in
    Array.iter (fun (elt: a wrapped_op) -> match elt with
    | Mk (Insert (key, vl), kont) -> kont (); inserts := (key,vl) :: !inserts
    | Mk (Search key, kont) -> searches := (key, kont) :: !searches
    ) ops;

    (* Initiate parallel searches *)
    let searches = Array.of_list !searches in
    if Array.length searches > 0 then
      par_search ~pool t searches;

    (* Initiate parallel inserts *)
    let inserts = Array.of_list !inserts in
    if Array.length inserts > 0 then begin
      Sort.sort pool ~compare:(fun (k1,_) (k2,_) -> V.compare k1 k2) inserts;
      par_insert ~pool t inserts
    end

end