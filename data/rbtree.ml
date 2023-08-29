module Make (V: Map.OrderedType) = struct

  module Sequential = struct
    type colour = Red | Black;;
    type side = Left | Right;;

    type 'a rb_node = Leaf | Node of {
      key: V.t;
      mutable tval: 'a;
      mutable colour: colour;
      mutable rl: side;
      mutable parent: 'a rb_node;
      mutable left: 'a rb_node;
      mutable right: 'a rb_node
    }

    type 'a rb_tree = {
      mutable root: 'a rb_node
    }

    let new_tree () = {root = Leaf}

    let new_node k v = Node {
      key = k;
      tval = v;
      colour = Red;
      rl = Left;
      parent = Leaf;
      left = Leaf;
      right = Leaf
    }

    let rec search_aux k n =
      match n with
      | Leaf -> None
      | Node n' ->
        if k == n'.key then Some n'.tval
        else if k > n'.key then search_aux k n'.right
        else search_aux k n'.left
  
    let search k t = search_aux k t.root
  
    let get_node_colour n =
      match n with
      | Leaf -> Black
      | Node n' -> n'.colour
  
    let get_node_key n =
      match n with
      | Leaf -> failwith "Can't get node key"
      | Node n' -> n'.key
  
    (* let rec traverse_aux n =
      match n with
      | Leaf -> ()
      | Node n' -> begin
          let col = if n'.colour = Red then "Red" else "Black" in
          let side = if n'.rl = Right then "Right" else "Left" in
          Printf.printf "(%d, %d, %d, %s, %s), " n'.key n'.tval (get_node_key n'.parent) col side;
          traverse_aux n'.left;
          traverse_aux n'.right
        end
  
    let traverse t =
      traverse_aux t.root; Printf.printf "\n" *)
  
    let rotate_left x t =
      (* Printf.printf "Rotating!\n"; *)
      (* traverse t; *)
      match x with
      | Leaf -> failwith "Couldn't left rotate, x empty\n"
      | Node x' -> begin
          let y = x'.right in
          (* Printf.printf "%d\n" @@ get_node_key y; *)
          match y with
          | Leaf -> failwith "Couldn't left rotate, y empty\n"
          | Node y' -> begin
            x'.right <- y'.left;
            (match y'.left with
            | Leaf -> () (* Printf.printf "No change here\n" *) (* No change here *)
            | Node yl' -> (yl'.rl <- Right; yl'.parent <- x));
            (* get_node_key *)
            (* Printf.printf "%d\n" @@ get_node_key t.root; *)
            (* t.root <- Node y'; *)
            (* y'.parent <- x'.parent; *)
            (match x'.parent with
            | Leaf -> (t.root <- y; (*Printf.printf "Changing root\n"*))
            | Node xp' -> if x'.rl = Left then (y'.rl <- Left; xp'.left <- y) else (y'.rl <- Right; xp'.right <- y));
            y'.parent <- x'.parent;
            x'.rl <- Left;
            y'.left <- x;
            x'.parent <- y;
            (* traverse t; *)
            end
        end
  
    let rotate_right x t =
      match x with
      | Leaf -> failwith "Couldn't right rotate, x empty\n"
      | Node x' -> begin
          let y = x'.left in
          match y with
          | Leaf -> failwith "Couldn't right rotate, y empty\n"
          | Node y' -> begin
              x'.left <- y'.right;
              (match y'.right with
              | Leaf -> () (* No change here *)
              | Node yr' -> (yr'.rl <- Right; yr'.parent <- x));
              (* y'.parent <- x'.parent; *)
              (match x'.parent with
              | Leaf -> t.root <- y
              | Node xp' -> if x'.rl = Left then (y'.rl <- Left; xp'.left <- y) else (y'.rl <- Right; xp'.right <- y));
              y'.parent <- x'.parent;
              x'.rl <- Right;
              y'.right <- x;
              x'.parent <- y
            end
        end
  
    (* Given the grandparent, parent, and current nodes, *)
    let rec fix_tree n t =
      match n with
      | Leaf -> failwith "Cannot fix a Leaf"
      | Node n' ->
        let np = n'.parent in
        match np with
        | Leaf -> () (* We reached the root node, exiting *)
        | Node np' ->
          if np'.colour = Red then
            let ngp = np'.parent in
            match ngp with
            | Leaf -> () (* failwith "n must have a grandparent node" *)
            | Node ngp' -> begin
                if np'.rl = Right then
                  let u = ngp'.left in
                  (* traverse t; *)
                  if get_node_colour u = Red then
                    match u with
                    | Leaf -> failwith "u cannot be Leaf at this juncture"
                    | Node u' -> (u'.colour <- Black; np'.colour <- Black; ngp'.colour <- Red; fix_tree ngp t)
                  else if n'.rl = Left then
                    begin
                      rotate_right np t;
                      let nngp = np'.parent in
                      match nngp with
                      | Leaf -> failwith "new parent cannot be Leaf"
                      | Node nngp' -> begin
                          nngp'.colour <- Black;
                          let nnggp = nngp'.parent in
                          match nnggp with
                          | Leaf -> failwith "new grandparent cannot be Leaf"
                          | Node nnggp' -> begin
                              nnggp'.colour <- Red;
                              rotate_left nnggp t;
                              fix_tree np t
                            end
                        end
                    end
                  else (np'.colour <- Black; ngp'.colour <- Red; rotate_left ngp t; fix_tree n t)
                else
                  let u = ngp'.right in
                  if get_node_colour u == Red then
                    match u with
                    | Leaf -> failwith "u cannot be Leaf at this juncture"
                    | Node u' -> (u'.colour <- Black; np'.colour <- Black; ngp'.colour <- Red; fix_tree ngp t)
                  else if n'.rl = Right then
                    begin
                      rotate_left np t;
                      let nngp = np'.parent in
                      match nngp with
                      | Leaf -> failwith "new parent cannot be Leaf"
                      | Node nngp' -> begin
                          nngp'.colour <- Black;
                          let nnggp = nngp'.parent in
                          match nnggp with
                          | Leaf -> failwith "new grandparent cannot be Leaf"
                          | Node nnggp' -> begin
                              nnggp'.colour <- Red;
                              rotate_right nnggp t;
                              fix_tree np t
                            end
                        end
                    end
                  else (np'.colour <- Black; ngp'.colour <- Red; rotate_right ngp t; fix_tree n t)
              end
  
    (* Insert node into the red-black tree, and rebalance afterwards *)
    let rec insert_aux new_node current_node t =
      match new_node with
      | Leaf -> failwith "Can't insert Leaf"
      | Node n ->
        match current_node with
        | Leaf -> failwith "This is not supposed to happen"
        | Node n' ->
          if n'.key = n.key then
            n'.tval <- n.tval
          else if n'.key > n.key then
            match n'.left with
            | Leaf -> begin
                n.colour <- Red;
                n.rl <- Left;
                n.parent <- current_node;
                n'.left <- new_node;
                (* traverse t; *)
                fix_tree new_node t;
                (match t.root with
                | Leaf -> failwith "bruh"
                | Node tr -> tr.colour <- Black);
                (* traverse t; *)
              end
            | Node _ -> insert_aux new_node n'.left t
          else
            match n'.right with
            | Leaf -> begin
                n.colour <- Red;
                n.rl <- Right;
                n.parent <- current_node;
                n'.right <- new_node;
                (* traverse t; *)
                fix_tree new_node t;
                (match t.root with
                | Leaf -> failwith "bruh"
                | Node tr -> tr.colour <- Black);
                (* traverse t; *)
              end
            | Node _ -> insert_aux new_node n'.right t
  
    let insert new_node t =
      (* Printf.printf "Hi"; *)
      match new_node with
      | Leaf -> failwith "Can't insert Leaf"
      | Node n ->
        match t.root with
        | Leaf -> (n.colour <- Black; n.parent <- Leaf; t.root <- new_node)
        | Node _ -> insert_aux new_node t.root t
  
    let rec get_black_height_aux acc n =
      match n with
      | Leaf -> acc + 1
      | Node n' -> get_black_height_aux (acc + if n'.colour = Black then 1 else 0) n'.left
  
    let get_black_height n = get_black_height_aux 0 n
  
    (* Use directly with nodes, no going through the tree type *)
    let rec join_right tl n tr =
      if get_node_colour tl.root = Black && get_black_height tl.root = get_black_height tr.root then
        match n with
        | Leaf -> failwith "n cannot be a Leaf for joining"
        | Node n' -> 
          (match tl.root with
          | Leaf -> ()
          | Node tln' -> tln'.parent <- n; tln'.rl <- Left);
          (match tr.root with
          | Leaf -> ()
          | Node trn' -> trn'.parent <- n; trn'.rl <- Right);
          n'.left <- tl.root; n'.right <- tr.root; {root = n}
      else begin
        match tl.root with
        | Leaf -> failwith "tl needs to contain a root node"
        | Node tln' ->
          (match tln'.right with
          | Leaf -> ()
          | Node tlnrn' -> tlnrn'.parent <- Leaf);
          let ntr = join_right {root = tln'.right} n tr in
          let t' = Node {
            key = tln'.key;
            tval = tln'.tval;
            colour = tln'.colour;
            rl = Left;
            parent = Leaf;
            left = tln'.left;
            right = ntr.root
          } in
          (match tln'.left with
          | Leaf -> ()
          | Node tlnln' -> tlnln'.parent <- t');
          match ntr.root with
          | Leaf -> failwith "this can't be a leaf"
          | Node ntrn' ->
            if tln'.colour = Black && get_node_colour ntr.root = Red && get_node_colour ntrn'.right = Red then
              match ntrn'.right with
              | Leaf -> failwith "this can't be a leaf"
              | Node ntrnrn' ->
                ntrnrn'.colour <- Black;
                let nt = {root = t'} in
                rotate_left t' nt; nt
            else {root = t'}
      end
  
    let rec join_left tl n tr =
      if get_node_colour tr.root = Black && get_black_height tl.root = get_black_height tr.root then
        match n with
        | Leaf -> failwith "n cannot be a Leaf for joining"
        | Node n' -> 
          (match tl.root with
          | Leaf -> ()
          | Node tln' -> tln'.parent <- n; tln'.rl <- Left);
          (match tr.root with
          | Leaf -> ()
          | Node trn' -> trn'.parent <- n; trn'.rl <- Right);
          n'.left <- tl.root; n'.right <- tr.root; {root = n}
      else begin
        match tr.root with
        | Leaf -> failwith "tl needs to contain a root node"
        | Node trn' ->
          (match trn'.left with
          | Leaf -> ()
          | Node tlnln' -> tlnln'.parent <- Leaf);
          let ntl = join_left {root = trn'.left} n tr in
          let t' = Node {
            key = trn'.key;
            tval = trn'.tval;
            colour = trn'.colour;
            rl = Left;
            parent = Leaf;
            left = ntl.root;
            right = trn'.right
          } in
          (match trn'.right with
          | Leaf -> ()
          | Node trnrn' -> trnrn'.parent <- t');
          match ntl.root with
          | Leaf -> failwith "this can't be a leaf"
          | Node ntln' ->
            if trn'.colour = Black && get_node_colour ntl.root = Red && get_node_colour ntln'.left = Red then
              match ntln'.right with
              | Leaf -> failwith "this can't be a leaf"
              | Node ntlnln' ->
                ntlnln'.colour <- Black;
                let nt = {root = t'} in
                rotate_right t' nt; nt
            else {root = t'}
      end
  
    let join tl n tr =
      if get_black_height tl.root > get_black_height tr.root then
        let t' = join_right tl n tr in
        (match t'.root with
        | Leaf -> failwith "joined tree cannot be a single Leaf"
        | Node tn' -> if tn'.colour = Red && get_node_colour tn'.right = Red then tn'.colour <- Black);
        t'
      else if get_black_height tl.root < get_black_height tr.root then
        let t' = join_left tl n tr in
        (match t'.root with
        | Leaf -> failwith "joined tree cannot be a single Leaf"
        | Node tn' -> if tn'.colour = Red && get_node_colour tn'.left = Red then tn'.colour <- Black);
        t'
      else
        let () = match tl.root with
        | Leaf -> ()
        | Node tln' -> tln'.parent <- n in
        let () = match tr.root with
        | Leaf -> ()
        | Node trn' -> trn'.parent <- n in
        let () = match n with
        | Leaf -> failwith "can't join with a Leaf node"
        | Node n' -> (
          n'.colour <- if get_node_colour tl.root = Black && get_node_colour tr.root = Black then Red else Black;
          n'.left <- tl.root; n'.right <- tr.root) in
        {root = n}
  
    let rec split_aux n k =
      match n with
      | Leaf -> (Leaf, Leaf, Leaf)
      | Node n' ->
        if n'.key = k then begin
          (match n'.left with
          | Leaf -> ()
          | Node nln' -> nln'.parent <- Leaf);
          (match n'.right with
          | Leaf -> ()
          | Node nrn' -> nrn'.parent <- Leaf);
          (n'.left, n, n'.right)
        end
        else if k < n'.key then
          let (l, b, r) = split_aux n'.left k in
          (l, b, (join {root = r} n {root = n'.right}).root)
        else
          let (l, b, r) = split_aux n'.right k in
          ((join {root = n'.left} n {root = l}).root, b, r)
    
    let split t k =
      let (l, b, r) = split_aux t.root k in
      ({root = l}, b, {root = r})
  end

  

end