module ChanBased = struct
  type 'a t = {
    chan : 'a Chan.t;
    size : int Atomic.t;
    total_added : int Atomic.t;
    batch_limit : int
  }

  (* Change batch limit to (N)processes *)
  let create ?(batch_limit=max_int) () = 
    {
      chan = Chan.make_unbounded ();
      size = Atomic.make 0;
      total_added = Atomic.make 0;
      batch_limit;
    }
  let add t elt =
    ignore @@ Atomic.fetch_and_add t.size 1;
    Chan.send t.chan elt
  let get t = 
    (* let batch_size = Atomic.exchange t.size 0 in *)
    let batch_size = Atomic.get t.size in
    (* Printf.printf "batch_size = %d\n" batch_size; *)
    let limit = min batch_size t.batch_limit in
    let _ = Atomic.fetch_and_add t.total_added limit in
    (* let topup = batch_size - limit in *)
    (* let _ = Atomic.fetch_and_sub t.size topup in *)
    let batch_size_2 = Atomic.get t.size in
    if batch_size_2 < batch_size then Printf.printf "batch_size_2 < batch_size\n%!";
    let _ = Atomic.fetch_and_add t.size (-limit) in
    Array.init limit (fun _ -> Chan.recv t.chan)
  let size t = Atomic.get t.size 

  let total_added t = Atomic.get t.total_added
end

include ChanBased

(* type container = ELT.t option array 
   type t = {
   switching : bool Atomic.t;
   primary : container Atomic.t;
   mutable secondary : container;
   size : int Atomic.t
   }

   let create ~batch_size () = {
   switching = Atomic.make false;
   primary = Atomic.make @@ Array.make batch_size None;
   secondary =  Array.make batch_size None;
   size = Atomic.make 0
   }

   let add (t : t) (elt : ELT.t) = 
   (* Make sure switching process is not happening *)
   while Atomic.get t.switching do () done;
   let slot = Atomic.fetch_and_add t.size 1 in
   t.container.(slot) <- Some elt

   let get t = 
   let size = Atomic.get t.size in
   let batch = Array.make t.size None in
   Array.blit t.container 0 batch 0 t.size;
   batch

   let size t = failwith "" *)
