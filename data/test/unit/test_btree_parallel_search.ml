let num_domains = Domain.recommended_domain_count () [@alert "-unstable"]
let pool = Domainslib.Task.setup_pool ~num_domains:(num_domains - 1) ()

module IntSet = Set.Make(Int)
module Btree = Data.Btree.Make(Int)

let gen_unique_array size f =
  let seen_ints = ref IntSet.empty in
  let rec fresh_int s =
    let vl = Random.int (size * 10) in
    if IntSet.mem vl !seen_ints
    then fresh_int s
    else (seen_ints := IntSet.add vl !seen_ints; vl) in
  Array.init size (fun _ -> f (fresh_int ()))


let test_all_inserts_can_be_found_parallel t =
  let id = ref 0 in
  let fresh_id () = incr id; Format.sprintf "id-%d" !id in
  let no_elts = 2_000_000 in
  let no_to_insert = no_elts / 2 in

  let total_values = gen_unique_array no_elts (fun i -> (i, fresh_id ())) in

  let called_functions = Array.make no_elts false in

  let query_results = Array.make no_elts None in

  let to_query =
    Array.mapi (fun ind (i, _) ->
      (i, fun result ->
      query_results.(ind) <- result;
      called_functions.(ind) <- true)) total_values in

  let tree = Btree.Sequential.init ~max_children:t () in
  for i = 0 to no_to_insert - 1 do
    let k,vl = total_values.(i) in
    Btree.Sequential.insert tree k vl;
  done;
  Btree.par_search ~pool tree to_query;
  for i = 0 to Array.length query_results - 1 do
    assert (called_functions.(i));
    let vl = query_results.(i) in
    match vl with
    | None -> assert (i >= no_to_insert)
    | Some vl ->
      assert (i < no_to_insert);
      assert (vl = snd total_values.(i))
  done

let () =
  Domainslib.Task.run pool (fun () -> 
  for t = 2 to 5 do
    test_all_inserts_can_be_found_parallel t
  done
  )

  
  


