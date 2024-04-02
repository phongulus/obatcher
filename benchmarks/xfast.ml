module IntXfastTrie = Data.Xfast.Sequential(Data.Xfast.ArrXfastTabl)
module IntXfastTriePrebatch = Data.Xfast.Prebatch(Data.Xfast.ArrXfastTabl)
module IntXfastTrieFunctor = Data.Exposerepair.Make(IntXfastTriePrebatch)
module BatchedIntXfastTrie = Domainslib.Batcher.Make(IntXfastTrieFunctor)

let int_size = 24

type generic_test_spec = {
  initial_elements: int array;
  insert_elements : int array;
  search_elements : int array;
}

type generic_spec_args = {
  sorted : bool;
  no_searches : int;
  initial_count: int;
  min: int;
  max: int;
}

let generic_spec_args: generic_spec_args Cmdliner.Term.t =
  let open Cmdliner in
  let sorted = Arg.(value @@ flag  @@ info ~doc:"whether the inserts should be sorted" ["s"; "sorted"]) in
  let no_searches =
    Arg.(value @@ opt (some int) None @@ info ~doc:"number of searches" ~docv:"NO_SEARCHES" ["n"; "no-searches"]) in
  let initial_count =
    Arg.(value @@ opt (some int) None @@ info ~doc:"Initial number of operations" ["init-count"]) in
  let min =
    Arg.(value @@ opt (some int) None @@ info ~doc:"Minimum value of data for random inputs" ["min"]) in
  let max =
    Arg.(value @@ opt (some int) None @@ info ~doc:"Maximum value of data for random inputs" ["max"]) in
  Term.(const (fun sorted no_searches initial_count min max ->
    {
      sorted;
      no_searches=Option.value ~default:0 no_searches;
      initial_count=Option.value ~default:0 initial_count;
      min=Option.value ~default:0 min;
      max=Option.value ~default:(Int.shift_left 1 int_size) max;
    }) $ sorted $ no_searches $ initial_count $ min $ max)

let generic_test_spec ~count spec_args =
  let {min;max;initial_count;_} = spec_args in
  let all_elements = Util.gen_random_unique_array ~min ~max (initial_count + count) in
  let initial_elements = Array.sub all_elements 0 initial_count in
  let insert_elements = Array.sub all_elements initial_count count in
  let search_elements = Util.gen_random_array ~min ~max spec_args.no_searches in
  if spec_args.sorted then
    Array.sort Int.compare insert_elements;
  { initial_elements; insert_elements; search_elements}

module Sequential = struct

  type t = IntXfastTrie.t

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args

  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~count spec_args =
    generic_test_spec ~count spec_args

  let init _pool test_spec = 
    let initial_elements = test_spec.initial_elements in
    let xfast = IntXfastTrie.init int_size in
    Array.iter (fun i -> IntXfastTrie.insert xfast i) initial_elements;
    xfast

  let run _pool t test_spec =
    Array.iter (fun i ->
        IntXfastTrie.insert t i) test_spec.insert_elements;
    Array.iter (fun i ->
        IntXfastTrie.mem t i |> ignore) test_spec.search_elements

  let cleanup (_t: t) (_test_spec: test_spec) = ()
    (* let all_elements = Array.concat [test_spec.insert_elements; test_spec.initial_elements] in
    Array.sort Int.compare all_elements;
    let all_elements_list = Array.to_list all_elements in
    let vebtree_flattened = IntXfastTrie.flatten t in
    assert (all_elements_list = vebtree_flattened);
    for i = 0 to Array.length all_elements - 1 do
      if i > 0 then
        assert (all_elements.(i - 1) = Option.get @@ IntXfastTrie.predecessor t all_elements.(i));
      if i < Array.length all_elements - 1 then
        assert (all_elements.(i + 1) = Option.get @@ IntXfastTrie.successor t all_elements.(i))
    done *)

end


module CoarseGrained = struct

  type t = {xfast : IntXfastTrie.t; mutex : Mutex.t}

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args


  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~count spec_args =
    generic_test_spec ~count spec_args

  let init _pool test_spec = 
    let initial_elements = test_spec.initial_elements in
    let xfast = IntXfastTrie.init int_size in
    Array.iter (fun i -> IntXfastTrie.insert xfast i) initial_elements;
    {xfast; mutex=Mutex.create ()}

  let run pool t test_spec =
    let total = Array.length test_spec.insert_elements + Array.length test_spec.search_elements - 1 in
    Domainslib.Task.parallel_for pool
      ~start:0 ~finish:total ~chunk_size:1
      ~body:(fun i ->
          Mutex.lock t.mutex;
          Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
              if i < Array.length test_spec.insert_elements
              then IntXfastTrie.insert t.xfast test_spec.insert_elements.(i)
              else if i < Array.length test_spec.insert_elements + Array.length test_spec.search_elements then
                ignore (IntXfastTrie.mem t.xfast
                          test_spec.search_elements.(i - Array.length test_spec.insert_elements))
            )
        )

  let cleanup (_t: t) (_test_spec: test_spec) = ()
    (* let all_elements = Array.concat [test_spec.insert_elements; test_spec.initial_elements] in
    Array.sort Int.compare all_elements;
    let all_elements = Array.to_list all_elements in
    let vebtree_flattened = IntXfastTrie.flatten t.xfast in
    assert (all_elements = vebtree_flattened) *)

end

module Batched = struct

  type t = BatchedIntXfastTrie.t

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args

  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~count spec_args =
    generic_test_spec ~count spec_args

  let init pool test_spec = 
    let initial_elements = test_spec.initial_elements in
    let xfast = BatchedIntXfastTrie.init pool in
    let exposed_tree = BatchedIntXfastTrie.unsafe_get_internal_data xfast in
    Array.iter (fun i -> IntXfastTrie.insert exposed_tree i) initial_elements;
    xfast

  let run pool t test_spec =
    (* Printf.printf "inserting %d elements\n" (Array.length test_spec.insert_elements); *)
    let total = Array.length test_spec.insert_elements + Array.length test_spec.search_elements - 1 in
    Domainslib.Task.parallel_for pool
      ~start:0 ~finish:total ~chunk_size:1
      ~body:(fun i ->
          if i < Array.length test_spec.insert_elements
          then BatchedIntXfastTrie.apply t (Insert test_spec.insert_elements.(i))
          else if i < Array.length test_spec.insert_elements + Array.length test_spec.search_elements then
            ignore (BatchedIntXfastTrie.apply t 
                      (Member test_spec.search_elements.(i - Array.length test_spec.insert_elements)))
        );
    BatchedIntXfastTrie.wait_for_batch t

  let cleanup (t: t) (test_spec: test_spec) =
    let t = BatchedIntXfastTrie.unsafe_get_internal_data t in
    let all_elements = Array.concat [test_spec.insert_elements; test_spec.initial_elements] in
    Array.sort Int.compare all_elements;
    let all_elements_list = Array.to_list all_elements in
    let vebtree_flattened = IntXfastTrie.flatten t in
    assert (all_elements_list = vebtree_flattened)

end
