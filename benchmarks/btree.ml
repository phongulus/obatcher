module IntSet = Set.Make(Int)
module IntBtree = Data.Btree.Make(Int)
module BatchedIntBtree = Domainslib.Batcher.Make1(IntBtree)

type generic_test_spec = {
  initial_elements: (unit -> int array);
  insert_elements: int array;
  search_elements: int array;
}

type generic_spec_args = {
  sorted: bool;
  no_searches: int;
  min: int;
  max: int;
  initial_count: int;
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

  Term.(const (fun sorted no_searches min max initial_count  -> {
      sorted;
      no_searches=Option.value ~default:0 no_searches;
      initial_count=Option.value ~default:1_000 initial_count;
      min=Option.value ~default:(-10_000_000) min;
      max=Option.value ~default:((Int.shift_left 1 30) - 1) max;
    }) $ sorted $ no_searches $ min $ max $ initial_count)

let generic_test_spec ~count spec_args =
  let min, max =  spec_args.min, spec_args.max in
  let initial_elements () = Util.gen_random_uniqe_array ~min ~max spec_args.initial_count in
  let insert_elements = Util.gen_random_uniqe_array ~min ~max count in
  let search_elements = Util.gen_random_uniqe_array ~min ~max spec_args.no_searches in
  if spec_args.sorted then
    Array.sort Int.compare insert_elements;
  { initial_elements; insert_elements; search_elements }

module Sequential = struct

  type t = unit IntBtree.t

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args

  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~count spec_args =
    generic_test_spec ~count spec_args

  let init _pool test_spec =
    let tree = IntBtree.Sequential.init ~max_children:4 () in
    Array.iter (fun i -> IntBtree.Sequential.insert tree i ())
      (test_spec.initial_elements ());
    tree

  let run _pool t test_spec =
    Array.iter (fun i ->
        IntBtree.Sequential.insert t i ()
      ) test_spec.insert_elements;
    Array.iter (fun i ->
        ignore @@ IntBtree.Sequential.search t i
      ) test_spec.search_elements;

end


module CoarseGrained = struct

  type t = {tree: unit IntBtree.t; mutex: Mutex.t}

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args

  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~count spec_args =
    generic_test_spec ~count spec_args

  let init _pool test_spec =
    let tree = IntBtree.Sequential.init ~max_children:4 () in
    Array.iter (fun i -> IntBtree.Sequential.insert tree i ())
      (test_spec.initial_elements ());
    let mutex = Mutex.create () in
    {tree;mutex}

  let run pool (t: t) test_spec =
    Domainslib.Task.parallel_for pool ~chunk_size:1
      ~start:0 ~finish:(Array.length test_spec.insert_elements + Array.length test_spec.search_elements - 1)
      ~body:(fun i ->
          Mutex.lock t.mutex;
          Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) (fun () ->
              if i < Array.length test_spec.insert_elements
              then IntBtree.Sequential.insert t.tree test_spec.insert_elements.(i) ()
              else ignore (IntBtree.Sequential.search t.tree
                             test_spec.search_elements.(i - Array.length test_spec.search_elements))
            )
        )

end


module Batched = struct

  type t = unit BatchedIntBtree.t

  type test_spec = generic_test_spec

  type spec_args = generic_spec_args

  let spec_args: spec_args Cmdliner.Term.t = generic_spec_args

  let test_spec ~count spec_args =
    generic_test_spec ~count spec_args

  let init pool test_spec =
    let tree = BatchedIntBtree.init pool in
    Domainslib.Task.run pool (fun () ->
        Array.iter (fun i -> BatchedIntBtree.apply tree (Insert (i, ())))
          (test_spec.initial_elements ());
      );
    tree

  let run pool (tree: t) test_spec =
    Domainslib.Task.parallel_for pool ~chunk_size:1
      ~start:0 ~finish:(Array.length test_spec.insert_elements + Array.length test_spec.search_elements - 1)
      ~body:(fun i ->
          if i < Array.length test_spec.insert_elements
          then BatchedIntBtree.apply tree (Insert (test_spec.insert_elements.(i), ()))
          else 
            ignore (BatchedIntBtree.apply tree (Search test_spec.search_elements.(i - Array.length test_spec.insert_elements)))
        )

end

