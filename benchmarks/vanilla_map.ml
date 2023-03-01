module IntMap = Map.Make(Int)

type generic_spec_args = {
  sorted: bool;
  no_searches: int;
  min: int;
  max: int;
  initial_count: int;
}

type generic_test_spec = {
  args: generic_spec_args;
  count: int;
  mutable initial_elements: int array;
  mutable insert_elements: int array;
  mutable search_elements: int array;
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
  Term.(const (fun sorted no_searches min max initial_count -> {
      sorted;
      no_searches=Option.value ~default:0 no_searches;
      initial_count=Option.value ~default:0 initial_count;
      min=Option.value ~default:0 min;
      max=Option.value ~default:((Int.shift_left 1 30) - 1) max;
    }) $ sorted $ no_searches $ min $ max $ initial_count)

let generic_test_spec ~count spec_args =
  { args=spec_args; count: int; insert_elements=[| |]; search_elements=[| |]; initial_elements=[| |] }

let generic_init test_spec f =
  let min, max =  test_spec.args.min, test_spec.args.max in
  let elements = Util.gen_random_unique_array ~min ~max (test_spec.args.initial_count + test_spec.count) in
  let initial_elements = Array.make test_spec.args.initial_count min in
  let insert_elements = Array.make test_spec.count min in
  let search_elements = Util.gen_random_unique_array ~min ~max test_spec.args.no_searches in
  Array.blit
    elements 0
    initial_elements 0
    test_spec.args.initial_count;
  Array.blit
    elements test_spec.args.initial_count
    insert_elements 0
    test_spec.count;

  if test_spec.args.sorted then
    Array.sort Int.compare insert_elements;
  test_spec.insert_elements <- insert_elements;
  test_spec.initial_elements <- initial_elements;
  test_spec.search_elements <- search_elements;
  f initial_elements


module Sequential = struct

  type t = unit IntMap.t ref

  type spec_args = generic_spec_args
  let spec_args = generic_spec_args

  type test_spec = generic_test_spec
  let test_spec = generic_test_spec

  let init _pool test_spec =
    generic_init test_spec (fun initial_elements ->
      let set = IntMap.of_seq (Array.to_seq initial_elements |> Seq.map (fun i -> (i, ()))) in
      ref set
    )

  let run _pool (t: t) test_spec =
    for i = 0 to Array.length test_spec.insert_elements - 1 do
      t := IntMap.add test_spec.insert_elements.(i) () !t
    done;
    for i = 0 to Array.length test_spec.search_elements - 1 do
       ignore (IntMap.find_opt test_spec.search_elements.(i) !t)
    done

  let cleanup (_t: t) (_test_spec: test_spec) = ()

end


module CoarseGrained = struct

  type t = {
    mutable set: unit IntMap.t;
    lock: Mutex.t;
  }

  type spec_args = generic_spec_args
  let spec_args = generic_spec_args

  type test_spec = generic_test_spec
  let test_spec = generic_test_spec

  let init _pool test_spec =
    generic_init test_spec (fun initial_elements ->
      let set = IntMap.of_seq (Array.to_seq initial_elements |> Seq.map (fun i -> (i, ()))) in
      {set; lock=Mutex.create ()}
    )

  let run pool (t: t) test_spec =
    Domainslib.Task.parallel_for pool ~chunk_size:1
      ~start:0 ~finish:(Array.length test_spec.insert_elements + Array.length test_spec.search_elements - 1)
      ~body:(fun i ->
        Mutex.lock t.lock;
        Fun.protect ~finally:(fun () -> Mutex.unlock t.lock) (fun () ->
          if i < Array.length test_spec.insert_elements
          then t.set <- IntMap.add test_spec.insert_elements.(i) () t.set
          else ignore (IntMap.find_opt test_spec.search_elements.(i - Array.length test_spec.insert_elements) t.set)
        )
      )

  let cleanup (_t: t) (_test_spec: test_spec) = ()

end


module Batched = struct

  module IntMapWrapper = struct
    type t = unit IntMap.t ref

    type _ op =
      | Insert : int -> unit op
      | Member : int -> bool op

    type wrapped_op = Mk: 'a op * ('a -> unit) -> wrapped_op

    let init () = ref IntMap.empty

    let run (t: t) (pool: Domainslib.Task.pool) (ops: wrapped_op array) =
      let searches : (int * (bool -> unit)) list ref = ref [] in
      let inserts : int list ref = ref [] in
      Array.iter (fun (elt: wrapped_op) -> match elt with
        | Mk (Insert key, kont) -> kont (); inserts := key :: !inserts
        | Mk (Member key, kont) -> searches := (key, kont) :: !searches
      ) ops;
      let searches = Array.of_list !searches in
      let set = !t in
      Domainslib.Task.parallel_for pool ~start:0 ~finish:(Array.length searches - 1) ~body:(fun i ->
        let key, kont = searches.(i) in
        kont (Option.is_some (IntMap.find_opt key set))
      );
      let inserts = Array.of_list !inserts in
      for i = 0 to Array.length inserts - 1 do
        t := IntMap.add inserts.(i) () !t
      done

  end

  module BatchedIntMap = Domainslib.Batcher.Make (IntMapWrapper)

  type t = BatchedIntMap.t

  type spec_args = generic_spec_args
  let spec_args = generic_spec_args

  type test_spec = generic_test_spec
  let test_spec = generic_test_spec

  let init pool test_spec =
    generic_init test_spec (fun initial_elements ->
      let set = BatchedIntMap.init pool in
      for i = 0 to Array.length initial_elements - 1 do
        BatchedIntMap.apply set (Insert initial_elements.(i));
      done;
      set
    )

  let run pool (t: t) test_spec =
    Domainslib.Task.parallel_for pool ~chunk_size:1
      ~start:0 ~finish:(Array.length test_spec.insert_elements + Array.length test_spec.search_elements - 1)
      ~body:(fun i ->
        if i < Array.length test_spec.insert_elements
        then BatchedIntMap.apply t (Insert test_spec.insert_elements.(i))
        else ignore (BatchedIntMap.apply t (Member test_spec.search_elements.(i - Array.length test_spec.insert_elements)))
      )

  let cleanup (_t: t) (_test_spec: test_spec) = ()

end
