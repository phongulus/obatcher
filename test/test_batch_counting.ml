let n = try int_of_string Sys.argv.(4) with _ -> 1_000_000

module T = Domainslib.Task
module C = Counters.BatchedCounter

let () =
  Printf.printf "\n\n\nRunning Par_Prefix_sum's benchmarks, ops = %d\n" n;
  Format.printf "@." ;
  Format.printf "     num_domains: " ;
  for i = 0 to 7 do
    Format.printf " %5i   " (i + 1)
  done ;
  Format.printf "@." ;
  Format.printf "  Par_prefix_sum: " ;
  for num_domains = 0 to 7 do
    let pool = T.setup_pool ~num_domains () in
    let t = C.create n in
    let op_arr = Array.make n C.Null in
    let _ = Array.init n (fun i ->
        let pr, set = T.promise () in
        op_arr.(i) <- C.Incr (t, set);
        pr) in
    let t0 = Unix.gettimeofday () in
    T.run pool (fun () -> C.par_prefix_sums pool t op_arr);
    assert (C.unsafe_get t = n);
    let t1 = Unix.gettimeofday () in
    let op_ms = (Int.to_float n) /. (1000.0 *. (t1 -. t0)) in
    Format.printf "  %7s%!"
      (Printf.sprintf "%.0f" op_ms);
    T.teardown_pool pool;
  done;
  Printf.printf "\n\n\n"
