open Data
module IntSkiplist = Skiplist.Make(Int)

let write file (v: IntSkiplist.t)  : unit =
  Out_channel.with_open_bin file (fun oc -> Marshal.to_channel oc v []) 
let read file : IntSkiplist.t = In_channel.with_open_bin file Marshal.from_channel 
let print v = IntSkiplist.Sequential.print_slist v string_of_int

let () =
  match List.init (Array.length Sys.argv - 1) (fun i -> Sys.argv.(i + 1)) with
  | "init" :: file :: _ -> 
    write file (IntSkiplist.init ())
  | "print" :: file :: _ ->
    let t = read file in
    print t;
    write file t
  | "insert" :: file :: k :: _ -> 
    let t = read file in
    let k = int_of_string k in
    IntSkiplist.Sequential.insert t k;
    print t;
    write file t
  | "par_insert" :: file :: batch :: _ ->
    let t = read file in
    let batch = String.split_on_char ',' batch |> List.map int_of_string |> Array.of_list in
    let pool = Domainslib.Task.setup_pool ~num_domains:(Domain.recommended_domain_count () - 1) () in
    IntSkiplist.par_insert t pool batch;
    Domainslib.Task.teardown_pool pool;
    print t;
    write file t
  | _ -> failwith "Do nothing"