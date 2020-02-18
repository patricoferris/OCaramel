(* Takes an execution log and makes sense of it *)
open Backends 

let open_file filename =
  open_in filename     

let read_file ic chunk = 
  let rec lines ic acc = function 
    | 0 -> (List.rev acc, ic, false)
    | n -> try let line = input_line ic in  
      lines ic (line::acc) (n - 1)
  with End_of_file -> close_in ic; (List.rev acc, ic, true) in lines ic [] chunk  

let stream_line ic = 
  Stream.from 
    (fun _ -> try Some (input_line ic) with End_of_file -> None)

let read_stream f ic = 
  Stream.iter (fun line -> f line) (stream_line ic)

let parse_log ic chunk = 
  let (lines, nic, closed) = read_file ic chunk in 
    (Utils.map (fun line -> Riscv.line_parse line) lines, nic, closed)

let exec_freq_instr freq_tbl instr = 
  let instr = Riscv.instr_to_string instr in 
      try let f = (Hashtbl.find freq_tbl instr) in
        Hashtbl.replace freq_tbl instr (f + 1)
      with Not_found -> Hashtbl.add freq_tbl instr 1
  
let execution_freq freq_tbl log = 
  let rec loop = function 
    | []    -> ()
    | i::is -> 
      let instr = Riscv.instr_to_string i in 
      try let f = (Hashtbl.find freq_tbl instr) in
        Hashtbl.replace freq_tbl instr (f + 1); loop is
      with Not_found -> Hashtbl.add freq_tbl instr 1; loop is in loop log

(********** CSV Output ***********)
let print_csv oc k v =
  Printf.fprintf oc "%s,%i\n" k v

let print_sorted tbl = 
  let key_values = List.sort (fun (_, v1) (_, v2) -> -Pervasives.compare v1 v2) (List.of_seq (Hashtbl.to_seq tbl)) in 
  let print_kv (k, v) = print_endline (k ^ ": " ^ (string_of_int v)) in 
    List.iter print_kv key_values; print_endline ("Total Number of Instructions: " ^ string_of_int ((List.fold_left (fun acc (_k, v) -> acc + v) 0 key_values) - (Hashtbl.find tbl "no_match")))

let to_csv filename tbl = 
  let oc = open_out filename in 
  let print = print_csv oc in
    Printf.fprintf oc "%s,%s\n" "instruction" "frequency";
    Hashtbl.iter print tbl; 
  close_out oc

(* Files are too big for memory - read in chuncks *)
let frequencies () =
  let freq_tbl = Hashtbl.create 50 in
  let add_to_table s = exec_freq_instr freq_tbl (Riscv.line_parse s) in 
  let _s = read_stream (add_to_table) stdin in 
    freq_tbl