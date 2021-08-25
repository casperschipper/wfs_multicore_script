(*
let fetchPid processName string =
  let lst = Str.split (Str.regexp " +") string in
  match List.nth_opt lst 10 with
  | Some name when name == processName ->
     if name == processName then
       begin
         match lst with
         | [] -> None
         | _::pidStr::_ -> int_of_string_opt pidStr
         | _::[] -> None
       end
     else
       None
  | _ ->
     None
*)

let getPids processName =
  let inp = Unix.open_process_in ("pgrep -f " ^ processName) in
  let pids = ref [] in
  try
    while true; do
      let pidStr = input_line inp in
      match int_of_string_opt pidStr with
      | None -> ()
      | Some pid -> print_int pid ; print_string processName ; print_newline () ; pids := pid :: !pids
    done; !pids
  with
    End_of_file ->
    close_in inp;
    List.rev !pids

(*
let getLines processName =
  let inp = Unix.open_process_in ("ps aux | grep " ^ processName)  in
  let lines = ref [] in
  try
    while true; do
      lines := input_line inp :: !lines
    done; !lines
  with
    End_of_file ->
    close_in inp;
    List.rev !lines
 *)

let buildTaskSetCmd cpuIndex pid =
  let pidStr = string_of_int pid in
  let cpuStr = string_of_int (cpuIndex + 2) in
  String.concat " " ["taskset -p -a --cpu-list";cpuStr;" ";pidStr;"\n"]

  (*
let listFromOpts lst =
  let f x acc =
    match x with
    | Some v -> v::acc
    | None -> acc
  in
  List.fold_right f lst []
  *)

let (<|) f a = f a

let () =
  let pids = getPids "[s]upernova" @ getPids "[j]ackd" @ getPids "[s]clang" in
  let n = List.length pids in
  if n > 12 then
    print_string <| "you have more processes, (number = " ^ string_of_int n ^ " - than cores, try reduce the number of servers in WFS preferences?"
  else
    let cmds =
      List.mapi buildTaskSetCmd pids
    in
    List.iter (fun cmd ->
        print_string <| "executing cmd:\n" ^ cmd ^ "\n";
        Sys.command cmd |>
          print_int)  cmds
