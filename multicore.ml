let getLines =
  let inp = Unix.open_process_in "ps aux | grep supernova" in
  let lines = ref [] in
  try
    while true; do
      lines := input_line inp :: !lines
    done; !lines
  with
    End_of_file ->
    close_in inp;
    List.rev !lines

let fetchPid string =
  let lst = Str.split (Str.regexp " +") string in
  match List.nth_opt lst 10 with
  | Some "supernova" ->
     begin
     match lst with
     | [] -> None
     | _::pidStr::_ -> int_of_string_opt pidStr
     | _::[] -> None
     end
  | _ ->
     None
     

let buildTaskSetCmd cpuIndex pid =
  let pidStr = string_of_int pid in
  let cpuStr = string_of_int (cpuIndex + 1) in
  String.concat " " ["taskset -p -a --cpu-list";cpuStr;" ";pidStr;"\n"]
  
let listFromOpts lst =
  let f x acc =
    match x with
    | Some v -> v::acc
    | None -> acc
  in
  List.fold_right f lst []       
  
let () =
  let lines = getLines in
  let cmds = List.map fetchPid lines
             |> listFromOpts
             |> List.mapi buildTaskSetCmd
  in
  List.iter print_string cmds
