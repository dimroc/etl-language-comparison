let fold_lines f init file =
  let input = open_in file in
  let rec fold state =
    match input_line input with
    | line -> fold (f state line)
    | exception _ -> close_in input; state in
  fold init

open Batteries

let knicks_re =
  Re_pcre.re "^.*\t(.*)\t.*\t.*knicks.*$" |> Re.no_case |> Re.whole_string |> Re.compile

let update map line =
  match Re.exec_opt knicks_re line with
  | Some groups ->
    let hood = Re.Group.get groups 1 in
    Map.modify_def 0 hood succ map
  | None -> map

let mapper file =
  let filename = Filename.concat "../tmp/tweets" file in
  fold_lines update Map.empty filename

let merge =
  Map.merge @@ fun _ mo no ->
    match mo, no with
    | Some m, Some n -> Some (m + n)
    | Some n, None | None, Some n -> Some n
    | None, None -> None

let compare (hood1, count1) (hood2, count2) =
  let cmp = Int.compare count2 count1 in
  if cmp = 0 then String.compare hood1 hood2 else cmp

let () =
  let files = Sys.readdir "../tmp/tweets" in
  let maps = Parmap.array_parmap mapper files in
  let map = Array.reduce merge maps in
  let results = Map.bindings map |> List.sort compare in
  File.with_file_out "../tmp/ocaml_output" @@ fun file ->
    results |> List.iter @@ fun (hood, count) ->
      Printf.fprintf file "%s\t%d\n" hood count
