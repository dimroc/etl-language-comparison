open Batteries

let knicks_re = Re.str "knicks" |> Re.no_case |> Re.compile

let update map line =
  match String.nsplit line "\t" with
  | [_; hood; _; message] when Re.execp knicks_re message ->
    Map.modify_def (-1) hood succ map
  | _ ->
    map

let mapper file =
  let filename = Filename.concat "../tmp/tweets" file in
  File.with_file_in filename @@ fun file ->
    IO.lines_of2 file |> fold update Map.empty

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
