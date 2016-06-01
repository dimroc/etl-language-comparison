open Batteries

let knicks_re = Re.str "knicks" |> Re.no_case |> Re.compile

let mapper file =
  let table = Hashtbl.create 200 in
  let filename = Filename.concat "../tmp/tweets" file in
  let () = File.with_file_in filename @@ fun file ->
    IO.lines_of2 file |> iter @@ fun line ->
    match String.nsplit line "\t" with
    | [_; hood; _; message] when Re.execp knicks_re message ->
      Hashtbl.modify_def 0 hood succ table
    | _ -> () in
  table

let reduce tables =
  let table = Hashtbl.create 200 in
  let () = tables |> Array.iter @@ Hashtbl.iter @@ fun hood count ->
    Hashtbl.modify_def 0 hood (Int.add count) table in
  table

let compare (hood1, count1) (hood2, count2) =
  let cmp = Int.compare count2 count1 in
  if cmp = 0 then String.compare hood1 hood2 else cmp

let () =
  let files = Sys.readdir "../tmp/tweets" in
  let tables = Parmap.array_parmap mapper files in
  let results = reduce tables |> Hashtbl.enum |> Array.of_enum in
  Array.sort compare results;
  File.with_file_out "../tmp/ocaml_ouput" @@ fun file ->
    results |> Array.iter @@ fun (hood, count) ->
      Printf.fprintf file "%s\t%d\n" hood count
