(* Needs mtime and lwt.unix *)

open Lwt.Infix

let f1 dir =
  let s = Lwt_unix.files_of_directory dir in
  let s = Lwt_stream.filter (fun s -> s <> "." && s <> "..") s in
  let s = Lwt_stream.map (Filename.concat dir) s in
  Lwt_stream.to_list s

let f2 dir =
  let d = Sys.readdir dir in
  let d = Array.to_list d in
  let d = List.rev_map (Filename.concat dir) d in
  Lwt.return d

let rec repeat n f x = match n with
  | 0 -> Lwt.return_unit
  | n -> f x >>= fun _ -> repeat (n-1) f x

let print_time msg c =
  Printf.printf "%s: %f.02s\n%!" msg (Mtime.to_s @@ Mtime.count c)

let random_letter () = Char.(chr @@ code 'a' + Random.int 26)

let random_filename () =
  Bytes.init (1 + Random.int 20) (fun _ -> random_letter ())
  |> Bytes.to_string

let (/) = Filename.concat

let tmpdir = Filename.get_temp_dir_name () / "readir-bench"

let mkdir d =
  let perm = 0o0700 in
  try Unix.mkdir d perm
  with
  | Unix.Unix_error (Unix.EEXIST, "mkdir", _) -> ()

let write f d =
  let f = tmpdir / f in
  mkdir (Filename.dirname f);
  let oc = open_out f in
  output_string oc d;
  close_out oc

let prepare_fs n =
  let fs = Array.init n (fun i -> random_filename (), string_of_int i) in
  Array.iter (fun (k, v) -> write k v) fs

let () =
  prepare_fs 5000;
  let c = Mtime.counter () in
  Lwt_main.run (repeat 500 f1 tmpdir);
  print_time "Lwt_unix.files_of_directory" c;
  let c = Mtime.counter () in
  Lwt_main.run (repeat 500 f2 tmpdir);
  print_time "Unix.readdir" c
