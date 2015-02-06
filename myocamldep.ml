(************************************************************************)
(* Script requirements.							*)
(************************************************************************)

#use "topfind";;
#require "batteries";;
#require "unix";;


(************************************************************************)
(* Module definitions							*)
(************************************************************************)

module String = BatString

module Alias_set = Set.Make (String)

module Alias_map = Map.Make (String)


(************************************************************************)
(* Project-specific dictionary of module aliases.			*)
(************************************************************************)

let dict =
	Alias_map.empty |>
	Alias_map.add "foo" Alias_set.(empty |> add "a" |> add "b" |> add "c") |>
	Alias_map.add "bar" Alias_set.(empty |> add "a" |> add "b" |> add "c")


(************************************************************************)
(* Process management utils.						*)
(************************************************************************)

let get_output cmd argv =
	let open Unix in
	let (fd_in, fd_out) = pipe () in
	let _ = create_process cmd argv stdin fd_out stderr in
	let res = input_line (in_channel_of_descr fd_in) in
	ignore (wait ());
	res


(************************************************************************)
(* Main.								*)
(************************************************************************)

let () =
	let argv = Array.init (Array.length Sys.argv + 1)
		begin function
			| 0 -> "ocamlfind"
			| 1 -> "ocamldep"
			| n -> Sys.argv.(n-1)
		end in
	let (prodpath, xdeps) = String.split (get_output argv.(0) argv) ~by:":" in
	let deps = String.nsplit (String.trim xdeps) ~by:" " in
	let prod = String.split (String.rsplit prodpath ~by:"/" |> snd) ~by:"." |> fst in
	if Alias_map.mem prod dict
	then
		Printf.printf "%s:\n" prodpath
	else
		let (prefix, name) = String.rsplit prod ~by:"_" in		(* We assume there's an underscore in the prod *)
		let set = Alias_map.find prefix dict in
		let transform dep =
			let dep' = String.lowercase dep in
			if Alias_set.mem dep' set
			then String.capitalize (prefix ^ "_" ^ dep')
			else dep in
		let deps' = List.map transform deps in
		Printf.printf "%s: %s\n" prodpath (String.join " " deps')

