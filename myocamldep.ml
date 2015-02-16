(************************************************************************)
(* Script requirements.							*)
(************************************************************************)

#use "topfind";;
#require "unix";;


(************************************************************************)
(* String utils.							*)
(************************************************************************)

module String =
struct
	include String

	let split ~sep str =			(* Note: sep is a char *)
		let len = length str in
		let idx = index str sep in
		(sub str 0 idx, sub str (idx+1) (len - idx - 1))

	let rsplit ~sep str =			(* Note: sep is a char *)
		let len = length str in
		let idx = rindex str sep in
		(sub str 0 idx, sub str (idx+1) (len - idx - 1))

	let nsplit ~sep str =			(* Note: sep is a char *)
		let len = length str in
		let prepend accum last_pos pos =
			if last_pos - pos > 1
			then sub str (pos+1) (last_pos - pos - 1) :: accum
			else accum in
		let rec loop accum last_pos pos =
			if pos < 0
			then prepend accum last_pos pos
			else if str.[pos] = sep
			then loop (prepend accum last_pos pos) pos (pos-1)
			else loop accum last_pos (pos-1) in
		loop [] len (len-1)
end


(************************************************************************)
(* Module definitions							*)
(************************************************************************)

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
	let (prodpath, xdeps) = String.split ~sep:':' (get_output argv.(0) argv) in
	let deps = String.nsplit ~sep:' ' (String.trim xdeps) in
	let prod = String.split ~sep:'.' (String.rsplit ~sep:'/' prodpath |> snd) |> fst in
	if Alias_map.mem prod dict
	then
		Printf.printf "%s:\n" prodpath
	else
		let (prefix, name) = String.rsplit ~sep:'_' prod in		(* We assume there's an underscore in the prod *)
		let set = Alias_map.find prefix dict in
		let transform dep =
			let dep' = String.lowercase_ascii dep in
			if Alias_set.mem dep' set
			then String.capitalize_ascii (prefix ^ "_" ^ dep')
			else begin
				try
					let idx = String.index dep' '.' in
					let ante = String.capitalize_ascii (String.sub dep' 0 idx) in
					let post = String.sub dep' (idx+1) (String.length dep' - idx - 1) in
					ante ^ "_" ^ post
				with
					Not_found -> dep
			end in
		let deps' = List.map transform deps in
		Printf.printf "%s: %s\n" prodpath (String.concat " " deps')

