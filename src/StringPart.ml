open Batteries
open Enum.Infix

type t = {
	string : string;
	index : int;
	length : int
}

(* Constructors *)

let of_index_length string index length = {
	string = string;
	index = index;
	length = length
}

let of_closed_indices string (i, j) =
	of_index_length string i (j - i + 1)

let of_clopen_indices string (i, j) =
	of_index_length string i (j - i)

let of_match string =
	of_clopen_indices
		string
		(Str.match_beginning (), Str.match_end ())

let of_match_group string group_index =
	of_clopen_indices
		string
		(Str.group_beginning group_index, Str.group_end group_index)

(* Getters *)

let get_index s =
	s.index

let get_last_index s =
	s.index + s.length - 1

let get_clopen_indices s =
	(s.index, s.index + s.length)

let get_string s =
	s.string

let get_substring s =
	String.sub s.string s.index s.length

(* Setters *)

let set_index s i =
	{s with index = i; length = s.index + s.length - i} (* index + length = new_index + new_length *)

let set_last_index s j =
	{s with length = j - s.index + 1}

let set_closed_indices s (i, j) =
	{s with index = i; length = j - i + 1}

let set_string s string =
	{s with string = string}

(* And the rest *)

let supermatch regexp number_of_groups string =
	let substrings = ref [] in
	string |> Str.global_substitute regexp (fun _ ->
		let substring = of_match string in
		let substring_groups =
			1 -- number_of_groups
			|> Enum.map (of_match_group string)
			|> List.of_enum
		in
		substrings := (substring, substring_groups) :: !substrings;
		""
	) |> ignore;
	List.rev !substrings

let is_space c =
	match c with
	| ' ' -> true
	| '\012' -> true
	| '\n' -> true
	| '\r' -> true
	| '\t' -> true
	| _ -> false

(* `trim_string string (i0, j0)` return the leftmost greatest substring of `string[i0..j0]` that neither starts nor ends with a space. *)
let trim substring =
	let i = ref (substring |> get_index) in
	let j = ref (substring |> get_last_index) in
	let string = substring |> get_string in
	while !i <= !j && is_space string.[!j] do
		j := !j - 1
	done;
	while !i <= !j && is_space string.[!i] do
		i := !i + 1
	done;
	set_closed_indices substring (!i, !j)

(* `expand_string string (i0, j0)` returns the greatest superstring of `string[i0..j0]` that doesn't contain two consecutive spaces (not counting spaces in string[i0..j0]). *)
let expand substring =
	let string = substring |> get_string in
	let i =
		let i = ref (substring |> get_index) in
		let continue = ref true in
		while !continue do
			if !i - 2 >= 0 && not (is_space string.[!i - 2]) then begin
				i := !i - 2
			end else if !i - 1 >= 0 && not (is_space string.[!i - 1]) then begin
				i := !i - 1
			end else begin
				continue := false
			end
		done;
		!i
	in
	let j =
		let j = ref (substring |> get_last_index) in
		let continue = ref true in
		let max_index = String.length string - 1 in
		while !continue do
			if !j + 2 <= max_index && not (is_space string.[!j + 2]) then begin
				j := !j + 2
			end else if !j + 1 <= max_index && not (is_space string.[!j + 1]) then begin
				j := !j + 1
			end else begin
				continue := false
			end
		done;
		!j
	in
	set_closed_indices substring (i, j)

(* `find_string_part string (i0, j0)` tries to find the content of the cell containing at least string[i0..j0]. *)
let normalize substring =
	substring
	|> trim
	|> expand
	|> trim
