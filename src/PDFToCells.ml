open Batteries
open Enum.Infix



let is_sorted list =
	List.sort Pervasives.compare list = list (* TODO faster *)

let max_length strings =
	strings
	|> Array.map String.length
	|> Array.reduce max

(* `ensure_rectangle strings` ensures that all strings in an array have the same length so that it can be considered as a matrix *)

let ensure_rectangle strings =
	let l = max_length strings in
	strings
	|> Array.map (fun string ->
		string ^ (String.repeat " " (l - String.length string))
	)

type 'a cell = {
	x : int;
	y : int;
	content : 'a
}

(* `get_cells column_header_extractors text` does the following:
	1) Extract headers from the first line that contains at least one header (and check that no other line contains a header)
	2) For each line under the header line and for each extracted header
		2.a) Try to guess the word under the header for this line
		2.b) Check that words from the current line do not intersect
		2.c) Add the word to the enumeration of cells with its line and column index
*)
let get_cells column_headers_extractor text0 =
	let text = text0 |> ensure_rectangle in
	let lines_containing_headers =
		text
		|> Array.mapi (fun l line ->
			match column_headers_extractor line with
			| [] -> None
			| headers -> Some (l, headers)
		)
		|> Array.filter_map identity
	in
	let headers_line_index, headers =
		match lines_containing_headers with
		| [| |] -> failwith "Could not find any header!\n"
		| [| x |] -> x
		| _ -> failwith ("Several lines contain headers!\n")
	in
	(headers_line_index + 1) -- (Array.length text - 1)
	|> Enum.map (fun l ->
		let line = text.(l) in
		let cells =
			headers |> List.mapi (fun h (header_data, header) ->
				(h, (header_data, StringPart.set_string header line |> StringPart.normalize))
			)
		in
		if not (cells |> List.map (fun (_, (_, header)) -> header |> StringPart.get_clopen_indices |> (fun (i, j) -> [i; j])) |> List.concat |> is_sorted) then begin
			Printf.eprintf "Some words intersect in line: %s\n" (String.escaped line)
		end;
		cells
		|> List.enum
		|> Enum.map (fun (h, header_with_data) -> {
			y = l;
			x = h;
			content = header_with_data
		})
		|> Enum.map (fun cell ->
			let (header_data, header) = cell.content in
			{cell with content = (header_data, header |> StringPart.get_substring)}
		)
		|> Enum.filter (fun c -> c.content |> snd <> "")
	)
	|> Enum.concat
	|> List.of_enum
	|> List.sort (
		(fun a b ->
			let open BatOrd in
			match poly a.x b.x, poly a.y b.y with
			| Eq, c -> c
			| c, _ -> c
		) |> BatOrd.comp
	)
	|> List.group_consecutive (fun a b -> a.x = b.x)
	|> List.enum
	|> Enum.map List.enum
	
