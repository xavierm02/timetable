open Batteries
open Yojson
open PDFToCells
open TokenTypes

let get_remote_file url =
	url
	|> Printf.sprintf "wget -q -O - %s"
	|> Unix.open_process_in

(* Get links to all PDF files (one per week) *)

let link_regexp = Str.regexp_case_fold "<a href=\"\\([^\"]*.pdf\\)\">S[0-9]+</a>"

let get_links () =
	let string =
		"www.ens-lyon.fr/DI/?p=4630"
		|> get_remote_file
		|> IO.read_all
	in
	StringPart.supermatch link_regexp 1 string
	|> List.map (fun (_, substring_groups) ->
		match substring_groups with
		| [url] -> url |> StringPart.get_substring
		| _ -> assert false
	)

(* Get cells from the table represented in the PDF files *)

type day = {
	day_name: string;
	date: int;
	month: int;
	year: int
}

let year_regexp = Str.regexp_case_fold ".*\\([0-9][0-9][0-9][0-9]\\).*"

let get_year_guesser lines =
	let years =
		lines
		|> Array.to_list
		|> List.filter_map (fun line ->
			if Str.string_match year_regexp line 0 then begin
				let year =
					Str.matched_group 1 line
					|> int_of_string
				in
				Some year
			end else begin
				None
			end
		)
	in
	match years |> List.sort Pervasives.compare |> List.unique with
	| [] -> failwith "Could not find year."
	| [year] -> fun _ -> year
	| [year1; year2] -> begin
		if year1 + 1 <> year2 then begin
			Printf.sprintf "The two years %d and %d are too far apart." year1 year2 |> failwith
		end;
		fun month ->
			if month <= 7 then
				year2
			else
				year1
	end
	| _ -> failwith "Found more than two years"
	

let day_to_json day =
	`Assoc [
		("type", `String "day");
		("day_name", `String day.day_name);
		("date", `Int day.date);
		("month", `Int day.month);
		("year", `Int day.year)
	]

let header_regexp =
	[ "Lundi"; "Mardi"; "Mercredi"; "Jeudi"; "Vendredi" ]
	|> String.concat "\\|"
	|> Printf.sprintf "\\(%s\\) \\([0-9]+\\)/\\([0-9]+\\)"
	|> Str.regexp_case_fold

let column_headers_extractor source year_guesser line =
	StringPart.supermatch header_regexp 3 line
	|> List.map (fun (substring, substring_groups) ->
		match substring_groups with
		| [day_name; date; month] -> begin
			let month = month |> StringPart.get_substring |> int_of_string in
			let day = {
				day_name = day_name |> StringPart.get_substring;
				date = date |> StringPart.get_substring |> int_of_string;
				month = month;
				year = year_guesser month
			} in
			let string_part = 
				StringPart.of_clopen_indices line (
					substring |> StringPart.get_index,
					substring |> StringPart.get_last_index
				)
			in
			((day, source), string_part)
		end
		| _ -> assert false
		
	)
	

let pdf_to_cells pdf =
	let lines =
		pdf
		|> Printf.sprintf "wget -q -O - %s | pdftotext -layout - - | konwert utf8-isolatin1"
		|> Unix.open_process_in
		|> IO.lines_of
		|> Array.of_enum
	in
	let year_guesser = get_year_guesser lines in
	lines |> PDFToCells.get_cells (column_headers_extractor pdf year_guesser)

type event = {
	source: string option;
	day: day option;
	period: period option;
	course: course option;
	room: room option;
	teachers: teachers option;
	comments: string option
}

let empty_event = {
	source = None;
	day = None;
	period = None;
	course = None;
	room = None;
	teachers = None;
	comments = None
}

let empty_event_of_day day =
	{empty_event with day = day}

let event_to_json event : Yojson.json =
	let list =
		[
			("source", event.source |> Option.map (fun s -> `String s));
			("day", event.day |> Option.map day_to_json);
			("period", event.period |> Option.map Period.to_json);
			("course", event.course |> Option.map Course.to_json);
			("room", event.room |> Option.map Room.to_json);
			("teachers", event.teachers |> Option.map Teachers.to_json);
			("comments", event.comments |> Option.map (fun string -> `String string))
		]
		|> List.filter_map (function
			| (_, None) -> None
			| (key, Some value) -> Some (key, value)
		)
	in
	`Assoc list

let event_to_string event =
	event
	|> event_to_json
	|> Yojson.to_string

let cells_to_events cells =
	let events = ref [] in
	cells |> Enum.iter (fun column ->
		let event = ref empty_event in
		let push_event () =
			begin
				match !event.source with
				| None -> !event |> event_to_string |> Printf.sprintf "Pushing event that has no source: %s\n" |> failwith
				| Some _ -> ()
			end;
			begin
				match !event.day with
				| None -> !event |> event_to_string |> Printf.sprintf "Pushing event that has no day: %s\n" |> failwith
				| Some _ -> ()
			end;
			begin
				match !event.period with
				| None -> !event |> event_to_string |> Printf.eprintf "Pushing event that has no period: %s\n"
				| Some _ -> ()
			end;
			if
				let e = !event in
				e.course <> None
				|| e.room <> None
				|| e.teachers <> None
				|| e.comments <> None
			then begin
				events := !event :: !events;
				event := empty_event
			end
		in
		column |> Enum.iter (fun cell ->
			let (day, source), string = cell.content in
			let new_empty_event =
				{empty_event with day = Some day; source = Some source}
			in
			begin
				match (!event).source with
				| None -> event := {!event with source = Some source}
				| Some other_source -> assert (source = other_source)
			end;
			begin
				match (!event).day with
				| None -> event := {!event with day = Some day}
				| Some other_day -> assert (day = other_day)
			end;
			try
				match string |> of_string_wrapped with
				| Period p -> begin
					if (!event).period <> None then begin
						push_event ();
						event := new_empty_event
					end;
					event := {!event with period = Some p}
				end
				| Course c -> begin
					if (!event).course <> None then begin
						push_event ();
						event := new_empty_event
					end;
					event := {!event with course = Some c}
				end
				| Room r -> begin
					if (!event).room <> None then begin
						push_event ();
						event := new_empty_event
					end;
					event := {!event with room = Some r}
				end
				| Teachers t -> begin
					if (!event).teachers <> None then begin
						push_event ();
						event := new_empty_event
					end;
					event := {!event with teachers = Some t}
				end
			with
			| No_token_type_matched _ -> begin
				match (!event).comments with
				| None -> event := {!event with day = Some day; comments = Some string}
				| Some comment -> event := {!event with comments = Some (comment ^ "\n" ^ string)}
			end;
		);
		if !event <> empty_event then begin
			push_event ()
		end else ()
	);
	List.rev !events

let get_json () =
	get_links ()
	|> List.enum
	|> Enum.map pdf_to_cells
	|> Enum.concat
	|> cells_to_events
	|> List.map event_to_json
	|> (fun list -> `List list)

let _ =
	get_json () |> Yojson.to_string |> print_endline

