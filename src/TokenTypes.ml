open Batteries
open Tuple

(* Types *)

type time = {
	hours: int;
	minutes: int
}

type period = {
	from_time: time;
	to_time: time
}

type course = {
	number : int;
	name : string
}

type room = string

type teachers = string

type token =
	| Period of period
	| Course of course
	| Room of room
	| Teachers of teachers

(* Common to all token types *)

module type TOKEN_TYPE = sig
	type t
	
	val of_string : string -> t option
	val to_string : t -> string
	val to_json : t -> Yojson.json
	val wrap : t -> token
end

module type ENRICHED_TOKEN_TYPE = sig
	include TOKEN_TYPE
	
	val of_string_wrapped : string -> token option
end

module Enrich (T : TOKEN_TYPE) : ENRICHED_TOKEN_TYPE with type t = T.t = struct
	include T
	
	let of_string_wrapped string =
		string
		|> of_string
		|> Option.map wrap
end

(* Time *)

module Time = struct
	type t = time

	let to_string t =
		Printf.sprintf "%2dh%2d" t.hours t.minutes

	let to_json t =
		`Assoc [
			("type", `String "time");
			("hours", `Int t.hours);
			("minutes", `Int t.minutes)
		]
end

(* Period *)

module Period : ENRICHED_TOKEN_TYPE with type t = period = Enrich(struct
	type t = period

	let period_regexp = Str.regexp_case_fold "\\([0-9]?[0-9]\\)h\\([0-9][0-9]\\) ?à ?\\([0-9]?[0-9]\\)h\\([0-9][0-9]\\)"

	let of_string string =
	if Str.string_match period_regexp string 0 then begin
		let (fh, fm, th, tm) =
			(1, 2, 3, 4)
			|> Tuple4.mapn (fun i ->
				Str.matched_group i string
				|> int_of_string
			)
		in
		let open Time in
		Some {
			from_time = {
				hours = fh;
				minutes = fm
			};
			to_time = {
				hours = th;
				minutes = tm
			}
		}
	end else begin
		None
	end

	let to_string p =
		Printf.sprintf "%s à %s" (p.from_time |> Time.to_string) (p.to_time |> Time.to_string)

	let to_json p =
		`Assoc [
			("type", `String "period");
			("from_time", p.from_time |> Time.to_json);
			("to_time", p.to_time |> Time.to_json)
		]
	
	let wrap t =
		Period t
end)

(* Course *)

module Course : ENRICHED_TOKEN_TYPE with type t = course = Enrich(struct
	type t = course

	let course_regexp = Str.regexp_case_fold ".*CR *\\([0-9][0-9]\\).*"

	let of_string string =
		if Str.string_match course_regexp string 0 then begin
			Some {
				number = Str.matched_group 1 string |> int_of_string;
				name = string
			}
		end else begin
			None
		end

	let to_string c = c.name

	let to_json c = `Assoc [
		("type", `String "course");
		("number", `Int c.number);
		("name", `String c.name)
	]
	
	let wrap c =
		Course c
end)

(* Room *)

module Room : ENRICHED_TOKEN_TYPE with type t = room = Enrich(struct
	type t = room

	let room_regexp = Str.regexp_case_fold "\\(\\(Salle\\|Amphi\\|Lieu\\) .*\\)"

	let of_string string =
		if Str.string_match room_regexp string 0 then begin
			Some string
		end else begin
			None
		end

	let to_string r =
		r

	let to_json r =
		`String r
	
	let wrap r =
		Room r
end)

(* Teachers *)

module Teachers : ENRICHED_TOKEN_TYPE with type t = teachers = Enrich(struct
	type t = teachers

	let teachers_regexp = Str.regexp_case_fold ".*\\(Mohab\\|Safey\\|Din\\|Jean-Charles\\|Faugère\\|Anne\\|Benoit\\|Blerina\\|Sinaimeri\\|Marie-France\\|Sagot\\|Pascal\\|Degiovanni\\|Omar\\|Fawzi\\|Natacha\\|Portier\\|Matteo\\|Mio\\|Colin\\|Riba\\|Jean-Michel\\|Muller\\|Damien\\|Pous\\|Laurent\\|Théry\\|Benoit\\|Libert\\|Frédéric\\|Prost\\|Pascal\\|Koiran\\|Natacha\\|Portier\\|Eddy\\|Caron\\|Gilles\\|Fedak\\|Laure\\|Gonnord\\|David\\|Monniaux\\|Nicolas\\|Bonneel\\|Julie\\|Digne\\|Filippo\\|Bonchi\\|Daniel\\|Hirschkoff\\|Damien\\|Pous\\|Francis\\|Lazarus\\|Arnaud\\|Mesmay\\|Jean-Marie\\|Gorce\\|Samir\\|Perlaza\\|Christophe\\|Crespelle\\|Marton\\|Karsai\\|Christophe\\|Alias\\|Fabrice\\|Rastello\\|Nicolas\\|Trotignon\\|Stéphan\\|Thomassé\\).*"

	let of_string string =
		if Str.string_match teachers_regexp string 0 then begin
			Some string
		end else begin
			None
		end

	let to_string t =
		t

	let to_json t =
		`String t
	
	let wrap t =
		Teachers t
end)

(* Enrich all token types *)

let of_string_wrapped_functions = [
	Period.of_string_wrapped;
	Course.of_string_wrapped;
	Room.of_string_wrapped;
	Teachers.of_string_wrapped
]

exception No_token_type_matched of string
exception Several_token_types_matched of string

let of_string_wrapped string =
	let possible_token_types =
		of_string_wrapped_functions
		|> List.filter_map (fun f -> f string)
	in
	match possible_token_types with
	| [] -> raise (No_token_type_matched string)
	| [x] -> x
	| _ -> raise (Several_token_types_matched string)

let to_string = function
	| Period p -> Period.to_string p
	| Course c -> Course.to_string c
	| Room r -> Room.to_string r
	| Teachers t -> Teachers.to_string t

let to_json = function
	| Period p -> Period.to_json p
	| Course c -> Course.to_json c
	| Room r -> Room.to_json r
	| Teachers t -> Teachers.to_json t
