open FSTypes2
open Printf
open Castle

type command = {
	name: string;
	f: connection -> string list -> unit;
	desc: string;
	params: string list;
}

let cmds : (string, command) Hashtbl.t = Hashtbl.create 10

(** show keys/values in ASCII, rather than hex bytes *)
let ascii_mode = ref true

(** Add some helper functions. *)
module String = struct
	include String

	let startswith phrase s =
		let len = length phrase in
		if length s < len
		then false
		else sub s 0 len = phrase

	(** Splits string s on the character c, returning only strings of
	    length >= 0 (NB!) and not containing c. *)
	let split_include_empty s c =
		let result = ref [] in
		let start = ref ((String.length s - 1)) in
		for i = String.length s - 1 downto 0 do
			if s.[i] = c then
			begin
				result := (String.sub s (i + 1) (!start - i))::!result;
				start := i-1
			end
		done;
		result := (String.sub s 0 (!start + 1))::!result;
		!result

	(** Splits string s on the characters cs, returning only strings of
	    length > 0 and not containing c. *)
	let split_any s cs =
		let result = ref [] in
		let start = ref (-1) in
		for i = String.length s - 1 downto 0 do
			if List.mem s.[i] cs
			then begin
				if !start > -1
				then begin
					result := (String.sub s (i + 1) (!start - i))::!result;
					start := -1
				end
			end else begin
				if !start = -1
				then start := i
			end
		done;
		if !start >= 0 then result := (String.sub s 0 (!start + 1))::!result;
		!result
end

let parse_hex_dimension s =
	if s = "" then
		""
	else begin
		if not (String.startswith "0x" s) then failwith "In hex mode, expected dimension of form 0x123456...";
		let len = String.length s in
		let s = String.sub s 2 (len - 2) in
		(* For strings containing odd numbers of characters, prepend a '0'. *)
		let s = if String.length s mod 2 = 0 then s else "0" ^ s in
		let len = String.length s in
		let buf = Buffer.create (len / 2) in
		for i = 0 to (len - 2) / 2 do
			let pair = String.sub s (2 * i) 2 in
			(* for OCaml's crappy parsing *)
			let pair = "0x" ^ pair in
			let pair_val = int_of_string pair in
			Buffer.add_char buf (Char.chr pair_val)
		done;
		Buffer.contents buf
	end

(** the opposite of String.escaped, after suggestion by Stefano Zacchiroli on ocaml-lib-devel *)
let unescape s =
	let lexer = lazy (Genlex.make_lexer []) in
		let tok_stream = Lazy.force lexer (Stream.of_string ("\"" ^ s ^ "\"")) in
			match Stream.peek tok_stream with
				| Some (Genlex.String s) -> s
				| _ -> assert false

let str_to_obj_key s =
	let guidance = "Keys must be given as '[dim0,dim1,...]'. An empty dim means an infinite bound." in
	let len = String.length s in
	if len < 2 || s.[0] <> '[' || s.[len - 1] <> ']' then
		failwith guidance
	else begin
		let s = String.sub s 1 (len - 2) in
		let elements = String.split_include_empty s ',' in
		Array.of_list (if !ascii_mode then List.map unescape elements else List.map parse_hex_dimension elements)
	end

(** force display of (escaped) chars in ascii mode; if false, just show '[x bytes]' if a key or value contains unprintable chars *)
let force_display = ref false

let sanitise s =
	let e = String.escaped s in
	if !force_display || e = s then
		(* Show the string, possibly with escaped binary chars. *)
		e
	else
		(* The string contained (escaped) binary chars. Instead just display the length of the string. *)
		sprintf "[%d bytes]" (String.length s)

(* produce two-character hex string for a char *)
let hex_of_char c = sprintf "%.2x" (Char.code c)

(* produce a string of space separated hex representations of a string's   *)
(* chars. Useful for debugging marshaling stuff.                           *)
let string_of_hex_buf ?(sep=true) s offset len =
    let newlen = len * (if sep then 3 else 2) in
    let buf = Buffer.create newlen in
    for i = 0 to len - 1 do
        let hl = hex_of_char s.[offset + i] in
        Buffer.add_string buf hl;
        if sep then Buffer.add_char buf ' '
    done;
    Buffer.contents buf

(* variant of byte_array_to_string in which leading zeroes are not culled, *)
(* and in which the 0x0 is missing *)
let string_of_hex_bytes str = string_of_hex_buf ~sep:false str 0 (String.length str)

let string_of_objvalue v =
	match v with 
		| Tombstone -> "<Tombstone>"
		| Value v -> if !ascii_mode then sanitise v else "0x" ^ string_of_hex_bytes v

(** string_of_list f arr produces the string "[x_0; x_1; x_2]", where xi is the
    result of applying f to the ith element of arr. Optional param ~sep changes
    the separator (default "; ") and ~ends allows you to disable the outer "[]".
*)
let string_of_list ?(sep = "; ") ?(ends=true) string_of_x x_list =
	let buf = Buffer.create 16 in
	if ends then Buffer.add_char buf '[';
	let rec f = function
		| [] ->
			()
		| elt::[] ->
			Buffer.add_string buf (string_of_x elt);
		| elt::next::rest ->
			Buffer.add_string buf (string_of_x elt);
			Buffer.add_string buf sep;
			f (next::rest)
	in
	f x_list;
	if ends then Buffer.add_char buf ']';
	Buffer.contents buf

let obj_key_to_string k =
	if !ascii_mode then
		string_of_list ~sep:"," sanitise (Array.to_list k)
	else
		string_of_list ~sep:"," (fun s -> "0x" ^ string_of_hex_bytes s) (Array.to_list k)

let str_to_val s =
    if !ascii_mode then unescape s else parse_hex_dimension s

let string_of_kvs kvps = 
	string_of_list (fun (k,v) -> sprintf "%s=%s" (obj_key_to_string k) (string_of_objvalue v)) (Array.to_list kvps)

let command name f ~desc ~params =
	Hashtbl.add cmds name {name=name; f=f; desc=desc; params=params}

exception Bad_arguments

let _ =
	command "get" (fun conn -> function 
		| [c; k] -> 
			let value = get conn (Int32.of_string c) (str_to_obj_key k) in
				print_endline (string_of_objvalue value)
		| _ -> raise Bad_arguments)
		~desc:"Get a value for the given key in the given collection"
		~params:["collection"; "key"];
		
	command "get_slice" (fun conn -> function
		| [c; k1; k2; limit] -> 
			let kvps = get_slice conn (Int32.of_string c) (str_to_obj_key k1) (str_to_obj_key k2) (int_of_string limit) in
				print_endline (string_of_kvs kvps);
				printf "%d result(s)\n" (Array.length kvps)
		| _ -> raise Bad_arguments)
		~desc:"Get keys and values in the given range in the given collection. The number of pairs retrieved will be <= limit; 0 means unlimited."
		~params:["collection"; "key"; "key"; "limit"];

	command "replace" (fun conn -> function 
		| [c; k; v] -> 
			replace conn (Int32.of_string c) (str_to_obj_key k) (str_to_val v)
		| _ -> raise Bad_arguments)
		~desc:"Replace a value for the given key in the given collection"
		~params:["collection_id:int"; "key"; "value:string"];

	command "delete" (fun conn -> function
		| [c; k] -> 
			remove conn (Int32.of_string c) (str_to_obj_key k)
		| _ -> raise Bad_arguments)
		~desc:"Delete any value for the given key in the given collection (i.e. replace with a Tombstone)"
		~params:["collection_id"; "key"];

	command "multi_replace" (fun conn -> function
		| c::kvps ->
			let rec f ?(acc=[]) = function
			| k::v::kvps -> f ~acc:((str_to_obj_key k, v)::acc) kvps
			| [] -> Array.of_list (List.rev acc)
			| _ -> failwith "Expected a list of space-separated key value pairs"
			in
				multi_replace conn (Int32.of_string c) (f kvps)
		| _ -> raise Bad_arguments)
		~desc:"Perform several replace operations at once"
		~params:["collection_id:int"; "key0"; "value0"; "..."; "keyN"; "valueN"];
		
	command "iter_start" (fun conn -> function
		| [c; start; finish] -> 
			let t = iter_start conn (Int32.of_string c) (str_to_obj_key start) (str_to_obj_key finish) in
				printf "token = %ld\n" t
		| _ -> raise Bad_arguments)
		~desc:"Create a new range query iterator"
		~params:["collection_id:int"; "key start"; "key finish"];

	command "iter_next" (fun conn -> function
		| [token; batch_size] ->
			let arr = iter_next conn (Int32.of_string token) (int_of_string batch_size) in
				print_endline (string_of_kvs arr);
				printf "%d result(s)\n" (Array.length arr)
		| _ -> raise Bad_arguments)
		~desc:"Return the next batch of key value pairs for an existing iterator"
		~params:["token:int"; "batch_size:int (must be >= 4096)"];

	command "iter_replace_last" (fun conn -> function
		| [t; i; v] -> 
			iter_replace_last conn (Int32.of_string t) (Int32.of_string i) v
		| _ -> raise Bad_arguments)
		~desc:"Replace the most recent item returned by an iterator"
		~params:["token:int"; "index:int"; "value:string"];

	command "iter_finish" (fun conn -> function
		| [t] ->
			iter_finish conn (Int32.of_string t)
		| _ -> raise Bad_arguments)
		~desc:"Discard state associated with an existing iterator"
		~params:["token:int"];

	command "claim" (fun conn -> function 
		| [dev] -> 
			let device = Int32.of_string dev in
			let disk = claim conn ~device in
				printf "0x%lx\n" disk
		| _ -> raise Bad_arguments)
		~desc:"Claims a device for the FS to use"
		~params:["device:int"];

	command "claim_dev" (fun conn -> function 
		| [dev] -> 
			let disk = claim_dev conn ~device:dev in
				printf "0x%lx\n" disk
		| _ -> raise Bad_arguments)
		~desc:"Claims a device for the FS to use"
		~params:["device:string"];

	command "release" (fun conn -> function 
		| [id] ->
			release conn ~disk:(Int32.of_string id)
		| _ -> raise Bad_arguments)
		~desc:"Releases a device from FS use"
		~params:["disk_id"];

	command "attach" (fun conn -> function 
		| [id] -> 
			let device = attach conn ~version:(Int32.of_string id) in
				printf "0x%lx\n" device
		| _ -> raise Bad_arguments)
		~desc:"Attach a version as a device"
		~params:["version_id"];

	command "attach_dev" (fun conn -> function 
		| [id] -> 
			let device = attach_dev conn ~version:(Int32.of_string id) in
				printf "%s\n" device
		| _ -> raise Bad_arguments)
		~desc:"Attach a version as a device"
		~params:["version_id"];

	command "detach" (fun conn -> function 
		| [id] -> detach conn ~device:(Int32.of_string id)
		| _ -> raise Bad_arguments)
		~desc:"Detaches a device"
		~params:["device_id"];

	command "detach_dev" (fun conn -> function 
		| [dev] -> detach_dev conn ~device:dev
		| _ -> raise Bad_arguments)
		~desc:"Detaches a device"
		~params:["device_name:string"];

	command "create" (fun conn -> function 
		| [size] ->
			let version = create conn ~size:(Int64.of_string size) in
				printf "0x%lx\n" version
		| _ -> raise Bad_arguments)
		~desc:"Create a new version"
		~params:["reserved (must be 0)"];

	(* TODO: deprecate *)
	command "destroy" (fun conn -> function
		| [id; "tree"] ->
			destroy_vertree conn ~vertree:(Int32.of_string id)
		| [id; "version"] ->
			delete_version conn ~version:(Int32.of_string id)
		| _ -> raise Bad_arguments)
		~desc:"Destroy a version or tree"
		~params:["id"; "tree|version"];

	command "destroy_vertree" (fun conn -> function
		| [id] -> destroy_vertree conn ~vertree:(Int32.of_string id) 
		| _ -> raise Bad_arguments)
		~desc:"Destroy a version or tree"
		~params:["id"];

	command "delete_version" (fun conn -> function
		| [id] -> delete_version conn ~version:(Int32.of_string id) 
		| _ -> raise Bad_arguments)
		~desc:"Destroy a version or tree"
		~params:["id"];

	command "clone" (fun conn -> function 
		| [id] -> 
			let version = clone conn ~version:(Int32.of_string id) in
				printf "0x%lx\n" version
		| _ -> raise Bad_arguments)
		~desc:"Clone the given version"
		~params:["version_id"];

	command "snapshot" (fun conn -> function 
		| [id] -> 
			let version = snapshot conn ~device:(Int32.of_string id) in
				printf "0x%lx\n" version
		| _ -> raise Bad_arguments)
		~desc:"Snapshot the given device"
		~params:["device_id"];

	command "snapshot_dev" (fun conn -> function 
		| [dev] -> 
			let version = snapshot_dev conn ~device:dev in
				printf "0x%lx\n" version
		| _ -> raise Bad_arguments)
		~desc:"Snapshot the given device"
		~params:["device_name:string"];

	command "init" (fun conn -> function 
		| [] ->
			init conn
		| _ -> raise Bad_arguments)
		~desc:"init the FS"
		~params:[];

	command "collection_attach" (fun conn -> function 
		| [version; name] -> 
			let collection = collection_attach conn ~version:(Int32.of_string version) ~name in
				printf "0x%lx\n" collection
		| _ -> raise Bad_arguments)
		~desc:"Attach the given version under the given name"
		~params:["version_id:int"; "name:string"];

	command "collection_detach" (fun conn -> function 
		| [id] ->
			collection_detach conn ~collection:(Int32.of_string id)
		| _ -> raise Bad_arguments)
		~desc:"Detach the given collection"
		~params:["collection_id"];

	command "collection_snapshot" (fun conn -> function 
		| [id] -> 
			let version = collection_take_snapshot conn ~collection:(Int32.of_string id) in
				printf "0x%lx\n" version
		| _ -> raise Bad_arguments)
		~desc:"Snapshot the given collection"
		~params:["collection_id"];

    command "fault" (fun conn -> function
        | [fault_id; fault_arg] ->
        	fault conn ~fault_id:(Int32.of_string fault_id) ~fault_arg:(Int32.of_string fault_arg)
		| _ -> raise Bad_arguments)
		~desc:"Inject fault into kernel. Look at castle_public.h for fault codes"
		~params:["fault_id:int"; "fault_arg:int"];

  command "slave_evacuate" (fun conn -> function 
    | [id] ->
	slave_evacuate conn ~disk:(Int32.of_string id) ~force:0l
    | [id; force] ->
	slave_evacuate conn ~disk:(Int32.of_string id) ~force:(Int32.of_string force)
    | _ -> raise Bad_arguments)
    ~desc:"Starts evacuation of a slave"
    ~params:["disk_id"; "force"];

    command "thread_priority" (fun conn -> function
        | [id] ->
        	thread_priority conn ~nice_value:(Int32.of_string id)
		| _ -> raise Bad_arguments)
		~desc:"Change nice value of all castle kernel threads."
		~params:["nice_value"];

    command "environment" (fun conn -> function
      | [name; data] ->
          let id = match name with
            "buildid" -> BuildId
          | "modulehash" -> ModuleHash
          | "description" -> Description
          | "hostname" -> Hostname
          | _ -> raise Bad_arguments
          in
          environment_set conn id data
      | _ -> raise Bad_arguments)
    ~desc:"Set environment variable in kernel"
    ~params:["name"; "value"];

    command "slave_scan" (fun conn -> function
        | [id] ->
        	slave_scan conn ~id:(Int32.of_string id)
		| _ -> raise Bad_arguments)
		~desc:"Start slave scan."
		~params:["id"]

let main () =
	let connection = Castle.connect "" 0 0 in

	let execute tokens =
		flush_all ();
		let cmd, args = match tokens with
			| cmd::args -> cmd, args
			| _ -> failwith "please enter a command!"
		in
		
		let the_command = 
			try Hashtbl.find cmds cmd 
			with Not_found -> failwith (sprintf "unknown command %s" cmd)
		in
			the_command.f connection args
	in

	let interactive () =
		print_endline "Interactive mode";
		let valid_cmds = Hashtbl.fold (fun k _ acc -> k::acc) cmds [] in
		let valid_cmds = List.sort compare valid_cmds in
		let valid_cmds = String.concat ", " valid_cmds in
		printf "Valid commands: %s\n" valid_cmds;
		let help_msg = "Try 'help <cmd>' for help.\nType 'ascii' to switch between hex/ASCII mode.\nType 'force' to force display of keys/values that contain unprintable characters, rather than just the length of the string." in
		print_endline help_msg;
		
		while true do
			try begin
				print_string "> ";
				let line = read_line () in
				let tokens = String.split_any line [' '; '\t'] in
				match tokens with
					| "help"::[] ->
						print_endline help_msg
					| "ascii"::[] ->
						ascii_mode := not !ascii_mode;
						print_endline ("ASCII mode " ^ if !ascii_mode then "enabled" else "disabled")
					| "force"::[] ->
						force_display := not !force_display;
						print_endline ("Display of unprintable chars " ^ if !force_display then "enabled" else "disabled")
					| "help"::cmd::[] ->
						let the_command = 
							try Hashtbl.find cmds cmd 
							with Not_found -> failwith (sprintf "help: unknown command %s" cmd)
						in
						print_endline the_command.name;
						print_endline the_command.desc;
						printf "Params: [%s]\n" (String.concat "; " the_command.params)
					| [] ->
						()
					| _ ->
						execute tokens
			end
			with
				| End_of_file -> exit 0
				| Unix.Unix_error (err_code, function_name, param) ->
					prerr_endline (sprintf "Error: %s during %s(%s)" (Unix.error_message err_code) function_name param)
				| e ->
					prerr_endline (Printexc.to_string e);
					prerr_endline (Printexc.get_backtrace ())
		done
	in
	
	if Array.length Sys.argv = 1
	then interactive ()
	else begin
		ascii_mode := false;
		let tokens = List.tl (Array.to_list Sys.argv) in
		let tokens = match tokens with
			| "--ascii"::tl -> ascii_mode := true; tl
			| x -> x
		in
		execute tokens
	end

let _ =
	Printexc.record_backtrace true;
	try
		main ()
	with
		| Unix.Unix_error (Unix.ENOPROTOOPT, _, _) ->
			prerr_endline "Protocol version mismatch between libcastle and castle-fs kernel module";
			exit 1
		| Unix.Unix_error (err_code, function_name, param) ->
		    prerr_endline (sprintf "Error: %s during %s(%s)" (Unix.error_message err_code) function_name param);
                    exit 1
		| e ->
			prerr_endline (Printexc.to_string e);
			prerr_endline (Printexc.get_backtrace ());
			exit 1

