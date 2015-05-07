(*
   First use wget to spider the site, e.g.

   $ wget --spider --recursive --no-verbose --output-file=samediggi.map -R png,gif,jpg,jpeg,JPG,PNG,css,js,ico,ttf,woff,svg,SVG,mp3,wmv,mpg,eps -X "/Dahpahusat,/layout/set/print"  http://www.samediggi.no/

   which outputs a file samediggi.map. Then

   $ make sdc.native && ./sdc.native < samediggi.map

 *)
open Lwt

let fmt = Printf.printf

let bind_optlwt x f =
  match%lwt x with
  | Some y -> return (Some (f y))
  | None -> return None

let print_lwt_string s =
  print_endline s;
  Lwt.return ()


let get_location url =
  let%lwt r,_ = Cohttp_lwt_unix.Client.get url in
  let code = Cohttp.Response.status r in
  match code with
  | `Found -> Cohttp.Response.headers r |> Cohttp.Header.get_location |> return
  | _ -> begin
      Printf.printf "# %s for %s\n" (Cohttp.Code.string_of_status @@ code) (Uri.to_string url);
      return None
    end

let location_of_string s =
  bind_optlwt (Uri.of_string s |> get_location) (fun (url) -> Uri.to_string url)

let wget_url =
  Re_perl.compile_pat
    "(?:[0-9-]+ [0-9:]+ URL:)(?:([^ ]+) \\[\\d+\\] -> \"([^ ]+)\"|([^ ]+) \\d+ OK)"

let switchlanguage =
  (* The last part needed to remove wget's clobber-numbering.
     Fortunately there seem to be no actual URL's ending in .[0-9]
     here. *)
  Re_perl.compile_pat "([^/]+)/switchlanguage/to/([^/.]+)(.*?)(?:[.][0-9]{1,2})?$"

let rstrip_slash url =
  Re.replace_string (Re_perl.compile_pat "/+$") ~by:"" url

type lang = Lnob | Lsma | Lsme | Lsmj
let lang_of_string = function
  | "nor" -> Lnob
  | "lulesamisk" -> Lsmj
  | "sorsamisk" -> Lsma
  | "nordsamisk" -> Lsme
  | s -> raise (Failure ("Unknown language: "^s))

let string_of_lang = function
  | Lnob -> "nob"
  | Lsmj -> "smj"
  | Lsma -> "sma"
  | Lsme -> "sme"

type wgetres = WSimple of string
             | WRedir of string * lang * string
             | WUnknown of string

let re_matches r s =
  try Re.get_all (Re.exec r s) with Not_found -> [| |]

let parse_wget line =
  match re_matches wget_url line with
  | [| _; u1; u2; "" |] ->
    begin
      match re_matches switchlanguage u2 with
      | [| _; host; l1; path |] -> WRedir ("http://"^host^(rstrip_slash path),
                                           lang_of_string l1,
                                           rstrip_slash u1)
      | [| |] -> WSimple u1
      | _ -> WUnknown line
    end
  | [| _; ""; ""; url |] -> WSimple url
  | _ -> WUnknown line

module SMap = Map.Make(struct type t = string * lang option let compare = compare end)

let proc_line_map line map =
  match parse_wget line with
  | WRedir (u1, lang, u2) -> begin
      if SMap.mem (u1, Some lang) map then fmt "Warning: overwriting %s %s\n" u1 (string_of_lang lang);
      SMap.add (u1, Some lang) u2 map
    end
  | WSimple url -> SMap.add (url, None) url map
  | WUnknown line -> begin
      fmt "Couldn't parse: %s\n" line;
      map
    end

let proc_stream ()=
  let input = Lwt_io.read_lines (Lwt_io.of_fd Lwt_io.input Lwt_unix.stdin) in
  let%lwt map = Lwt_stream.fold_s (fun l m -> return (proc_line_map l m)) input SMap.empty in
  begin
    (* TODO: if no language, get location_of_string for each language *)
    SMap.iter (fun (u,l) v -> print_endline @@ u^" : "^v) map;
    return ()
  end


let examples = [ "http://www.samediggi.no/switchlanguage/to/lulesamisk/Biras-areala-ja-kultursuodjaleapmi/Kulturmuittut/Sami-kulturmuittut-min-arbi-boahtteaigai";
                 "http://www.samediggi.no/";
               ]
let proc_examples () =
  let%lwt results = Lwt_list.map_s location_of_string examples in
  List.iter (function Some r -> print_endline r | None -> ()) results;
  return ()


let () =
  Lwt_main.run @@ proc_stream ()
