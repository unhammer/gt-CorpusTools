(*
   First use wget to spider the site, e.g.

   $ wget --spider --recursive --no-verbose --output-file=samediggi.map -R png,gif,jpg,jpeg,JPG,PNG,css,js,ico,ttf,woff,svg,SVG,mp3,wmv,mpg,eps -X "/Dahpahusat,/layout/set/print"  http://www.samediggi.no/

   which outputs a file samediggi.map. Then

   $ make sdc.native && ./sdc.native < samediggi.map

   Assumes input urls are sme

 *)
open Lwt

let err = Printf.eprintf
let fmt = Printf.printf
let sp = Printf.sprintf

let re_matches r s =
  try Re.get_all (Re.exec r s) with Not_found -> [| |]

let re_rem pat s =
  Re.replace_string pat ~by:"" s

module U8 = struct
  let map s f = (* Map f on the bytes that can be decoded, skip the rest *)
    let b = Buffer.create (Bytes.length s) in
    let e = Uutf.encoder `UTF_8 (`Buffer b) in
    ignore @@ Uutf.String.fold_utf_8 (fun a i c -> begin
          match c with
          | `Uchar u -> Uutf.encode e (`Uchar (f u))
          | _ -> `Ok
        end)
      `Ok s;
    ignore @@ Uutf.encode e `End;
    Buffer.to_bytes b

  let lower s =
    map s (fun u -> match Uucp.Case.Fold.fold u with `Uchars (l::_) -> l | _ -> u)
end

module Lang = struct
  type t = Lnob | Lsma | Lsme | Lsmj
  let langs = [ Lnob ; Lsma ; Lsme ; Lsmj ]
  let compare = compare
  let of_sdstring = function
    | "nor" -> Lnob
    | "lulesamisk" -> Lsmj
    | "sorsamisk" -> Lsma
    | "nordsamisk" -> Lsme
    | s -> raise (Failure ("Unknown language: "^s))

  let to_sdstring = function
    | Lnob -> "nor"
    | Lsmj -> "lulesamisk"
    | Lsma -> "sorsamisk"
    | Lsme -> "nordsamisk"

  let to_string = function
    | Lnob -> "nob"
    | Lsmj -> "smj"
    | Lsma -> "sma"
    | Lsme -> "sme"
end

module Redir = struct
  let bind_optlwt x f =
    match%lwt x with
    | Some y -> return (Some (f y))
    | None -> return None

  let print_lwt_string s =
    print_endline s;
    return ()

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
end

module LMap = Map.Make(Lang)
module UMap = Map.Make(String)
let umap_add_once k v m =
  if UMap.mem k m then m else UMap.add k v m

module Wgetparse = struct
  let wget_url =
    Re_perl.compile_pat
      "(?:[0-9-]+ [0-9:]+ URL:)(?:([^ ]+) \\[\\d+\\] -> \"([^ ]+)\"| ([^ ]+) \\d+ OK)"

  type wgetres = WSimple of string
               | WRedir of string * string
               | WUnknown of string

  let parse_wget line =
    match re_matches wget_url line with
    | [| _; u1; u2; "" |] -> WRedir (u1, u2)
    | [| _; ""; ""; url |] -> WSimple url
    | _ -> WUnknown line

end

module Sdcrawl = struct
  let switchlanguage =
    (* The last part needed to remove wget's clobber-numbering.
       Fortunately there seem to be no actual URL's ending in .[0-9]
       here. *)
    Re_perl.compile_pat "([^/]+)/switchlanguage/to/([^/.]+)(.*?)(?:[.][0-9]{1,2})?$"

  let lslash = Re_perl.compile_pat "^/+"
  let rslash = Re_perl.compile_pat "/+$"

  let get_switch u1 u2 =
    match re_matches switchlanguage u2 with
    | [| _; host; l1; path |] -> Some ("http://"^host^(re_rem rslash path),
                                       Lang.of_sdstring l1,
                                       re_rem rslash u1)
    | _ -> None

  let proc_line_map line map =
    match Wgetparse.parse_wget line with
    | Wgetparse.WRedir (u1, u2) -> begin
        match get_switch u1 u2 with
        | Some (u1, lang, u2) -> begin
            let map = umap_add_once u1 LMap.empty map in
            let lmap = UMap.find u1 map in
            if LMap.mem lang lmap then err "Warning: overwriting %s for lang %s\n" u1 (Lang.to_string lang);
            UMap.add u1 (LMap.add lang u2 lmap) map
          end
        | None -> umap_add_once u1 LMap.empty map (* Some other kind of redirect that we ignore here *)
      end
    | Wgetparse.WSimple url -> umap_add_once url LMap.empty map
    | Wgetparse.WUnknown line -> begin
        err "Couldn't parse: %s\n" line;
        map
      end

  let switchback u1 lang =
    let u = Uri.of_string u1 in
    Uri.with_path u (sp "/switchlanguage/to/%s%s" (Lang.to_sdstring lang) (Uri.path u))
    |> Uri.to_string

  let redo_redirects map =
    (* Any that are missing redirects, we try to download with the
       /switchlanguage/ to see if we get something useful -- not doing
       this currently since it seems the wget spidering got it all. *)
    let todo = UMap.fold (fun u1 lmap acc ->
        print_endline u1;LMap.iter (fun l u2 -> fmt "%s %s %s\n" u1 (Lang.to_string l) u2) lmap;
        Lang.langs |> List.fold_left (fun acc lang ->
            if LMap.mem lang lmap then
              acc
            else
              (u1, lang, switchback u1 lang) :: acc
          ) acc
      ) map [] in
    let%lwt redirected = todo |> Lwt_list.map_s (fun (u1, lang, uswitch) -> Redir.location_of_string uswitch) in
    let redirects = List.combine todo redirected
                    |> List.filter (fun ((u1,l,_),res) ->
                        match res with
                        | None -> false
                        | Some u2 ->
                          let u1i = Uri.of_string u1 in
                          let u2i = Uri.of_string u2 in
                          Uri.path u1i != Uri.path u2i)
    in
    begin
      List.iter (fun ((u1,lang, uswitch), res) ->
          match res with | None -> fmt "%s → %s → %s = None" u1 (Lang.to_string lang) uswitch
                         | Some res -> fmt "%s → %s → %s = %s\n" u1 (Lang.to_string lang) uswitch res) redirects;
      return redirects
    end

  let avoid_in_paths = Re_perl.compile_pat "[][{}<>%$^~`#\"'()\\/:?*]"
  let initial_slash = Re_perl.compile_pat "[][{}<>%$^~`#\"'()\\/:?*]"
  let name_of url =
    Uri.of_string url
    |> Uri.path
    |> re_rem lslash
    |> (function "" -> "index" | u -> u)
    |> Re.replace_string avoid_in_paths ~by:"_" ~all:true
    |> (fun s -> s^".html")
    |> U8.lower

  let quote_list l = List.map (fun s -> "'"^s^"'") l |> String.concat " "

  let add_cmd name lang url =
    sp "add_files_to_corpus --rename \"%s\" . %s admin/sd/samediggi.no '%s'\n" name (Lang.to_string lang) url

  let proc_stream () =
    let input = Lwt_io.read_lines (Lwt_io.of_fd Lwt_io.input Lwt_unix.stdin) in
    let%lwt map = Lwt_stream.fold_s (fun l m -> return (proc_line_map l m)) input UMap.empty in
    begin
      UMap.iter (fun u1 lmap ->
          if LMap.is_empty lmap then () else begin
            let n1 = name_of u1 in
            print_string (add_cmd n1 Lang.Lsme u1);
            let names = [n1] |> LMap.fold (fun l2 u2 names ->
                let n2 = name_of u2 in
                print_string (add_cmd n2 l2 u2);
                n2::names) lmap
            in
            (* TODO: names need orig/lang/genre/ prefixed *)
            fmt "rogganreaiddut/para-all.sh %s\n" @@ quote_list names
          end) map;
      return ()
    end

  let examples = [ "http://www.samediggi.no/switchlanguage/to/lulesamisk/Biras-areala-ja-kultursuodjaleapmi/Kulturmuittut/Sami-kulturmuittut-min-arbi-boahtteaigai";
                   "http://www.samediggi.no/";
                 ]
  let proc_examples () =
    let%lwt results = Lwt_list.map_s Redir.location_of_string examples in
    List.iter (function Some r -> print_endline r | None -> ()) results;
    return ()
end

let () =
  Lwt_main.run @@ Sdcrawl.proc_stream ()
