(****************************************************************************)
(* MLGles2: OpenGL ES2 interface for Objective Caml                         *)
(*                                                                          *)
(* Copyright (C) 2014   Alexandre Miquel <amiquel@fing.edu.uy>              *)
(*                                                                          *)
(* MLGles2 is free software: you can redistribute it and/or modify it under *)
(* the terms of the  GNU Lesser General Public License  as published by the *)
(* Free Software Foundation,  either version 3 of the License,  or (at your *)
(* option) any later version.                                               *)
(*                                                                          *)
(* MLGles2 is distributed  in the hope that it will be useful,  but WITHOUT *)
(* ANY WARRANTY;  without even  the implied warranty of MERCHANTABILITY  or *)
(* FITNESS  FOR  A PARTICULAR PURPOSE.  See the  GNU  Lesser General Public *)
(* License for more details.                                                *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with MLGles2.  If not, see <http://www.gnu.org/licenses/>.         *)
(****************************************************************************)
(* maketags.ml: collecting ML polymorpic variant tags                       *)
(****************************************************************************)

(*** Excerpt from $(OCAML_SRC)/typing/btype.ml ***)

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let build_keys : string array -> int array =
  Array.map hash_variant

(****************************************************************************)
(*   PARSING                                                                *)
(****************************************************************************)

let iter_polymorphic_variants : (string -> unit) -> string -> unit =
  let iter_lines fn fname =
    let ic = open_in fname in
    try while true do fn (input_line ic) done
    with End_of_file -> close_in ic
  in
  let is_alpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') in
  let is_alpha_num c = is_alpha c || ('0' <= c && c <= '9') || c = '_' in
  let rec collect_from_line fn line =
    let len = String.length line in
    let gn i c =
      if c <> '`' || i+1 >= len || not (is_alpha line.[i+1]) then () else
      let l = ref 1 in
      while i + !l + 1 < len && is_alpha_num line.[i + !l + 1] do
        incr l
      done;
      fn (String.sub line (i+1) !l)
    in
    String.iteri gn line
  in
  fun add -> iter_lines (collect_from_line add)

let parse_variants fnames =
  let variants = ref [] in
  let add_variant s = variants := s :: !variants in
  List.iter (iter_polymorphic_variants add_variant) fnames;
  Array.of_list (List.sort_uniq String.compare !variants)

(****************************************************************************)
(*   GENERATING ENUM NAME                                                   *)
(****************************************************************************)

let special_names : (string * string) list =
  [ ("color_buffer"     , "GL_COLOR_BUFFER_BIT"  )
  ; ("depth_buffer"     , "GL_DEPTH_BUFFER_BIT"  )
  ; ("stencil_buffer"   , "GL_STENCIL_BUFFER_BIT")
  ; ("ubyte"            , "GL_UNSIGNED_BYTE"     )
  ; ("ushort"           , "GL_UNSIGNED_SHORT"    )
  ; ("uint"             , "GL_UNSIGNED_INT"      )
  ; ("texture_2d_shadow", "GL_TEXTURE_2D"        ) ]

let default_enum tag =
  "GL_" ^ String.uppercase_ascii tag

let enum_name : string -> string = fun v ->
  try List.assoc v special_names with Not_found -> default_enum v

let build_pairs : string array -> (string * string) array =
  Array.map (fun s -> (s, enum_name s))

(****************************************************************************)
(*   COMPUTING MASK                                                         *)
(****************************************************************************)

let tmp = Array.make 1000 false

let evaluate_params keys size mask =
  let num = Array.length keys in
  Array.fill tmp 0 size false ;
  let sum_probes = ref 0 in
  let max_probes = ref 0 in
  for i = 0 to num - 1 do
    let hash = ref ((keys.(i) lxor mask) mod size) in
    if !hash < 0 then hash := !hash + size ;
    let probes = ref 0 in
    while tmp.(!hash) do
      incr probes ; incr hash ;
      if !hash = size then hash := 0
    done ;
    tmp.(!hash) <- true ;
    sum_probes := !sum_probes + !probes ;
    if !probes > !max_probes then max_probes := !probes
  done ;
  (!max_probes, !sum_probes)

let compute_params keys size max_mask =
  let best_mask = ref (-1) in
  let best_mark = ref (max_int, max_int) in
  for mask = 0 to max_mask do
    Printf.eprintf "\rmask = 0x%X\r" mask ;
    flush stderr ;
    let mark = evaluate_params keys size mask in
    if mark < !best_mark then begin
      best_mark := mark ; best_mask := mask ;
    end
  done ;
  (!best_mask, !best_mark)

(****************************************************************************)
(*   OUTPUT GENERATED C-FILE                                                *)
(****************************************************************************)

let output_C_file ch pairs size mask =
  let num = Array.length pairs in
  for i = 0 to num - 1 do
    let (tag, enum) = pairs.(i) in
    let hash = hash_variant tag in
    Printf.fprintf ch "  { %11d, %s },\n" hash enum
  done ;
  Printf.fprintf ch "#define TAG_NUMBER   %d\n" num ;
  Printf.fprintf ch "#define TABLE_SIZE   %d\n" size ;
  Printf.fprintf ch "#define TABLE_MASK   0x%X\n" mask ;
  flush ch

(****************************************************************************)
(*   MAIN CALL                                                              *)
(****************************************************************************)

let _ =
  let fnames = List.tl (Array.to_list Sys.argv) in
  let variants = parse_variants fnames in
  let pairs = build_pairs variants in
  let keys = build_keys variants in
  let num = Array.length pairs in
  let size = 2 * num + 1 in
  let (mask, mark) = compute_params keys size 0xFFF in
  Format.eprintf "Found params: " ;
  Format.eprintf "num = %d, size = %d, mask = 0x%X@." num size mask ;
  let (max, sum) = mark in
  Format.eprintf "Probes: max = %d, sum = %d@." max sum ;
  output_C_file stdout pairs size mask
