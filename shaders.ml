(****************************************************************************)
(* MLGles3: OpenGL ES 3.0 interface for Objective Caml                      *)
(*                                                                          *)
(* Copyright (C) 2016   Christophe Raffalli <raffalli@univ-savoie.fr>       *)
(*                                                                          *)
(* MLGles3 is free software: you can redistribute it and/or modify it under *)
(* the terms of the  GNU Lesser General Public License  as published by the *)
(* Free Software Foundation,  either version 3 of the License,  or (at your *)
(* option) any later version.                                               *)
(*                                                                          *)
(* MLGles3 is distributed  in the hope that it will be useful,  but WITHOUT *)
(* ANY WARRANTY;  without even  the implied warranty of MERCHANTABILITY  or *)
(* FITNESS  FOR  A PARTICULAR PURPOSE.  See the  GNU  Lesser General Public *)
(* License for more details.                                                *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with MLGles3.  If not, see <http://www.gnu.org/licenses/>.         *)
(****************************************************************************)
(* shaders.ml: interface of Gles3 library                                   *)
(****************************************************************************)

open Gles3
open Buffers
open Textures

let split_lines str =
  let len = String.length str in
  let res = ref [] in
  let pos = ref 0 in
  while !pos < len do
    let i = try String.index_from str !pos '\n' with Not_found -> len - 1 in
    let line = String.sub str !pos (i + 1 - !pos) in
    res := line :: !res;
    pos := i + 1
  done;
  Array.of_list (List.rev !res)

type shader = {
  name : string;
  ty : shader_type;
  src : string
}

let load_shader ty filename =
  let ch = open_in filename in
  let len = in_channel_length ch in
  let str = Bytes.make len ' ' in
  really_input ch str 0 len;
  { name = filename;
    ty;
    src = Bytes.unsafe_to_string str }

type fixed =
  { program : Gles3.program;
    init : unit -> unit; clean : unit -> unit }

type 'a program = {
  name : string;
  attributes : (string * int * attribute_type * int) list;
  uniforms : (string * int * uniform_type * int) list;
  fixed : fixed;
  value : fixed -> (unit -> unit) -> 'a;
}

let last_used_program = ref None

let use_program : fixed -> unit = fun prg ->
  begin
    match !last_used_program with
    | None ->
       last_used_program := Some prg;
       use_program prg.program;
       prg.init ()
    | Some p ->
       if p != prg then (
         p.clean ();
         last_used_program := Some prg;
         use_program prg.program;
         prg.init ())
  end

let clean : unit -> unit = fun () ->
  match !last_used_program with
  | None -> ()
  | Some p -> p.clean ();last_used_program := None


let error_replace (shaders:shader list) msg =
  let fn num filename =
    let r = Str.regexp (Printf.sprintf "%d:\\([0-9]+\\)(\\([0-9]+\\))" num) in
    let templ = Printf.sprintf "file: %s, line \\1, column \\2" filename in
    Str.global_replace r templ
  in
  snd (List.fold_left (fun (i,s) sh -> (i+1, try fn i (sh:shader).name s with _ -> s)) (1,msg) shaders)

exception Compile_error
let compile : ?version:string -> ?precision:string -> string * shader list -> unit program =
  fun ?(version="300 es") ?(precision="highp") (prgname, shaders) ->
  let prg = create_program () in
  let fragment_shaders, vertex_shaders = List.partition
    (fun s -> s.ty = `fragment_shader) shaders
  in
  let fn ty shaders =
    let header = Printf.sprintf "#version %s\nprecision %s float;\n" version precision in
    let shader_srcs = Array.of_list (header :: List.mapi (fun i s ->
      Printf.sprintf "#line 1 %d\n" (i+1) ^ s.src) shaders) in
    let shader = create_shader ty in
    let _ = shader_source shader shader_srcs in
    let _ = compile_shader shader in
    if not (get_shader_compile_status shader) then (
      Printf.eprintf "compilation of %s failed for shader:\n%s"
	prgname (error_replace shaders (get_shader_info_log shader));
    raise Compile_error
    );
    attach_shader prg shader;
    shader
  in
  let vertex_shader = fn `vertex_shader vertex_shaders in
  let fragment_shader = fn `fragment_shader fragment_shaders in

  link_program prg;
  if not (get_program_link_status prg) then (
    Printf.eprintf "link failed program %s:\n%s" prgname (get_program_info_log prg);
    raise Compile_error
  );
  let cmp (s1,_,_,_) (s2,_,_,_) = compare s1 s2 in
  let rec res = {
    fixed = {
      program = prg;
      init = (fun () -> ());
      clean = (fun () -> ());
    };
    name = prgname;
    attributes = List.sort cmp (get_active_attribs prg);
    uniforms = List.sort cmp (get_active_uniforms prg);
    value = (fun fixed cont -> use_program fixed; cont ())
  } in
  Gc.finalise (fun _ ->
    delete_program prg;
    delete_shader vertex_shader;
    delete_shader fragment_shader) res;
  res

let check_complete prg =
  if prg.attributes <> [] || prg.uniforms <> [] then begin
    List.iter (fun (name,_,_,_) ->
      Printf.eprintf "ERROR: Missing attribute %s for %s\n%!" name prg.name) prg.attributes;
    List.iter (fun (name,_,_,_) ->
      Printf.eprintf "ERROR: Missing uniform %s for %s\n%!" name prg.name) prg.uniforms;
    failwith "Missing attributes of uniform"
  end

let draw_arrays prg shape ?(first=0) count =
  check_complete prg;
  prg.value prg.fixed (fun () -> draw_arrays shape ~first:0 ~count)

let draw_ushort_elements prg shape a =
  check_complete prg;
  prg.value prg.fixed (fun () -> draw_ushort_elements shape ~count:(Bigarray.Genarray.dims a).(0) a)

let draw_ubyte_elements prg shape a =
  check_complete prg;
  prg.value prg.fixed (fun () -> draw_ubyte_elements shape ~count:(Bigarray.Genarray.dims a).(0) a)

let draw_uint_elements prg shape a =
  check_complete prg;
  prg.value prg.fixed (fun () -> draw_uint_elements shape ~count:(Bigarray.Genarray.dims a).(0) a)

let draw_buffer_elements prg shape (buf:'a element_buffer) =
  check_complete prg;
  prg.value prg.fixed (fun () ->
      bind_buffer `element_array_buffer buf.index ;
      draw_buffer_elements shape ~count:buf.size ~typ:buf.ty 0;
      bind_buffer `element_array_buffer null_buffer)

let assoc_rm name l =
  let rec fn acc = function
    | [] -> raise Not_found
    | (name',index,ty,size) as x::l ->
       if name = name' then ((ty,size,index), List.rev_append acc l) else fn (x::acc) l
  in fn [] l

let tysize = function
  | `int | `float | `bool -> 1
  | `int_vec2 | `float_vec2 | `bool_vec2 -> 2
  | `int_vec3 | `float_vec3 | `bool_vec3 -> 3
  | `int_vec4 | `float_vec4 | `bool_vec4 -> 4
  | `float_mat2 -> 4
  | `float_mat3 -> 9
  | `float_mat4 -> 16
  | `sampler_2d | `sampler_2d_shadow | `sampler_cube -> failwith "unsupported"

let (//) a b = if a mod b <> 0 then raise Exit; a/b

let gen_attr =
  fun (fn : index:int -> size:int -> ?norm:bool -> ?stride:int -> 'b -> unit)
      (prg : 'c program) ?(norm=false) ?(stride=0) (name : string) ->
    try
      let (ty,_size,index), attributes = assoc_rm name prg.attributes in
      let size = tysize ty in
      let protect f a =
        try f a
        with Failure s ->
	  failwith (Printf.sprintf "%s for %s in %s" s name prg.name)
      in
      let init () =
        protect (fun () -> enable_vertex_attrib_array index) ();
        prg.fixed.init ();
      in
      let clean () =
        protect (fun () -> disable_vertex_attrib_array index) ();
        prg.fixed.clean ()
      in
      let value fixed cont a =
        let cont () =
	  protect (fn ~index ~size ~norm ~stride) a;
          cont ()
        in
	prg.value fixed cont
      in
      { prg with fixed = { prg.fixed with init; clean }; attributes; value }
    with
      Not_found ->
	Printf.eprintf "WARNING: Useless attributes %s for %s\n%!" name prg.name;
	let value fixed cont a = prg.value fixed cont in
	{ prg with value }

let gen_cst_attr =
  fun (fn : index:int -> size:int -> ?norm:bool -> ?stride:int -> 'a)
      (prg : 'c program) ?(norm=false) ?(stride=0) (name : string) a ->
    try
      let (ty,_size,index), attributes = assoc_rm name prg.attributes in
      let size = tysize ty in
      let protect f a =
        try f a
        with Failure s ->
	  failwith (Printf.sprintf "%s for %s in %s" s name prg.name)
      in
      let clean () =
	protect (fun () -> disable_vertex_attrib_array index) ();
        prg.fixed.clean ();
      in
      let init () =
	protect (fun () ->
	   enable_vertex_attrib_array index;
	   fn ~index ~size ~norm ~stride a) ();
	prg.fixed.init ();
      in
      { prg with attributes; fixed = { prg.fixed with init; clean }}
    with
      Not_found ->
	Printf.eprintf "WARNING: Useless attributes %s for %s\n%!" name prg.name;
	prg

let byte_attr = fun prg -> gen_attr vertex_attrib_byte_pointer prg
let ubyte_attr = fun prg -> gen_attr vertex_attrib_ubyte_pointer prg
let short_attr = fun prg -> gen_attr vertex_attrib_short_pointer prg
let ushort_attr = fun prg -> gen_attr vertex_attrib_ushort_pointer prg
let uint_attr = fun prg -> gen_attr vertex_attrib_uint_pointer prg
let float_attr = fun prg -> gen_attr vertex_attrib_float_pointer prg

let byte_cst_attr = fun prg -> gen_cst_attr vertex_attrib_byte_pointer prg
let ubyte_cst_attr = fun prg -> gen_cst_attr vertex_attrib_ubyte_pointer prg
let short_cst_attr = fun prg -> gen_cst_attr vertex_attrib_short_pointer prg
let ushort_cst_attr = fun prg -> gen_cst_attr vertex_attrib_ushort_pointer prg
let uint_cst_attr = fun prg -> gen_cst_attr vertex_attrib_uint_pointer prg
let float_cst_attr = fun prg -> gen_cst_attr vertex_attrib_float_pointer prg
let buffer_cst_attr = fun prg ?(norm=false) ?(stride=0) name buffer ->
  gen_cst_attr (fun ~index ~size ?(norm=false) ?(stride=0) buf ->
		bind_buffer `array_buffer buf;
		vertex_attrib_buffer_pointer ~index ~size ~typ:buffer.Buffers.ty ~norm ~stride 0;
		bind_buffer `array_buffer null_buffer)
	       prg name ~norm ~stride buffer.index

let gen_uniform1 = fun ty (fn: loc:int -> 'a -> unit) prg name ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s\n%!" name prg.name;
      failwith "Bad type");
    let value fixed cont a =
      let cont () = fn ~loc:index a; prg.value fixed cont; cont () in
      prg.value fixed cont
    in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s\n%!" name prg.name;
      let value fixed cont a = prg.value fixed cont in
      { prg with value }

let gen_cst_uniform1 = fun ty (fn: loc:int -> 'a -> unit) prg name a ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
      failwith "Bad type");
    let init () = fn ~loc:index a; prg.fixed.init () in
    { prg with uniforms; fixed = { prg.fixed with init } }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      prg

let gen_uniform2 = fun ty (fn: loc:int -> 'a -> 'a -> unit) prg name ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s\n%!" name prg.name;
      failwith "Bad type");
    let value fixed cont a b =
      let cont () = fn ~loc:index a b; cont () in
      prg.value fixed cont
    in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s\n%!" name prg.name;
      let value fixed cont a b = prg.value fixed cont in
      { prg with value }

let gen_cst_uniform2 = fun ty (fn: loc:int -> 'a -> 'a -> unit) prg name a b ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
      failwith "Bad type");
    let init () = fn ~loc:index a b; prg.fixed.init () in
    { prg with uniforms; fixed = { prg.fixed with init } }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      prg

let gen_uniform3 = fun ty (fn: loc:int -> 'a -> 'a -> 'a -> unit) prg name ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s\n%!" name prg.name;
      failwith "Bad type");
    let value fixed cont a b c =
      let cont () = fn ~loc:index a b c; cont () in
      prg.value fixed cont in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s\n%!" name prg.name;
      let value fixed cont a b c = prg.value fixed cont in
      { prg with value }

let gen_cst_uniform3 = fun ty (fn: loc:int -> 'a -> 'a -> 'a -> unit) prg name a b c ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
      failwith "Bad type");
    let init () = fn ~loc:index a b c; prg.fixed.init () in
    { prg with uniforms; fixed = { prg.fixed with init }  }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      prg

let gen_uniform4 = fun ty (fn: loc:int -> 'a -> 'a -> 'a -> 'a -> unit) prg name ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s\n%!" name prg.name;
      failwith "Bad type");
    let value fixed cont a b c d =
      let cont () = fn ~loc:index a b c d; cont () in
      prg.value fixed cont in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s\n%!" name prg.name;
      let value fixed cont a b c d = prg.value fixed cont in
      { prg with value }

let gen_cst_uniform4 = fun ty (fn: loc:int -> 'a -> 'a -> 'a -> 'a -> unit) prg name a b c d ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
      failwith "Bad type");
    let init () = fn ~loc:index a b c d; prg.fixed.init () in
    { prg with uniforms; fixed = { prg.fixed with init } }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      prg

let int1_uniform prg name = gen_uniform1 `int uniform_1i prg name
let bool1_uniform prg name = gen_uniform1 `bool uniform_1b prg name
let float1_uniform prg name = gen_uniform1 `float uniform_1f prg name
let int2_uniform prg name = gen_uniform2 `int uniform_2i prg name
let bool2_uniform prg name = gen_uniform2 `bool uniform_2b prg name
let float2_uniform prg name = gen_uniform2 `float uniform_2f prg name
let int3_uniform prg name = gen_uniform3 `int uniform_3i prg name
let bool3_uniform prg name = gen_uniform3 `bool uniform_3b prg name
let float3_uniform prg name = gen_uniform3 `float uniform_3f prg name
let int4_uniform prg name = gen_uniform4 `int uniform_4i prg name
let bool4_uniform prg name = gen_uniform4 `bool uniform_4b prg name
let float4_uniform prg name = gen_uniform4 `float uniform_4f prg name

let int1_cst_uniform prg name a = gen_cst_uniform1 `int uniform_1i prg name a
let bool1_cst_uniform prg name a = gen_cst_uniform1 `bool uniform_1b prg name a
let float1_cst_uniform prg name a = gen_cst_uniform1 `float uniform_1f prg name a
let int2_cst_uniform prg name a b = gen_cst_uniform2 `int uniform_2i prg name a b
let bool2_cst_uniform prg name a b = gen_cst_uniform2 `bool uniform_2b prg name a b
let float2_cst_uniform prg name a b = gen_cst_uniform2 `float uniform_2f prg name a b
let int3_cst_uniform prg name a b c = gen_cst_uniform3 `int uniform_3i prg name a b c
let bool3_cst_uniform prg name a b c = gen_cst_uniform3 `bool uniform_3b prg name a b c
let float3_cst_uniform prg name a b c = gen_cst_uniform3 `float uniform_3f prg name a b c
let int4_cst_uniform prg name a b c d = gen_cst_uniform4 `int uniform_4i prg name a b c d
let bool4_cst_uniform prg name a b c d = gen_cst_uniform4 `bool uniform_4b prg name a b c d
let float4_cst_uniform prg name a b c d = gen_cst_uniform4 `float uniform_4f prg name a b c d


let gen_uniform = fun ty (fn: loc:int -> ?count:int -> 'a array -> unit) prg name ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s\n%!" name prg.name;
      failwith "Bad type");
    let size = tysize ty in
    let value fixed cont a =
      let cont () =
        let count = Array.length a / size in
        (try fn ~loc:index ~count a with Failure s ->
	   failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
        cont ()
      in
      prg.value fixed cont
    in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s\n%!" name prg.name;
      let value fixed cont a = prg.value fixed cont in
      { prg with value }

let gen_cst_uniform = fun ty (fn: loc:int -> ?count:int -> 'a array -> unit) prg name a ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
      failwith "Bad type");
    let size = tysize ty in
    let init () =
      let count = Array.length a / size in
      (try fn ~loc:index ~count a with Failure s ->
	failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
      prg.fixed.init ()
    in
    { prg with uniforms; fixed = { prg.fixed with init } }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      prg

let int1v_uniform = fun prg name -> gen_uniform `int uniform_1iv prg name
let int2v_uniform = fun prg name -> gen_uniform `int_vec2 uniform_2iv prg name
let int3v_uniform = fun prg name -> gen_uniform `int_vec3 uniform_3iv prg name
let int4v_uniform = fun prg name -> gen_uniform `int_vec4 uniform_4iv prg name
let bool1v_uniform = fun prg name -> gen_uniform `bool uniform_1bv prg name
let bool2v_uniform = fun prg name -> gen_uniform `bool_vec2 uniform_2bv prg name
let bool3v_uniform = fun prg name -> gen_uniform `bool_vec3 uniform_3bv prg name
let bool4v_uniform = fun prg name -> gen_uniform `bool_vec4 uniform_4bv prg name
let float1v_uniform = fun prg name -> gen_uniform `float uniform_1fv prg name
let float2v_uniform = fun prg name -> gen_uniform `float_vec2 uniform_2fv prg name
let float3v_uniform = fun prg name -> gen_uniform `float_vec3 uniform_3fv prg name
let float4v_uniform = fun prg name -> gen_uniform `float_vec4 uniform_4fv prg name

let int1v_cst_uniform = fun prg name -> gen_cst_uniform `int uniform_1iv prg name
let int2v_cst_uniform = fun prg name -> gen_cst_uniform `int_vec2 uniform_2iv prg name
let int3v_cst_uniform = fun prg name -> gen_cst_uniform `int_vec3 uniform_3iv prg name
let int4v_cst_uniform = fun prg name -> gen_cst_uniform `int_vec4 uniform_4iv prg name
let bool1v_cst_uniform = fun prg name -> gen_cst_uniform `bool uniform_1bv prg name
let bool2v_cst_uniform = fun prg name -> gen_cst_uniform `bool_vec2 uniform_2bv prg name
let bool3v_cst_uniform = fun prg name -> gen_cst_uniform `bool_vec3 uniform_3bv prg name
let bool4v_cst_uniform = fun prg name -> gen_cst_uniform `bool_vec4 uniform_4bv prg name
let float1v_cst_uniform = fun prg name -> gen_cst_uniform `float uniform_1fv prg name
let float2v_cst_uniform = fun prg name -> gen_cst_uniform `float_vec2 uniform_2fv prg name
let float3v_cst_uniform = fun prg name -> gen_cst_uniform `float_vec3 uniform_3fv prg name
let float4v_cst_uniform = fun prg name -> gen_cst_uniform `float_vec4 uniform_4fv prg name

let gen_mat_uniform = fun ty (fn: loc:int -> ?count:int -> ?transp:bool -> 'a array -> unit) prg ?(transp=false) name ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
      failwith "Bad type");
    let size = tysize ty in
    let value fixed cont a =
      let cont () =
        let count = Array.length a / size in
        (try fn ~loc:index ~count a;
         with Failure s ->
	   failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
        cont ()
      in
      prg.value fixed cont
    in

    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      let value fixed cont a = prg.value fixed cont in
      { prg with value }

let gen_cst_mat_uniform = fun ty (fn: loc:int -> ?count:int -> ?transp:bool -> 'a array -> unit) prg ?(transp=false)  name a ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
      failwith "Bad type");
    let size = tysize ty in
    let init () =
      let count = Array.length a / size in
      (try fn ~loc:index ~count a;
       with Failure s ->
	 failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
      prg.fixed.init ()
    in
    { prg with uniforms; fixed = { prg.fixed with init } }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      prg

let float_mat2_uniform = fun prg name -> gen_mat_uniform `float_mat2 uniform_matrix_2fv prg name
let float_mat3_uniform = fun prg name -> gen_mat_uniform `float_mat3 uniform_matrix_3fv prg name
let float_mat4_uniform = fun prg name -> gen_mat_uniform `float_mat4 uniform_matrix_4fv prg name

let float_mat2_cst_uniform = fun prg name -> gen_cst_mat_uniform `float_mat2 uniform_matrix_2fv prg name
let float_mat3_cst_uniform = fun prg name -> gen_cst_mat_uniform `float_mat3 uniform_matrix_3fv prg name
let float_mat4_cst_uniform = fun prg name -> gen_cst_mat_uniform `float_mat4 uniform_matrix_4fv prg name

let texture_2d_uniform =
  fun prg name ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    let ty = match ty' with
      | `sampler_2d -> `texture_2d
      | `sampler_2d_shadow -> `texture_2d
      | _ ->
	 Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
	failwith "Bad type"
    in
    let value fixed cont texture =
      let cont () =
        (try
	   active_texture texture.tex_index;
	   bind_texture ty texture.tex_index;
	   uniform_1i index (int_of_texture texture.tex_index)
         with Failure s ->
	   failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
        cont ()
      in
      prg.value fixed cont
    in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      let value fixed cont a = prg.value fixed cont in
      { prg with value }

let texture_2d_cst_uniform =
  fun prg name texture ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    let ty = match ty' with
      | `sampler_2d -> `texture_2d
      | `sampler_2d_shadow -> `texture_2d
      | _ ->
	 Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
	failwith "Bad type"
    in
    let init () =
      (try
	 active_texture texture.tex_index;
	 bind_texture ty texture.tex_index;
	 uniform_1i index (int_of_texture texture.tex_index)
       with Failure s ->
	 failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
      prg.fixed.init ()
    in
    { prg with uniforms; fixed = { prg.fixed with init } }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      prg
