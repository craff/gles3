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

type 'a program = {
  name : string;
  program : Gles3.program;
  attributes : (string * attribute_type * int * int) list;
  uniforms : (string * uniform_type * int * int) list;
  value : (unit -> unit) -> 'a;
}

let number l =
  let i = ref 0 in
  List.map(fun (x,y,z) -> let j = !i in incr i; (x,y,z,j)) l

let error_replace (shaders:shader list) msg =
  let fn num filename =
    let r = Str.regexp (Printf.sprintf "%d:\\([0-9]+\\)(\\([0-9]+\\))" num) in
    let templ = Printf.sprintf "file: %s, line \\1, column \\2" filename in
    Str.global_replace r templ
  in
  snd (List.fold_left (fun (i,s) sh -> (i+1, fn i (sh:shader).name s)) (1,msg) shaders)

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
  let res = {
    program = prg;
    name = prgname;
    attributes = List.sort cmp (number (get_active_attribs prg));
    uniforms = List.sort cmp (number (get_active_uniforms prg));
    value = (fun f -> f ()) ;
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
  check_complete prg; use_program prg.program;
  prg.value (fun () -> draw_arrays shape ~first:0 ~count)

let draw_ushort_elements prg shape a =
  check_complete prg; use_program prg.program;
  prg.value (fun () -> draw_ushort_elements shape ~count:(Bigarray.Array1.dim a) a)

let draw_ubyte_elements prg shape a =
  check_complete prg; use_program prg.program;
  prg.value (fun () -> draw_ubyte_elements shape ~count:(Bigarray.Array1.dim a) a)

let draw_uint_elements prg shape a =
  check_complete prg; use_program prg.program;
  prg.value (fun () -> draw_uint_elements shape ~count:(Bigarray.Array1.dim a) a)

let draw_buffer_elements prg shape (buf:'a element_buffer) =
  check_complete prg; use_program prg.program;
  prg.value (fun () ->
    bind_buffer `element_array_buffer buf.index ;
    draw_buffer_elements shape ~count:buf.size ~typ:buf.ty 0)

let assoc_rm name l =
  let rec fn acc = function
    | [] -> raise Not_found
    | (name',ty,size,index) as x::l ->
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
  fun (fn : index:int -> size:int -> ?norm:bool -> ?stride:int -> 'b -> 'a)
      (prg : 'c program) ?(norm=false) ?(stride=0) (name : string) ->
    try
      let (ty,_size,index), attributes = assoc_rm name prg.attributes in
      let size = tysize ty in
      let value cont a =
	let cont () =
	  cont ();
	  disable_vertex_attrib_array index;
	in
	(try
	   enable_vertex_attrib_array index;
	   fn ~index ~size ~norm ~stride a;
	 with Failure s ->
	   failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
	prg.value cont
      in
      { prg with attributes; value }
    with
      Not_found ->
	Printf.eprintf "WARNING: Useless attributes %s for %s\n%!" name prg.name;
	let value cont a = prg.value cont in
	{ prg with value }

let gen_cst_attr =
  fun (fn : index:int -> size:int -> ?norm:bool -> ?stride:int -> 'a)
      (prg : 'c program) ?(norm=false) ?(stride=0) (name : string) a ->
    try
      let (ty,_size,index), attributes = assoc_rm name prg.attributes in
      let size = tysize ty in
      let value cont =
	let cont () =
	  cont ();
	  disable_vertex_attrib_array index;
	in
	(try
	   enable_vertex_attrib_array index;
	   fn ~index ~size ~norm ~stride a;
	 with Failure s ->
	   failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
	prg.value cont
      in
      { prg with attributes; value }
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
    vertex_attrib_buffer_pointer ~index ~size ~typ:buffer.Buffers.ty ~norm ~stride 0)
    prg name ~norm ~stride buffer.index

let gen_uniform = fun ty (fn: loc:int -> ?count:int -> 'a array -> unit) prg name ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s\n%!" name prg.name;
      failwith "Bad type");
    let size = tysize ty in
    let value cont a =
      let count = Array.length a / size in
      (try fn ~loc:index ~count a with Failure s ->
	failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
      prg.value cont
    in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s\n%!" name prg.name;
      let value cont a = prg.value cont in
      { prg with value }

let gen_cst_uniform = fun ty (fn: loc:int -> ?count:int -> 'a array -> unit) prg name a ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
      failwith "Bad type");
    let size = tysize ty in
    let value cont =
      let count = Array.length a / size in
      (try fn ~loc:index ~count a with Failure s ->
	failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
      prg.value cont
    in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      prg

let int_uniform = fun prg name -> gen_uniform `int uniform_1iv prg name
let int2_uniform = fun prg name -> gen_uniform `int_vec2 uniform_2iv prg name
let int3_uniform = fun prg name -> gen_uniform `int_vec3 uniform_3iv prg name
let int4_uniform = fun prg name -> gen_uniform `int_vec4 uniform_4iv prg name
let bool_uniform = fun prg name -> gen_uniform `bool uniform_1bv prg name
let bool2_uniform = fun prg name -> gen_uniform `bool_vec2 uniform_2bv prg name
let bool3_uniform = fun prg name -> gen_uniform `bool_vec3 uniform_3bv prg name
let bool4_uniform = fun prg name -> gen_uniform `bool_vec4 uniform_4bv prg name
let float_uniform = fun prg name -> gen_uniform `float uniform_1fv prg name
let float2_uniform = fun prg name -> gen_uniform `float_vec2 uniform_2fv prg name
let float3_uniform = fun prg name -> gen_uniform `float_vec3 uniform_3fv prg name
let float4_uniform = fun prg name -> gen_uniform `float_vec4 uniform_4fv prg name

let int_cst_uniform = fun prg name -> gen_cst_uniform `int uniform_1iv prg name
let int2_cst_uniform = fun prg name -> gen_cst_uniform `int_vec2 uniform_2iv prg name
let int3_cst_uniform = fun prg name -> gen_cst_uniform `int_vec3 uniform_3iv prg name
let int4_cst_uniform = fun prg name -> gen_cst_uniform `int_vec4 uniform_4iv prg name
let bool_cst_uniform = fun prg name -> gen_cst_uniform `bool uniform_1bv prg name
let bool2_cst_uniform = fun prg name -> gen_cst_uniform `bool_vec2 uniform_2bv prg name
let bool3_cst_uniform = fun prg name -> gen_cst_uniform `bool_vec3 uniform_3bv prg name
let bool4_cst_uniform = fun prg name -> gen_cst_uniform `bool_vec4 uniform_4bv prg name
let float_cst_uniform = fun prg name -> gen_cst_uniform `float uniform_1fv prg name
let float2_cst_uniform = fun prg name -> gen_cst_uniform `float_vec2 uniform_2fv prg name
let float3_cst_uniform = fun prg name -> gen_cst_uniform `float_vec3 uniform_3fv prg name
let float4_cst_uniform = fun prg name -> gen_cst_uniform `float_vec4 uniform_4fv prg name

let gen_mat_uniform = fun ty (fn: loc:int -> ?count:int -> ?transp:bool -> 'a array -> unit) prg ?(transp=false) name ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
      failwith "Bad type");
    let size = tysize ty in
    let value cont a =
      let count = Array.length a / size in
      (try fn ~loc:index ~count a;
       with Failure s ->
	 failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
      prg.value cont
    in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      let value cont a = prg.value cont in
      { prg with value }

let gen_cst_mat_uniform = fun ty (fn: loc:int -> ?count:int -> ?transp:bool -> 'a array -> unit) prg ?(transp=false)  name a ->
  try
    let (ty',_size,index), uniforms = assoc_rm name prg.uniforms in
    if ty <> ty' then (
      Printf.eprintf "ERROR: bad type for uniforms %s for %s" name prg.name;
      failwith "Bad type");
    let size = tysize ty in
    let value cont =
      let count = Array.length a / size in
      (try fn ~loc:index ~count a;
       with Failure s ->
	 failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
      prg.value cont
    in
    { prg with uniforms; value }
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
    let value cont texture =
      (try
	 active_texture texture.n;
	 bind_texture ty texture.index;
	 uniform_1i index texture.n
       with Failure s ->
	 failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
      prg.value cont
    in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      let value cont a = prg.value cont in
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
    let value cont =
      (try
	 active_texture texture.n;
	 bind_texture ty texture.index;
	 uniform_1i index texture.n
       with Failure s ->
	 failwith (Printf.sprintf "%s for %s in %s" s name prg.name));
      prg.value cont
    in
    { prg with uniforms; value }
  with
    Not_found ->
      Printf.eprintf "ERROR: Useless uniforms %s for %s" name prg.name;
      prg
