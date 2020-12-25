(****************************************************************************)
(* MLGles3: OpenGL ES 3.0 interface for Objective Caml                      *)
(*                                                                          *)
(* Copyright (C) 2014   Alexandre Miquel <amiquel@fing.edu.uy>              *)
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
(* along with MLGles2.  If not, see <http://www.gnu.org/licenses/>.         *)
(****************************************************************************)
(* gles3.ml: implementation of Gles2 library                                *)
(****************************************************************************)

(*** Bigarray types ***)
module Type = Gles3_type

open Type
open Bigarray

type byte_bigarray = (int, int8_signed_elt, c_layout) Genarray.t
type ubyte_bigarray = (int, int8_unsigned_elt, c_layout) Genarray.t
type short_bigarray = (int, int16_signed_elt, c_layout) Genarray.t
type ushort_bigarray = (int, int16_unsigned_elt, c_layout) Genarray.t
type uint_bigarray = (int32, int32_elt, c_layout) Genarray.t
type float_bigarray = (float, float32_elt, c_layout) Genarray.t

let create_byte_bigarray len = Genarray.create int8_signed c_layout [|len|]
let create_ubyte_bigarray len = Genarray.create int8_unsigned c_layout [|len|]
let create_short_bigarray len = Genarray.create int16_signed c_layout [|len|]
let create_ushort_bigarray len = Genarray.create int16_unsigned c_layout [|len|]
let create_uint_bigarray len = Genarray.create int32 c_layout [|len|]
let create_float_bigarray len = Genarray.create float32 c_layout [|len|]

(* create a shadow file descriptor *)
let tempfd () =
 let name = Filename.temp_file "mmap" "TMP" in
  try
    let fd = Unix.openfile name [Unix.O_RDWR; Unix.O_CREAT] 0o600 in
    Unix.unlink name;
    fd
  with e -> Unix.unlink name; raise e

let create_mmapped_byte_bigarray len = Unix.map_file (tempfd ()) int8_signed c_layout true [|len|]
let create_mmapped_ubyte_bigarray len = Unix.map_file (tempfd ()) int8_unsigned c_layout true [|len|]
let create_mmapped_short_bigarray len = Unix.map_file (tempfd ()) int16_signed c_layout true [|len|]
let create_mmapped_ushort_bigarray len = Unix.map_file (tempfd ()) int16_unsigned c_layout true [|len|]
let create_mmapped_uint_bigarray len = Unix.map_file (tempfd ()) int32 c_layout true [|len|]
let create_mmapped_float_bigarray len = Unix.map_file (tempfd ()) float32 c_layout true [|len|]

(****************************************************************************)
(*   VERTEX ATTRIBUTES & DRAWING                                            *)
(****************************************************************************)

external vertex_attrib_1f :
    index:(int [@untagged]) -> (float [@unboxed]) -> unit
	= "ml_glVertexAttrib1f" "mlU_glVertexAttrib1f"

external vertex_attrib_2f :
    index:(int [@untagged]) -> (float [@unboxed]) -> (float [@unboxed]) -> unit
	= "ml_glVertexAttrib2f" "mlU_glVertexAttrib2f"

external vertex_attrib_3f :
  index:(int [@untagged]) -> (float [@unboxed]) -> (float [@unboxed])
       -> (float [@unboxed]) -> unit
	= "ml_glVertexAttrib3f" "mlU_glVertexAttrib3f"

external vertex_attrib_4f :
  index:(int [@untagged]) -> (float [@unboxed]) -> (float [@unboxed])
       -> (float [@unboxed]) -> (float [@unboxed]) -> unit
	= "ml_glVertexAttrib4f" "mlU_glVertexAttrib4f"

external vertex_attrib_fv :
    index:int -> float array -> unit
	= "ml_glVertexAttribfv"

external enable_vertex_attrib_array : index:(int [@untagged]) -> unit
    = "ml_glEnableVertexAttribArray" "mlU_glEnableVertexAttribArray"

external disable_vertex_attrib_array : index:(int [@untagged]) -> unit
    = "ml_glDisableVertexAttribArray" "mlU_glDisableVertexAttribArray"

external vertex_attrib_byte_pointer_aux :
    index:int -> size:int -> norm:bool ->
    stride:int -> byte_bigarray -> unit
	= "ml_glVertexAttribBytePointer"

let vertex_attrib_byte_pointer ~index ~size ?(norm=false) ?(stride=0) a =
  vertex_attrib_byte_pointer_aux ~index ~size ~norm ~stride a

external vertex_attrib_ubyte_pointer_aux :
    index:int -> size:int -> norm:bool ->
    stride:int -> ubyte_bigarray -> unit
	= "ml_glVertexAttribUBytePointer"

let vertex_attrib_ubyte_pointer ~index ~size ?(norm=false) ?(stride=0) a =
  vertex_attrib_ubyte_pointer_aux ~index ~size ~norm ~stride a

external vertex_attrib_short_pointer_aux :
    index:int -> size:int -> norm:bool ->
    stride:int -> short_bigarray -> unit
	= "ml_glVertexAttribShortPointer"

let vertex_attrib_short_pointer ~index ~size ?(norm=false) ?(stride=0) a =
  vertex_attrib_short_pointer_aux ~index ~size ~norm ~stride a

external vertex_attrib_ushort_pointer_aux :
    int -> int -> bool -> int -> ushort_bigarray -> unit
	= "ml_glVertexAttribUShortPointer"

let vertex_attrib_ushort_pointer ~index ~size ?(norm=false) ?(stride=0) a =
  vertex_attrib_ushort_pointer_aux index size norm stride a

external vertex_attrib_uint_pointer_aux :
    int -> int -> bool -> int -> uint_bigarray -> unit
	= "ml_glVertexAttribUIntPointer"

let vertex_attrib_uint_pointer ~index ~size ?(norm=false) ?(stride=0) a =
  vertex_attrib_uint_pointer_aux index size norm stride a

external vertex_attrib_float_pointer_aux :
    index:int -> size:int -> norm:bool ->
    stride:int -> float_bigarray -> unit
	= "ml_glVertexAttribFloatPointer"

let vertex_attrib_float_pointer ~index ~size ?(norm=false) ?(stride=0) a =
  vertex_attrib_float_pointer_aux ~index ~size ~norm ~stride a

external vertex_attrib_buffer_pointer_aux :
  (int [@untagged]) -> (int [@untagged]) -> (storage_type [@untagged]) ->
  norm:bool -> (int [@untagged]) -> (int [@untagged]) -> unit
  = "ml_glVertexAttribBufferPointer" "mlU_glVertexAttribBufferPointer"

let vertex_attrib_buffer_pointer
    ~index ~size ~typ ?(norm=false) ?(stride=0) p =
  vertex_attrib_buffer_pointer_aux index size typ norm stride p

external draw_arrays_aux :
  (shape [@untagged]) -> first:(int [@untagged]) -> count:(int [@untagged]) -> unit
    = "ml_glDrawArrays" "mlU_glDrawArrays"

let draw_arrays shape ?(first=0) ~count =
  draw_arrays_aux shape ~first ~count

external draw_ubyte_elements :
    (shape [@untagged]) -> count:(int [@untagged]) -> ubyte_bigarray -> unit
	= "ml_glDrawUByteElements" "mlU_glDrawUByteElements"

external draw_ushort_elements :
    (shape [@untagged]) -> count:(int [@untagged]) -> ushort_bigarray -> unit
	= "ml_glDrawUShortElements" "mlU_glDrawUShortElements"

external draw_uint_elements :
    (shape [@untagged]) -> count:(int [@untagged]) -> uint_bigarray -> unit
	= "ml_glDrawUIntElements" "mlU_glDrawUIntElements"

external draw_buffer_elements :
  (shape [@untagged]) -> count:(int [@untagged])
  -> typ:(storage_type [@untagged]) -> (int [@untagged]) -> unit
	= "ml_glDrawBufferElements" "mlU_glDrawBufferElements"

(****************************************************************************)
(*   BUFFERS                                                                *)
(****************************************************************************)

type buffer = int

external int_of_buffer : buffer -> int = "%identity"
external buffer_of_int : int -> buffer = "%identity"
let null_buffer = buffer_of_int 0

external is_buffer : (buffer [@untagged]) -> bool
  = "ml_glIsBuffer" "mlU_glIsBuffer"
external gen_buffer : unit -> buffer = "ml_glGenBuffer"
external gen_buffers : int -> buffer array = "ml_glGenBuffers"
external delete_buffer : buffer -> unit = "ml_glDeleteBuffer"
external delete_buffers : buffer array -> unit = "ml_glDeleteBuffers"

external bind_buffer : target:('a buffer_target [@untagged]) -> (buffer [@untagged]) -> unit
    = "ml_glBindBuffer" "mlU_glBindBuffer"

external buffer_size :
    target:('a buffer_target [@untagged]) -> size:(int [@untagged]) -> usage:(buffer_usage [@untagged]) -> unit
	= "ml_glBufferSize" "mlU_glBufferSize"

external buffer_data :
    target:'a buffer_target ->
    ('a, 'b, c_layout) Genarray.t -> usage:buffer_usage -> unit
	= "ml_glBufferData"

external buffer_sub_data_aux :
    target:'a buffer_target -> offset:int ->
    ('a, 'b, c_layout) Genarray.t -> unit
	= "ml_glBufferSubData"

let buffer_sub_data ~target ?(offset=0) ba =
  buffer_sub_data_aux ~target ~offset ba

external get_buffer_size : target:'a buffer_target -> int
    = "ml_glGetBufferSize"

external get_buffer_usage : target:'a buffer_target -> buffer_usage
    = "ml_glGetBufferUsage"

(****************************************************************************)
(*   SHADERS                                                                *)
(****************************************************************************)

type shader = int

external int_of_shader : shader -> int = "%identity"
external shader_of_int : int -> shader = "%identity"
let null_shader = shader_of_int 0

external is_shader : (shader [@untagged]) -> bool
  = "ml_glIsShader" "mlU_glIsShader"
external create_shader : (shader_type [@untagged]) -> (shader [@untagged])
  = "ml_glCreateShader" "mlU_glCreateShader"
external delete_shader : (shader [@untagged]) -> unit
  = "ml_glDeleteShader" "mlU_glDeleteShader"

external shader_source : shader -> string array -> unit = "ml_glShaderSource"
external compile_shader : (shader [@untagged]) -> unit
  = "ml_glCompileShader" "mlU_glCompileShader"
external release_shader_compiler : unit -> unit = "ml_glReleaseShaderCompiler"

external get_shader_type : shader -> shader_type = "ml_glGetShaderType"
external get_shader_source : shader -> string = "ml_glGetShaderSource"
external get_shader_info_log : shader -> string = "ml_glGetShaderInfoLog"

external get_shader_delete_status : shader -> bool
    = "ml_glGetShaderDeleteStatus"

external get_shader_compile_status : shader -> bool
    = "ml_glGetShaderCompileStatus"

(****************************************************************************)
(*   PROGRAMS                                                               *)
(****************************************************************************)

type program = int

external int_of_program : program -> int = "%identity"
external program_of_int : int -> program = "%identity"
let null_program = program_of_int 0

external is_program : (program [@untagged]) -> bool
  = "ml_glIsShader" "mlU_glIsShader"
external create_program : unit -> (program [@untagged])
  = "ml_glCreateProgram" "mlU_glCreateProgram"
external delete_program : (program [@untagged]) -> unit
  = "ml_glDeleteProgram" "mlU_glDeleteProgram"

external attach_shader : (program [@untagged]) -> (shader [@untagged]) -> unit =
  "ml_glAttachShader" "mlU_glAttachShader"
external detach_shader : (program [@untagged]) -> (shader [@untagged]) -> unit =
  "ml_glDetachShader" "mlU_glDetachShader"

external link_program : (program [@untagged]) -> unit =
  "ml_glLinkProgram"  "mlU_glLinkProgram"
external use_program : (program [@untagged]) -> unit =
  "ml_glUseProgram"   "mlU_glUseProgram"
external validate_program : program -> bool = "ml_glValidateProgram"

let string_of_type = function
  | x when x = sh_int -> "int"
  | x when x = sh_int_vec2 -> "int_vec2"
  | x when x = sh_int_vec3 -> "int_vec3"
  | x when x = sh_int_vec4 -> "int_vec4"
  | x when x = sh_bool -> "bool"
  | x when x = sh_bool_vec2 -> "bool_vec2"
  | x when x = sh_bool_vec3 -> "bool_vec3"
  | x when x = sh_bool_vec4 -> "bool_vec4"
  | x when x = sh_float -> "float"
  | x when x = sh_float_vec2 -> "float_vec2"
  | x when x = sh_float_vec3 -> "float_vec3"
  | x when x = sh_float_vec4 -> "float_vec4"
  | x when x = sh_float_mat2 -> "float_mat2"
  | x when x = sh_float_mat3 -> "float_mat3"
  | x when x = sh_float_mat4 -> "float_mat4"
  | x when x = sh_sampler_2d -> "sampler_2d"
  | x when x = sh_sampler_2d_shadow -> "sampler_2d_shadow"
  | x when x = sh_sampler_cube -> "sampler_cube"
  | _ -> assert false

let glsl_string_of_type = function
  | x when x = sh_int -> "int"
  | x when x = sh_int_vec2 -> "ivec2"
  | x when x = sh_int_vec3 -> "ivec3"
  | x when x = sh_int_vec4 -> "ivec4"
  | x when x = sh_bool -> "bool"
  | x when x = sh_bool_vec2 -> "bvec2"
  | x when x = sh_bool_vec3 -> "bvec3"
  | x when x = sh_bool_vec4 -> "bvec4"
  | x when x = sh_float -> "float"
  | x when x = sh_float_vec2 -> "vec2"
  | x when x = sh_float_vec3 -> "vec3"
  | x when x = sh_float_vec4 -> "vec4"
  | x when x = sh_float_mat2 -> "mat2"
  | x when x = sh_float_mat3 -> "mat3"
  | x when x = sh_float_mat4 -> "mat4"
  | x when x = sh_sampler_2d -> "sampler2D"
  | x when x = sh_sampler_2d_shadow -> "sampler_2d_shadow"
  | x when x = sh_sampler_cube -> "samplerCube"
  | _ -> assert false

external get_active_attribs :
    program -> (string * int * attribute_type * int) list
	= "ml_glGetActiveAttribs"

external get_attrib_location :
  (program [@untagged]) -> string -> (int [@untagged]) =
  "ml_glGetAttribLocation" "mlU_glGetAttribLocation"

external bind_attrib_location :
  (program [@untagged]) -> (int [@untagged]) -> string -> unit
  = "ml_glBindAttribLocation" "mlU_glBindAttribLocation"

external get_active_uniforms :
    program -> (string * int * uniform_type * int) list
	= "ml_glGetActiveUniforms"

external get_uniform_location :
  (program [@untagged]) -> string -> (int [@untagged])
  = "ml_glGetUniformLocation" "mlU_glGetUniformLocation"

external get_attached_shaders : program -> shader array
  = "ml_glGetAttachedShaders"

external get_program_info_log : program -> string = "ml_glGetProgramInfoLog"

external get_program_delete_status : program -> bool
    = "ml_glGetProgramDeleteStatus"

external get_program_link_status : program -> bool
    = "ml_glGetProgramLinkStatus"

(****************************************************************************)
(*   UNIFORMS                                                               *)
(****************************************************************************)

external uniform_1i : loc:(int [@untagged]) -> (int [@untagged]) -> unit
  = "ml_glUniform1i" "mlU_glUniform1i"

external uniform_2i :
  loc:(int [@untagged]) -> (int [@untagged]) -> (int [@untagged]) -> unit
  = "ml_glUniform2i" "mlU_glUniform2i"

external uniform_3i :
  loc:(int [@untagged]) -> (int [@untagged]) -> (int [@untagged])
  -> (int [@untagged]) -> unit
  = "ml_glUniform3i" "mlU_glUniform3i"

external uniform_4i :
  loc:(int [@untagged]) -> (int [@untagged]) -> (int [@untagged])
  -> (int [@untagged]) -> (int [@untagged]) -> unit
  = "ml_glUniform4i" "mlU_glUniform4i"

external uniform_1iv_aux : loc:int -> count:int -> int array -> unit
    = "ml_glUniform1iv"

let uniform_1iv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a
    | Some count -> count in
  uniform_1iv_aux ~loc ~count a

external uniform_2iv_aux : loc:int -> count:int -> int array -> unit
    = "ml_glUniform2iv"

let uniform_2iv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a / 2
    | Some count -> count in
  uniform_2iv_aux ~loc ~count a

external uniform_3iv_aux : loc:int -> count:int -> int array -> unit
    = "ml_glUniform3iv"

let uniform_3iv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a / 3
    | Some count -> count in
  uniform_3iv_aux ~loc ~count a

external uniform_4iv_aux : loc:int -> count:int -> int array -> unit
    = "ml_glUniform4iv"

let uniform_4iv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a / 4
    | Some count -> count in
  uniform_4iv_aux ~loc ~count a

external uniform_1b :
    loc:int -> bool -> unit = "ml_glUniform1i"

external uniform_2b :
    loc:int -> bool -> bool -> unit = "ml_glUniform2i"

external uniform_3b :
    loc:int -> bool -> bool -> bool -> unit = "ml_glUniform3i"

external uniform_4b :
    loc:int -> bool -> bool -> bool -> bool -> unit = "ml_glUniform4i"

external uniform_1bv_aux : loc:int -> count:int -> bool array -> unit
    = "ml_glUniform1iv"

let uniform_1bv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a
    | Some count -> count in
  uniform_1bv_aux ~loc ~count a

external uniform_2bv_aux : loc:int -> count:int -> bool array -> unit
    = "ml_glUniform2iv"

let uniform_2bv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a / 2
    | Some count -> count in
  uniform_2bv_aux ~loc ~count a

external uniform_3bv_aux : loc:int -> count:int -> bool array -> unit
    = "ml_glUniform3iv"

let uniform_3bv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a / 3
    | Some count -> count in
  uniform_3bv_aux ~loc ~count a

external uniform_4bv_aux : loc:int -> count:int -> bool array -> unit
    = "ml_glUniform4iv"

let uniform_4bv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a / 4
    | Some count -> count in
  uniform_4bv_aux ~loc ~count a

external uniform_1f : loc:(int [@untagged]) -> (float [@unboxed]) -> unit
  = "ml_glUniform1f" "mlU_glUniform1f"

external uniform_2f :
  loc:(int [@untagged]) -> (float [@unboxed]) -> (float [@unboxed]) -> unit
  = "ml_glUniform2f" "mlU_glUniform2f"

external uniform_3f :
  loc:(int [@untagged]) -> (float [@unboxed]) -> (float [@unboxed])
  -> (float [@unboxed]) -> unit
  = "ml_glUniform3f" "mlU_glUniform3f"

external uniform_4f :
  loc:(int [@untagged]) -> (float [@unboxed]) -> (float [@unboxed])
  -> (float [@unboxed]) -> (float [@unboxed]) -> unit
  = "ml_glUniform4f" "mlU_glUniform4f"

external uniform_1fv_aux : loc:int -> count:int -> float array -> unit
    = "ml_glUniform1fv"

let uniform_1fv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a
    | Some count -> count in
  uniform_1fv_aux ~loc ~count a

external uniform_2fv_aux : loc:int -> count:int -> float array -> unit
    = "ml_glUniform2fv"

let uniform_2fv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a / 2
    | Some count -> count in
  uniform_2fv_aux ~loc ~count a

external uniform_3fv_aux : loc:int -> count:int -> float array -> unit
    = "ml_glUniform3fv"

let uniform_3fv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a / 3
    | Some count -> count in
  uniform_3fv_aux ~loc ~count a

external uniform_4fv_aux : loc:int -> count:int -> float array -> unit
    = "ml_glUniform4fv"

let uniform_4fv ~loc ?count a =
  let count =
    match count with
    | None -> Array.length a / 4
    | Some count -> count in
  uniform_4fv_aux ~loc ~count a

external uniform_matrix_2fv_aux :
    loc:int -> count:int -> transp:bool -> float array -> unit
	= "ml_glUniformMatrix2fv"

let uniform_matrix_2fv ~loc ?count ?(transp=false) a =
  let count =
    match count with
    | None -> Array.length a / 4
    | Some count -> count in
  uniform_matrix_2fv_aux ~loc ~count ~transp a

external uniform_matrix_3fv_aux :
    loc:int -> count:int -> transp:bool -> float array -> unit
	= "ml_glUniformMatrix3fv"

let uniform_matrix_3fv ~loc ?count ?(transp=false) a =
  let count =
    match count with
    | None -> Array.length a / 9
    | Some count -> count in
  uniform_matrix_3fv_aux ~loc ~count ~transp a

external uniform_matrix_4fv_aux :
    loc:int -> count:int -> transp:bool -> float array -> unit
	= "ml_glUniformMatrix4fv"

let uniform_matrix_4fv ~loc ?count ?(transp=false) a =
  let count =
    match count with
    | None -> Array.length a / 16
    | Some count -> count in
  uniform_matrix_4fv_aux ~loc ~count ~transp a

(****************************************************************************)
(*   RASTERIZATION                                                          *)
(****************************************************************************)

external depth_range
         : near:(float [@unboxed]) -> far:(float [@unboxed]) -> unit
  = "ml_glDepthRangef" "mlU_glDepthRangef"
external viewport :
  x:(int [@untagged]) -> y:(int [@untagged]) -> w:(int [@untagged])
  -> h:(int [@untagged]) -> unit
  = "ml_glViewport" "mlU_glViewport"

external is_enabled : (cap [@untagged]) -> bool
  = "ml_glIsEnabled" "mlU_glIsEnabled"
external enable : (cap [@untagged]) -> unit
  = "ml_glEnable" "mlU_glEnable"
external disable : (cap [@untagged]) -> unit
  = "ml_glDisable" "mlU_glDisable"

external line_width : (float [@unboxed]) -> unit =
  "ml_glLineWidth" "mlU_glLineWidth"

external front_face : (orientation [@untagged]) -> unit
  = "ml_glFrontFace" "mlU_glFrontFace"
external cull_face : face:(face [@untagged]) -> unit
  = "ml_glCullFace" "mlU_glCullFace"

external polygon_offset
         : factor:(float [@unboxed]) -> units:(float [@unboxed]) -> unit =
  "ml_glPolygonOffset" "mlU_glPolygonOffset"

(****************************************************************************)
(*   TEXTURES                                                               *)
(****************************************************************************)

type texture = int

external int_of_texture : texture -> int = "%identity"
external texture_of_int : int -> texture = "%identity"
let null_texture = texture_of_int 0

external is_texture : (texture [@untagged]) -> bool
  = "ml_glIsTexture" "mlU_glIsTexture"
external gen_texture : unit -> texture = "ml_glGenTexture"
external gen_textures : int -> texture array = "ml_glGenTextures"
external delete_texture : texture -> unit = "ml_glDeleteTexture"
external delete_textures : texture array -> unit = "ml_glDeleteTextures"

external active_texture : texture -> unit = "ml_glActiveTexture"

external bind_texture : target:(texture_target [@untagged]) -> (texture [@untagged]) -> unit
    = "ml_glBindTexture" "mlU_glBindTexture"

type image = {
    width : int ;
    height : int ;
    format : image_format ;
    data : ubyte_bigarray ;
  }

external tex_image_2d_aux :
    target:texture_image_target -> level:int -> image -> unit
	= "ml_glTexImage2D"

external tex_null_image_2d_aux :
    target:texture_image_target -> level:int -> int -> int -> internal_image_format -> unit
	= "ml_glTexNullImage2D"

let tex_image_2d ~target ?(level=0) img =
  tex_image_2d_aux ~target ~level img

let tex_null_image_2d ~target ?(level=0) n m iif =
  tex_null_image_2d_aux ~target ~level n m iif

external tex_sub_image_2d_aux :
    target:texture_image_target -> level:int ->
    xoffset:int -> yoffset:int -> image -> unit = "ml_glTexImage2D"

let tex_sub_image_2d ~target ?(level=0) ?(xoffset=0) ?(yoffset=0) img =
  tex_sub_image_2d_aux ~target ~level ~xoffset ~yoffset img

type rectangle = int * int * int * int  (* x, y, width, height *)

external copy_tex_image_2d_aux :
    target:texture_image_target -> level:int ->
    format:image_format -> rectangle -> unit = "ml_glCopyTexImage2D"

let copy_tex_image_2d ~target ?(level=0) ~format img =
  copy_tex_image_2d_aux ~target ~level ~format img

external copy_tex_sub_image_2d_aux :
    target:texture_image_target -> level:int ->
    xoffset:int -> yoffset:int -> rectangle -> unit = "ml_glCopyTexSubImage2D"

let copy_tex_sub_image_2d ~target ?(level=0) ?(xoffset=0) ?(yoffset=0) rect =
  copy_tex_sub_image_2d_aux ~target ~level ~xoffset ~yoffset rect

external tex_parameter :
  target:(texture_target [@untagged])->
  ('a texture_parameter [@untagged])  -> ('a texture_value [@untagged]) -> unit
    = "ml_glTexParameteri" "mlU_glTexParameteri"

external generate_mipmap : target:(texture_target [@untagged]) -> unit
    = "ml_glGenerateMipmap" "mlU_glGenerateMipmap"

(****************************************************************************)
(*   PER-FRAGMENT OPERATIONS                                                *)
(****************************************************************************)

external scissor : x:(int [@untagged]) -> y:(int [@untagged])
                   -> w:(int [@untagged]) -> h:(int [@untagged]) -> unit
  = "ml_glScissor" "mlU_glScissor"

external sample_coverage_aux : (float [@unboxed]) -> invert:bool -> unit =
  "ml_glSampleCoverage" "mlU_glSampleCoverage"


let sample_coverage ?(invert=false) f = sample_coverage_aux f ~invert

external stencil_func :
  func:(cmp_func [@untagged]) -> ref:(int [@untagged])
  -> mask:(int [@untagged]) -> unit
  = "ml_glStencilFunc" "mlU_glStencilFunc"

external stencil_func_separate :
  face:(face [@untagged]) -> func:(cmp_func [@untagged])
  -> ref:(int [@untagged]) -> mask:(int [@untagged]) -> unit
  = "ml_glStencilFuncSeparate" "mlU_glStencilFuncSeparate"

external stencil_op :
  sfail:(stencil_op [@untagged]) -> dpfail:(stencil_op [@untagged])
  -> dppass:(stencil_op [@untagged]) -> unit
  = "ml_glStencilOp" "mlU_glStencilOp"

external stencil_op_separate :
  face:(face [@untagged]) -> sfail:(stencil_op [@untagged]) ->
  dpfail:(stencil_op [@untagged]) -> dppass:(stencil_op [@untagged]) -> unit
  = "ml_glStencilOpSeparate" "mlU_glStencilOpSeparate"

external depth_func :
  func:(cmp_func [@untagged]) -> unit
  = "ml_glDepthFunc" "mlU_glDepthFunc"

external blend_equation : (blend_mode [@untagged]) -> unit
  = "ml_glBlendEquation" "mlU_glBlendEquation"
external blend_equation_separate :
  rgb:(blend_mode [@untagged]) -> alpha:(blend_mode [@untagged]) -> unit
  = "ml_glBlendEquationSeparate" "mlU_glBlendEquationSeparate"

external blend_func :
  src:(src_blend_func [@untagged]) -> dst:(dst_blend_func [@untagged]) -> unit
  = "ml_glBlendFunc" "mlU_glBlendFunc"

external blend_func_separate :
  src_rgb:(src_blend_func [@untagged]) ->
  dst_rgb:(dst_blend_func [@untagged]) ->
  src_alpha:(src_blend_func [@untagged]) ->
  dst_alpha:(dst_blend_func [@untagged]) -> unit
	= "ml_glBlendFuncSeparate" "mlU_glBlendFuncSeparate"

type rgba = { r : float ; g : float ; b : float ; a : float }

external blend_color : rgba -> unit = "ml_glBlendColor"

(****************************************************************************)
(*   WHOLE FRAMEBUFFER OPERATIONS                                           *)
(****************************************************************************)

let rgba ~r ~g ~b ~a = { r = r ; g = g ; b = b ; a = a }

external color_mask :
    red:bool -> green:bool -> blue:bool -> alpha:bool -> unit
	= "ml_glColorMask"

external depth_mask : bool -> unit = "ml_glDepthMask"
external stencil_mask : (int [@untagged]) -> unit =
  "ml_glStencilMask" "mlU_glStencilMask"

external stencil_mask_separate :
  face:(face [@untagged]) -> (int [@untagged]) -> unit =
  "ml_glStencilMaskSeparate" "mlU_glStencilMaskSeparate"

external clear : which_buffer list -> unit = "ml_glClear"

external clear_color : rgba -> unit = "ml_glClearColor"
external clear_depth : (float [@unboxed]) -> unit =
  "ml_glClearDepthf" "mlU_glClearDepthf"
external clear_stencil : (int [@untagged]) -> unit =
  "ml_glClearStencil" "mlU_glClearStencil"

external read_pixels : x:int -> y:int -> image -> unit
    = "ml_glReadPixels"

(****************************************************************************)
(*   RENDERBUFFERS                                                          *)
(****************************************************************************)

type renderbuffer = int

external int_of_renderbuffer : renderbuffer -> int = "%identity"
external renderbuffer_of_int : int -> renderbuffer = "%identity"
let null_renderbuffer = renderbuffer_of_int 0

external is_renderbuffer : (renderbuffer [@untagged]) -> bool
  = "ml_glIsRenderbuffer" "mlU_glIsRenderbuffer"

external gen_renderbuffer : unit -> renderbuffer
    = "ml_glGenRenderbuffer"
external gen_renderbuffers : int -> renderbuffer array
    = "ml_glGenRenderbuffers"

external delete_renderbuffer : renderbuffer -> unit
    = "ml_glDeleteRenderbuffer"
external delete_renderbuffers : renderbuffer array -> unit
    = "ml_glDeleteRenderbuffers"

external draw_buffers : buffer_attachment array -> unit
    = "ml_glDrawBuffers"

external bind_renderbuffer :
    target:(renderbuffer_target [@untagged]) -> (renderbuffer [@untagged]) -> unit
	= "ml_glBindRenderbuffer" "mlU_glBindRenderbuffer"

external renderbuffer_storage :
  target:(renderbuffer_target [@untagged]) ->
  format:(renderbuffer_format [@untagged])->
  width:(int [@untagged]) -> height:(int [@untagged]) -> unit
  = "ml_glRenderbufferStorage" "mlU_glRenderbufferStorage"

(****************************************************************************)
(*   FRAMEBUFFERS                                                           *)
(****************************************************************************)

type framebuffer = int

external int_of_framebuffer : framebuffer -> int = "%identity"
external framebuffer_of_int : int -> framebuffer = "%identity"
let null_framebuffer = framebuffer_of_int 0

external is_framebuffer : framebuffer -> bool = "ml_glIsFramebuffer"

external gen_framebuffer : unit -> framebuffer
    = "ml_glGenFramebuffer"
external gen_framebuffers : int -> framebuffer array
    = "ml_glGenFramebuffers"

external delete_framebuffer : framebuffer -> unit
    = "ml_glDeleteFramebuffer"
external delete_framebuffers : framebuffer array -> unit
    = "ml_glDeleteFramebuffers"

external bind_framebuffer :
    target:(framebuffer_target [@untagged]) -> (framebuffer [@untagged]) -> unit
	= "ml_glBindFramebuffer" "mlU_glBindFramebuffer"

external framebuffer_renderbuffer :
  target:(framebuffer_target [@untagged]) ->
  attach:(framebuffer_attachment [@untagged])->
  target2:(renderbuffer_target [@untagged]) -> (renderbuffer [@untagged]) -> unit
  = "ml_glFramebufferRenderbuffer" "mlU_glFramebufferRenderbuffer"

external framebuffer_texture_2d :
  target:(framebuffer_target [@untagged]) ->
  attach:(framebuffer_attachment [@untagged]) ->
  target2:(texture_image_target [@untagged]) ->
  (texture [@untagged]) -> level:(int [@untagged]) -> unit
  = "ml_glFramebufferTexture2D" "mlU_glFramebufferTexture2D"

external check_framebuffer_status :
    target:(framebuffer_target [@untagged]) -> (framebuffer_status [@untagged])
	= "ml_glCheckFramebufferStatus" "mlU_glCheckFramebufferStatus"

(****************************************************************************)
(*   MISCELLANEOUS                                                          *)
(****************************************************************************)

let error_to_string = function
  | x when x = gl_no_error -> "no_error"
  | x when x = gl_invalid_enum -> "invalid_enum"
  | x when x = gl_invalid_framebuffer_operation -> "invalid_framebuffer_operation"
  | x when x = gl_invalid_value -> "invalid_value"
  | x when x = gl_invalid_operation -> "invalid_operation"
  | x when x = gl_out_of_memory -> "out_of_memory"
  | _ -> "unknown gl error"

external get_error : unit -> (error [@untagged])
  = "ml_glGetError" "mlU_glGetError"

let rec show_errors msg =
  let error = get_error () in
  if error <> gl_no_error then (
    Printf.eprintf "error %s during draw %s\n%!" (error_to_string error) msg;
    show_errors msg)

external get_vendor : unit -> string = "ml_glGetVendor"
external get_renderer : unit -> string = "ml_glGetRenderer"
external get_version : unit -> string = "ml_glGetVersion"
external get_shading_language_version : unit -> string
    = "ml_glGetShadingLanguageVersion"
external get_extensions : unit -> string = "ml_glGetExtensions"

external flush : unit -> unit = "ml_glFlush"
external finish : unit -> unit = "ml_glFinish"

external get_max_textures : unit -> int = "ml_glGetMaxTextures"
