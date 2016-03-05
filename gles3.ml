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
(* gles2.ml: implementation of Gles2 library                                *)
(****************************************************************************)

(*** Bigarray types ***)

open Bigarray

type byte_bigarray = (int, int8_signed_elt, c_layout) Array1.t
type ubyte_bigarray = (int, int8_unsigned_elt, c_layout) Array1.t
type short_bigarray = (int, int16_signed_elt, c_layout) Array1.t
type ushort_bigarray = (int, int16_unsigned_elt, c_layout) Array1.t
type uint_bigarray = (int32, int32_elt, c_layout) Array1.t
type float_bigarray = (float, float32_elt, c_layout) Array1.t

let create_byte_bigarray len = Array1.create int8_signed c_layout len
let create_ubyte_bigarray len = Array1.create int8_unsigned c_layout len
let create_short_bigarray len = Array1.create int16_signed c_layout len
let create_ushort_bigarray len = Array1.create int16_unsigned c_layout len
let create_uint_bigarray len = Array1.create int32 c_layout len
let create_float_bigarray len = Array1.create float32 c_layout len

(****************************************************************************)
(*   VERTEX ATTRIBUTES & DRAWING                                            *)
(****************************************************************************)

external vertex_attrib_1f :
    index:int -> float -> unit
	= "ml_glVertexAttrib1f"

external vertex_attrib_2f :
    index:int -> float -> float -> unit
	= "ml_glVertexAttrib2f"

external vertex_attrib_3f :
    index:int -> float -> float -> float -> unit
	= "ml_glVertexAttrib3f"

external vertex_attrib_4f :
    index:int -> float -> float -> float -> float -> unit
	= "ml_glVertexAttrib4f"

external vertex_attrib_fv :
    index:int -> float array -> unit
	= "ml_glVertexAttribfv"

external enable_vertex_attrib_array : index:int -> unit
    = "ml_glEnableVertexAttribArray"

external disable_vertex_attrib_array : index:int -> unit
    = "ml_glDisableVertexAttribArray"

type storage_type = [ `byte | `ubyte | `short | `ushort | `uint | `float ]

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
    index:int -> size:int -> norm:bool ->
    stride:int -> ushort_bigarray -> unit
	= "ml_glVertexAttribUShortPointer"

let vertex_attrib_ushort_pointer ~index ~size ?(norm=false) ?(stride=0) a =
  vertex_attrib_ushort_pointer_aux ~index ~size ~norm ~stride a

external vertex_attrib_uint_pointer_aux :
    index:int -> size:int -> norm:bool ->
    stride:int -> uint_bigarray -> unit
	= "ml_glVertexAttribUIntPointer"

let vertex_attrib_uint_pointer ~index ~size ?(norm=false) ?(stride=0) a =
  vertex_attrib_uint_pointer_aux ~index ~size ~norm ~stride a

external vertex_attrib_float_pointer_aux :
    index:int -> size:int -> norm:bool ->
    stride:int -> float_bigarray -> unit
	= "ml_glVertexAttribFloatPointer"

let vertex_attrib_float_pointer ~index ~size ?(norm=false) ?(stride=0) a =
  vertex_attrib_float_pointer_aux ~index ~size ~norm ~stride a

external vertex_attrib_buffer_pointer_aux :
    index:int -> size:int -> typ:storage_type ->
    norm:bool -> stride:int -> int -> unit
	= "ml_glVertexAttribBufferPointer_bc" "ml_glVertexAttribBufferPointer"

let vertex_attrib_buffer_pointer
    ~index ~size ~typ ?(norm=false) ?(stride=0) p =
  vertex_attrib_buffer_pointer_aux ~index ~size ~typ ~norm ~stride p

type shape =
  [ `points | `lines | `line_strip | `line_loop
  | `triangles | `triangle_strip | `triangle_fan ]

external draw_arrays_aux : shape -> first:int -> count:int -> unit
    = "ml_glDrawArrays"

let draw_arrays shape ?(first=0) ~count =
  draw_arrays_aux shape ~first ~count

external draw_ubyte_elements :
    shape -> count:int -> ubyte_bigarray -> unit
	= "ml_glDrawUByteElements"

external draw_ushort_elements :
    shape -> count:int -> ushort_bigarray -> unit
	= "ml_glDrawUShortElements"

external draw_uint_elements :
    shape -> count:int -> uint_bigarray -> unit
	= "ml_glDrawUIntElements"

external draw_buffer_elements :
    shape -> count:int -> typ:storage_type -> int -> unit
	= "ml_glDrawBufferElements"

(****************************************************************************)
(*   BUFFERS                                                                *)
(****************************************************************************)

type buffer_usage = [ `static_draw | `dynamic_draw | `stream_draw ]
type buffer_target = [ `array_buffer | `element_array_buffer ]

type buffer

external int_of_buffer : buffer -> int = "%identity"
external buffer_of_int : int -> buffer = "%identity"
let null_buffer = buffer_of_int 0

external is_buffer : buffer -> bool = "ml_glIsBuffer"
external gen_buffer : unit -> buffer = "ml_glGenBuffer"
external gen_buffers : int -> buffer array = "ml_glGenBuffers"
external delete_buffer : buffer -> unit = "ml_glDeleteBuffer"
external delete_buffers : buffer array -> unit = "ml_glDeleteBuffers"

external bind_buffer : target:buffer_target -> buffer -> unit
    = "ml_glBindBuffer"

external buffer_size :
    target:buffer_target -> size:int -> usage:buffer_usage -> unit
	= "ml_glBufferSize"

external buffer_data :
    target:buffer_target ->
    ('a, 'b, c_layout) Array1.t -> usage:buffer_usage -> unit
	= "ml_glBufferData"

external buffer_sub_data_aux :
    target:buffer_target -> offset:int ->
    ('a, 'b, c_layout) Array1.t -> unit
	= "ml_glBufferSubData"

let buffer_sub_data ~target ?(offset=0) ba =
  buffer_sub_data_aux ~target ~offset ba

external get_buffer_size : target:buffer_target -> int
    = "ml_glGetBufferSize"

external get_buffer_usage : target:buffer_target -> buffer_usage
    = "ml_glGetBufferUsage"

(****************************************************************************)
(*   SHADERS                                                                *)
(****************************************************************************)

type shader

type shader_type = [ `vertex_shader | `fragment_shader ]

external int_of_shader : shader -> int = "%identity"
external shader_of_int : int -> shader = "%identity"
let null_shader = shader_of_int 0

external is_shader : shader -> bool = "ml_glIsShader"
external create_shader : shader_type -> shader = "ml_glCreateShader"
external delete_shader : shader -> unit = "ml_glDeleteShader"

external shader_source : shader -> string array -> unit = "ml_glShaderSource"
external compile_shader : shader -> unit = "ml_glCompileShader"
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

type program

external int_of_program : program -> int = "%identity"
external program_of_int : int -> program = "%identity"
let null_program = program_of_int 0

external is_program : program -> bool = "ml_glIsShader"
external create_program : unit -> program = "ml_glCreateProgram"
external delete_program : program -> unit = "ml_glDeleteProgram"

external attach_shader : program -> shader -> unit = "ml_glAttachShader"
external detach_shader : program -> shader -> unit = "ml_glDetachShader"

external link_program : program -> unit = "ml_glLinkProgram"
external use_program : program -> unit = "ml_glUseProgram"
external validate_program : program -> bool = "ml_glValidateProgram"

type int_type = [ `int | `int_vec2 | `int_vec3 | `int_vec4 ]
type bool_type = [ `bool | `bool_vec2 | `bool_vec3 | `bool_vec4 ]
type sampler_type = [ `sampler_2d | `sampler_2d_shadow |`sampler_cube ]

type float_type =
  [ `float | `float_vec2 | `float_vec3 | `float_vec4
  | `float_mat2 | `float_mat3 | `float_mat4 ]

type attribute_type = float_type
type uniform_type = [int_type|bool_type|float_type|sampler_type]

let string_of_type = function
  | `int -> "int"
  | `int_vec2 -> "int_vec2"
  | `int_vec3 -> "int_vec3"
  | `int_vec4 -> "int_vec4"
  | `bool -> "bool"
  | `bool_vec2 -> "bool_vec2"
  | `bool_vec3 -> "bool_vec3"
  | `bool_vec4 -> "bool_vec4"
  | `float -> "float"
  | `float_vec2 -> "float_vec2"
  | `float_vec3 -> "float_vec3"
  | `float_vec4 -> "float_vec4"
  | `float_mat2 -> "float_mat2"
  | `float_mat3 -> "float_mat3"
  | `float_mat4 -> "float_mat4"
  | `sampler_2d -> "sampler_2d"
  | `sampler_2d_shadow -> "sampler_2d_shadow"
  | `sampler_cube -> "sampler_cube"

let glsl_string_of_type = function
  | `int -> "int"
  | `int_vec2 -> "ivec2"
  | `int_vec3 -> "ivec3"
  | `int_vec4 -> "ivec4"
  | `bool -> "bool"
  | `bool_vec2 -> "bvec2"
  | `bool_vec3 -> "bvec3"
  | `bool_vec4 -> "bvec4"
  | `float -> "float"
  | `float_vec2 -> "vec2"
  | `float_vec3 -> "vec3"
  | `float_vec4 -> "vec4"
  | `float_mat2 -> "mat2"
  | `float_mat3 -> "mat3"
  | `float_mat4 -> "mat4"
  | `sampler_2d -> "sampler2D"
  | `sampler_2d_shadow -> "sampler_2d_shadow"
  | `sampler_cube -> "samplerCube"

external get_active_attribs :
    program -> (string * attribute_type * int) list
	= "ml_glGetActiveAttribs"

external get_attrib_location :
    program -> string -> int = "ml_glGetAttribLocation"

external bind_attrib_location :
    program -> int -> string -> unit = "ml_glBindAttribLocation"

external get_active_uniforms :
    program -> (string * uniform_type * int) list
	= "ml_glGetActiveUniforms"

external get_uniform_location :
    program -> string -> int = "ml_glGetUniformLocation"

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

external uniform_1i :
    loc:int -> int -> unit = "ml_glUniform1i"

external uniform_2i :
    loc:int -> int -> int -> unit = "ml_glUniform2i"

external uniform_3i :
    loc:int -> int -> int -> int -> unit = "ml_glUniform3i"

external uniform_4i :
    loc:int -> int -> int -> int -> int -> unit = "ml_glUniform4i"

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

external uniform_1f :
    loc:int -> float -> unit = "ml_glUniform1f"

external uniform_2f :
    loc:int -> float -> float -> unit = "ml_glUniform2f"

external uniform_3f :
    loc:int -> float -> float -> float -> unit = "ml_glUniform3f"

external uniform_4f :
    loc:int -> float -> float -> float -> float -> unit = "ml_glUniform4f"

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

external depth_range : near:float -> far:float -> unit = "ml_glDepthRangef"
external viewport : x:int -> y:int -> w:int -> h:int -> unit = "ml_glViewport"

type cap =
  [ `blend|`cull_face|`depth_test|`dither|`polygon_offset_fill
  | `sample_alpha_to_coverage|`sample_coverage|`scissor_test|`stencil_test ]

external is_enabled : cap -> bool = "ml_glIsEnabled"
external enable : cap -> unit = "ml_glEnable"
external disable : cap -> unit = "ml_glDisable"

external line_width : float -> unit = "ml_glLineWidth"

type face = [ `front | `back | `front_and_back ]

external front_face : [`cw|`ccw] -> unit = "ml_glFrontFace"
external cull_face : face:face -> unit = "ml_glCullFace"

external polygon_offset : factor:float -> units:float -> unit =
  "ml_glPolygonOffset"

(****************************************************************************)
(*   TEXTURES                                                               *)
(****************************************************************************)

type texture

external int_of_texture : texture -> int = "%identity"
external texture_of_int : int -> texture = "%identity"
let null_texture = texture_of_int 0

external is_texture : texture -> bool = "ml_glIsTexture"
external gen_texture : unit -> texture = "ml_glGenTexture"
external gen_textures : int -> texture array = "ml_glGenTextures"
external delete_texture : texture -> unit = "ml_glDeleteTexture"
external delete_textures : texture array -> unit = "ml_glDeleteTextures"

external active_texture : int -> unit = "ml_glActiveTexture"

type texture_target = [ `texture_2d | `texture_2d_shadow |`texture_cube_map ]

external bind_texture : target:texture_target -> texture -> unit
    = "ml_glBindTexture"

type texture_image_target =
  [ `texture_2d
  | `texture_cube_map_positive_x | `texture_cube_map_negative_x
  | `texture_cube_map_positive_y | `texture_cube_map_negative_y
  | `texture_cube_map_positive_z | `texture_cube_map_negative_z ]

type image_format =
  [ `alpha | `rgb | `rgba | `luminance | `luminance_alpha ]

type image = {
    width : int ;
    height : int ;
    format : image_format ;
    data : ubyte_bigarray ;
  }

type internal_image_format =
  [ `alpha | `rgb | `rgba | `luminance | `luminance_alpha
  | `depth_component16 | `depth_component24 | `depth24_stencil8 ]

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

type wrap_mode = [ `repeat | `clamp_to_edge ]

type min_filter =
  [ `nearest | `linear
  | `nearest_mipmap_nearest | `nearest_mipmap_linear
  | `linear_mipmap_nearest | `linear_mipmap_linear ]

type mag_filter = [ `nearest | `linear ]

type compare_mode = [ `none | `compare_ref_to_texture ]

type texture_parameter =
  [ `texture_wrap_s of wrap_mode
  | `texture_wrap_t of wrap_mode
  | `texture_min_filter of min_filter
  | `texture_compare_mode of compare_mode
  | `texture_mag_filter of mag_filter ]

external tex_parameter : target:texture_target -> texture_parameter -> unit
    = "ml_glTexParameter"

external generate_mipmap : target:texture_target -> unit
    = "ml_glGenerateMipmap"

(****************************************************************************)
(*   PER-FRAGMENT OPERATIONS                                                *)
(****************************************************************************)

external scissor : x:int -> y:int -> w:int -> h:int -> unit = "ml_glScissor"

external sample_coverage_aux : float -> invert:bool -> unit =
  "ml_glSampleCoverage"

let sample_coverage ?(invert=false) f = sample_coverage_aux f ~invert

type cmp_func =
  [ `never | `always | `equal | `notequal
  | `less | `lequal | `greater | `gequal ]

type stencil_op =
  [ `keep | `zero | `replace | `invert
  | `incr | `decr | `incr_wrap | `decr_wrap ]

external stencil_func :
    func:cmp_func -> ref:int -> mask:int -> unit = "ml_glStencilFunc"

external stencil_func_separate :
    face:face -> func:cmp_func -> ref:int -> mask:int -> unit
	= "ml_glStencilFuncSeparate"

external stencil_op :
    sfail:stencil_op ->
    dpfail:stencil_op -> dppass:stencil_op -> unit
	= "ml_glStencilOp"

external stencil_op_separate :
    face:face -> sfail:stencil_op ->
    dpfail:stencil_op -> dppass:stencil_op -> unit
	= "ml_glStencilOpSeparate"

external depth_func : func:cmp_func -> unit = "ml_glDepthFunc"

type blend_mode = [ `func_add | `func_subtract | `func_reverse_subtract ]

external blend_equation : blend_mode -> unit = "ml_glBlendEquation"
external blend_equation_separate :
    rgb:blend_mode -> alpha:blend_mode -> unit = "ml_glBlendEquationSeparate"

type dst_blend_func =
  [ `zero | `one
  | `src_color | `one_minus_src_color
  | `dst_color | `one_minus_dst_color
  | `src_alpha | `one_minus_src_alpha
  | `dst_alpha | `one_minus_dst_alpha
  | `constant_color | `one_minus_constant_color
  | `constant_alpha | `one_minus_constant_alpha ]

type src_blend_func = [ dst_blend_func | `src_alpha_saturate ]

external blend_func : src:src_blend_func -> dst:dst_blend_func -> unit
    = "ml_glBlendFunc"

external blend_func_separate :
    src_rgb:src_blend_func -> dst_rgb:dst_blend_func ->
    src_alpha:src_blend_func -> dst_alpha:dst_blend_func -> unit
	= "ml_glBlendFuncSeparate"

type rgba = { r : float ; g : float ; b : float ; a : float }

external blend_color : rgba -> unit = "ml_glBlendColor"

(****************************************************************************)
(*   WHOLE FRAMEBUFFER OPERATIONS                                           *)
(****************************************************************************)

type which_buffer = [ `color_buffer | `depth_buffer | `stencil_buffer ]

let rgba ~r ~g ~b ~a = { r = r ; g = g ; b = b ; a = a }

external color_mask :
    red:bool -> green:bool -> blue:bool -> alpha:bool -> unit
	= "ml_glColorMask"

external depth_mask : bool -> unit = "ml_glDepthMask"
external stencil_mask : int -> unit = "ml_glStencilMask"

external stencil_mask_separate :
    face:face -> int -> unit = "ml_glStencilMaskSeparate"

external clear : which_buffer list -> unit = "ml_glClear"

external clear_color : rgba -> unit = "ml_glClearColor"
external clear_depth : float -> unit = "ml_glClearDepthf"
external clear_stencil : int -> unit = "ml_glClearStencil"

external read_pixels : x:int -> y:int -> image -> unit
    = "ml_glReadPixels"

(****************************************************************************)
(*   RENDERBUFFERS                                                          *)
(****************************************************************************)

type renderbuffer_target = [ `renderbuffer ]

type renderbuffer

external int_of_renderbuffer : renderbuffer -> int = "%identity"
external renderbuffer_of_int : int -> renderbuffer = "%identity"
let null_renderbuffer = renderbuffer_of_int 0

external is_renderbuffer : renderbuffer -> bool = "ml_glIsRenderbuffer"

external gen_renderbuffer : unit -> renderbuffer
    = "ml_glGenRenderbuffer"
external gen_renderbuffers : int -> renderbuffer array
    = "ml_glGenRenderbuffers"

external delete_renderbuffer : renderbuffer -> unit
    = "ml_glDeleteRenderbuffer"
external delete_renderbuffers : renderbuffer array -> unit
    = "ml_glDeleteRenderbuffers"

external draw_buffers : [ `none | `back | `color_attachment0 | `color_attachment1
 | `color_attachment2 | `color_attachment3 | `color_attachment4 | `color_attachment5
 | `color_attachment6 | `color_attachment7 | `color_attachment8 | `color_attachment9
 | `color_attachment10 | `color_attachment11 | `color_attachment12 | `color_attachment13
 | `color_attachment14 | `color_attachment15 ] array -> unit
    = "ml_glDrawBuffers"

external bind_renderbuffer :
    target:renderbuffer_target -> renderbuffer -> unit
	= "ml_glBindRenderbuffer"

type renderbuffer_format =
    [ `depth_component16 | `depth_component24 | `rgba4 | `rgb5_a1 | `rgb565 | `stencil_index8 ]

external renderbuffer_storage :
    target:renderbuffer_target -> format:renderbuffer_format ->
    width:int -> height:int -> unit = "ml_glRenderbufferStorage"

(****************************************************************************)
(*   FRAMEBUFFERS                                                           *)
(****************************************************************************)

type framebuffer_target = [ `framebuffer ]

type framebuffer

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
    target:framebuffer_target -> framebuffer -> unit
	= "ml_glBindFramebuffer"

type framebuffer_attachment =
  [ `color_attachment0 | `depth_attachment | `stencil_attachment ]

external framebuffer_renderbuffer :
    target:framebuffer_target -> attach:framebuffer_attachment ->
    target2:renderbuffer_target -> renderbuffer -> unit
	= "ml_glFramebufferRenderbuffer"

external framebuffer_texture_2d :
    target:framebuffer_target -> attach:framebuffer_attachment ->
    target2:texture_image_target -> texture -> level:int -> unit
	= "ml_glFramebufferTexture2D"

type framebuffer_status =
  [ `framebuffer_complete
  | `framebuffer_incomplete_attachment
  | `framebuffer_incomplete_dimensions
  | `framebuffer_incomplete_missing_attachment
  | `framebuffer_unsupported ]

external check_framebuffer_status :
    target:framebuffer_target -> framebuffer_status
	= "ml_glCheckFramebufferStatus"

(****************************************************************************)
(*   MISCELLANEOUS                                                          *)
(****************************************************************************)

type error =
  [ `no_error | `invalid_enum | `invalid_framebuffer_operation
  | `invalid_value | `invalid_operation | `out_of_memory ]

external get_error : unit -> error = "ml_glGetError"

external get_vendor : unit -> string = "ml_glGetVendor"
external get_renderer : unit -> string = "ml_glGetRenderer"
external get_version : unit -> string = "ml_glGetVersion"
external get_shading_language_version : unit -> string
    = "ml_glGetShadingLanguageVersion"
external get_extensions : unit -> string = "ml_glGetExtensions"

external flush : unit -> unit = "ml_glFlush"
external finish : unit -> unit = "ml_glFinish"
