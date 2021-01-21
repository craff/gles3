(****************************************************************************)
(* MLGles2: OpenGL ES 3.0 interface for Objective Caml                      *)
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
(* along with MLGles3.  If not, see <http://www.gnu.org/licenses/>.         *)
(****************************************************************************)
(* gles3.mli: interface of Gles3 library                                    *)
(****************************************************************************)

(** Lowlevel OCaml interface to GLES API *)

(** {b Big array types} ***)

module Type : sig
  type (_,_) eq_type = True : ('a,'a) eq_type | False : ('a,'b) eq_type

  type shape
  val gl_points : shape
  val gl_lines  : shape
  val gl_line_loop : shape
  val gl_line_strip : shape
  val gl_triangles : shape
  val gl_triangle_strip : shape
  val gl_triangle_fan : shape

  type storage_type
  val gl_byte : storage_type
  val gl_ubyte : storage_type
  val gl_short : storage_type
  val gl_ushort : storage_type
  val gl_uint : storage_type
  val gl_float : storage_type

  type gl_bool
  val gl_true : gl_bool
  val gl_false : gl_bool

  type buffer_usage
  val gl_static_draw : buffer_usage
  val gl_stream_draw : buffer_usage
  val gl_dynamic_draw : buffer_usage

  type 'a buffer_target
  type array_buffer_t
  type element_array_buffer_t
  val gl_array_buffer : array_buffer_t buffer_target
  val gl_element_array_buffer : element_array_buffer_t buffer_target

  type shader_type
  val gl_fragment_shader : shader_type
  val gl_vertex_shader : shader_type

  type ('a) gl_type
  type sampler
  type int_type = int gl_type
  type bool_type = bool gl_type
  type float_type = float gl_type
  type sampler_type = sampler gl_type
  type attribute_type = float_type

  type uniform_type = UT : 'a gl_type -> uniform_type [@@boxed]

  val sh_int      : int_type
  val sh_int_vec2 : int_type
  val sh_int_vec3 : int_type
  val sh_int_vec4 : int_type

  val sh_bool      : bool_type
  val sh_bool_vec2 : bool_type
  val sh_bool_vec3 : bool_type
  val sh_bool_vec4 : bool_type

  val sh_float      : float_type
  val sh_float_vec2 : float_type
  val sh_float_vec3 : float_type
  val sh_float_vec4 : float_type
  val sh_float_mat2 : float_type
  val sh_float_mat3 : float_type
  val sh_float_mat4 : float_type

  val sh_sampler_2d        : sampler_type
  val sh_sampler_2d_shadow : sampler_type
  val sh_sampler_cube      : sampler_type

  val tysize : 'a gl_type -> int
  val eq_type : 'a gl_type -> 'b gl_type -> bool (* FIXME: use GADT*)
  val neq_type : 'a gl_type -> 'b gl_type -> bool (* FIXME: use GADT*)

  type cap
  val gl_blend : cap
  val gl_cull_face : cap
  val gl_depth_test : cap
  val gl_dither : cap
  val gl_polygon_offset_fill : cap
  val gl_sample_alpha_to_coverage : cap
  val gl_sample_coverage : cap
  val gl_scissor_test : cap
  val gl_stencil_test : cap

  type face
  val gl_front : face
  val gl_back : face
  val gl_front_and_back : face

  type orientation
  val gl_cw : orientation
  val gl_ccw : orientation

  type texture_target
  val gl_texture_2d : texture_target
  val gl_texture_cube_map : texture_target

  type texture_image_target
  val gl_texture_2d_target : texture_image_target
  val gl_texture_cube_map_positive_x : texture_image_target
  val gl_texture_cube_map_negative_x : texture_image_target
  val gl_texture_cube_map_positive_y : texture_image_target
  val gl_texture_cube_map_negative_y : texture_image_target
  val gl_texture_cube_map_positive_z : texture_image_target
  val gl_texture_cube_map_negative_z : texture_image_target

  type wrap_mode
  type 'a filter
  type min
  type mag
  type min_filter = min filter
  type mag_filter = mag filter
  type 'a texture_value

  val gl_repeat : wrap_mode texture_value
  val gl_clamp_to_edge : wrap_mode texture_value

  val gl_nearest : 'a filter texture_value
  val gl_linear  : 'a filter texture_value
  val gl_nearest_mipmap_nearest : min_filter texture_value
  val gl_nearest_mipmap_linear : min_filter texture_value
  val gl_linear_mipmap_nearest : min_filter texture_value
  val gl_linear_mipmap_linear : min_filter texture_value

  type compare_mode
  val gl_compare_none : compare_mode texture_value
  val gl_compare_ref_to_texture : compare_mode texture_value

  type 'a texture_parameter
  val gl_texture_wrap_s : wrap_mode texture_parameter
  val gl_texture_wrap_t : wrap_mode texture_parameter
  val gl_texture_min_filter : min_filter texture_parameter
  val gl_texture_mag_filter : mag_filter texture_parameter
  val gl_texture_compare_mode : compare_mode texture_parameter

  val eq_tex_parameter
      : 'a texture_parameter -> 'b texture_parameter -> ('a,'b) eq_type

  type ('a,'b) imf
  type internal_fmt
  type image_fmt
  type buffer_fmt
  type internal_image_format = (image_fmt, internal_fmt) imf
  type renderbuffer_format = (buffer_fmt, internal_fmt) imf
  type image_format = (image_fmt, image_fmt) imf

  val gl_alpha : (image_fmt,'a) imf
  val gl_rgb : (image_fmt,'a) imf
  val gl_rgba : (image_fmt,'a) imf
  val gl_luminance : (image_fmt,'a) imf
  val gl_luminance_alpha : (image_fmt,'a) imf
  val gl_depth_component16 : ('a, internal_fmt) imf
  val gl_depth_component24 : ('a, internal_fmt) imf
  val gl_depth24_stencil8 : ('a, internal_fmt) imf
  val gl_rgb4 : (buffer_fmt, internal_fmt) imf
  val gl_rgb5_a1 : (buffer_fmt, internal_fmt) imf
  val gl_rgb565 : (buffer_fmt, internal_fmt) imf
  val gl_stencil_index8 : (buffer_fmt, internal_fmt) imf

  type cmp_func
  val gl_never : cmp_func
  val gl_always : cmp_func
  val gl_equal : cmp_func
  val gl_notequal : cmp_func
  val gl_less : cmp_func
  val gl_lequal : cmp_func
  val gl_greater : cmp_func
  val gl_gequal : cmp_func

  type stencil_op
  val gl_keep : stencil_op
  val gl_zero : stencil_op
  val gl_replace : stencil_op
  val gl_invert : stencil_op
  val gl_incr : stencil_op
  val gl_decr : stencil_op
  val gl_incr_wrap : stencil_op
  val gl_decr_wrap : stencil_op

  type blend_mode
  val gl_func_add : blend_mode
  val gl_func_subtract : blend_mode
  val gl_func_reverse_subtract : blend_mode

  type 'a blend_func
  type dst_blend_func = unit blend_func
  type src
  type src_blend_func = src blend_func
  val gl_blend_zero : 'a blend_func
  val gl_blend_one : 'a blend_func
  val gl_src_color : 'a blend_func
  val gl_one_minus_src_color : 'a blend_func
  val gl_dst_color : 'a blend_func
  val gl_one_minus_dst_color : 'a blend_func
  val gl_src_alpha : 'a blend_func
  val gl_one_minus_src_alpha : 'a blend_func
  val gl_dst_alpha : 'a blend_func
  val gl_one_minus_dst_alpha : 'a blend_func
  val gl_constant_color : 'a blend_func
  val gl_one_minus_constant_color : 'a blend_func
  val gl_constant_alpha : 'a blend_func
  val gl_one_minus_constant_alpha : 'a blend_func
  val gl_src_alpha_saturate : src_blend_func

  type which_buffer
  val gl_color_buffer : which_buffer
  val gl_depth_buffer : which_buffer
  val gl_stencil_buffer : which_buffer

  type renderbuffer_target
  val gl_renderbuffer : renderbuffer_target

  type framebuffer_target
  val gl_framebuffer : framebuffer_target
  val gl_read_framebuffer : framebuffer_target
  val gl_draw_framebuffer : framebuffer_target

  type 'a attachment
  type buffer_at
  type frame_at
  type buffer_attachment = buffer_at attachment
  type framebuffer_attachment = frame_at attachment
  val gl_no_buffer : buffer_attachment
  val gl_back_buffer : buffer_attachment
  val gl_color_attachment0 : 'a attachment
  val gl_color_attachment1 : buffer_attachment
  val gl_color_attachment2 : buffer_attachment
  val gl_color_attachment3 : buffer_attachment
  val gl_color_attachment4 : buffer_attachment
  val gl_color_attachment5 : buffer_attachment
  val gl_color_attachment6 : buffer_attachment
  val gl_color_attachment7 : buffer_attachment
  val gl_color_attachment8 : buffer_attachment
  val gl_color_attachment9 : buffer_attachment
  val gl_color_attachment10 : buffer_attachment
  val gl_color_attachment11 : buffer_attachment
  val gl_color_attachment12 : buffer_attachment
  val gl_color_attachment13 : buffer_attachment
  val gl_color_attachment14 : buffer_attachment
  val gl_color_attachment15 : buffer_attachment
  val gl_depth_attachment : framebuffer_attachment
  val gl_stencil_attachment : framebuffer_attachment

  type framebuffer_status
  val gl_framebuffer_complete : framebuffer_status
  val gl_framebuffer_incomplete_attachment : framebuffer_status
  val gl_framebuffer_incomplete_dimensions : framebuffer_status
  val gl_framebuffer_incomplete_missing_attachment : framebuffer_status
  val gl_framebuffer_unsupported : framebuffer_status

  type error
  val gl_no_error : error
  val gl_invalid_enum : error
  val gl_invalid_framebuffer_operation : error
  val gl_invalid_value : error
  val gl_invalid_operation : error
  val gl_out_of_memory : error

  type precision
  val gl_high_float : precision
  val gl_medium_float : precision
  val gl_low_float : precision
  val gl_high_int : precision
  val gl_medium_int : precision
  val gl_low_int : precision

end

open Type
open Bigarray

type byte_bigarray = (int, int8_signed_elt, c_layout) Genarray.t
type ubyte_bigarray = (int, int8_unsigned_elt, c_layout) Genarray.t
type short_bigarray = (int, int16_signed_elt, c_layout) Genarray.t
type ushort_bigarray = (int, int16_unsigned_elt, c_layout) Genarray.t
type uint_bigarray = (int32, int32_elt, c_layout) Genarray.t
type float_bigarray = (float, float32_elt, c_layout) Genarray.t

val create_byte_bigarray : int -> byte_bigarray
val create_ubyte_bigarray : int -> ubyte_bigarray
val create_short_bigarray : int -> short_bigarray
val create_ushort_bigarray : int -> ushort_bigarray
val create_uint_bigarray : int -> uint_bigarray
val create_float_bigarray : int -> float_bigarray

val create_mmapped_byte_bigarray : int -> byte_bigarray
val create_mmapped_ubyte_bigarray : int -> ubyte_bigarray
val create_mmapped_short_bigarray : int -> short_bigarray
val create_mmapped_ushort_bigarray : int -> ushort_bigarray
val create_mmapped_uint_bigarray : int -> uint_bigarray
val create_mmapped_float_bigarray : int -> float_bigarray

(****************************************************************************)
(**  {b VERTEX ATTRIBUTES & DRAWING }                                       *)
(****************************************************************************)

val vertex_attrib_1f : index:int -> float -> unit
val vertex_attrib_2f : index:int -> float -> float -> unit
val vertex_attrib_3f : index:int -> float -> float -> float -> unit
val vertex_attrib_4f : index:int -> float -> float -> float -> float -> unit

val vertex_attrib_fv : index:int -> float array -> unit

val enable_vertex_attrib_array : index:int -> unit
val disable_vertex_attrib_array : index:int -> unit

val vertex_attrib_byte_pointer :
    index:int -> size:int -> ?norm:bool ->
    ?stride:int -> byte_bigarray -> unit

val vertex_attrib_ubyte_pointer :
    index:int -> size:int -> ?norm:bool ->
    ?stride:int -> ubyte_bigarray -> unit

val vertex_attrib_short_pointer :
    index:int -> size:int -> ?norm:bool ->
    ?stride:int -> short_bigarray -> unit

val vertex_attrib_ushort_pointer :
    index:int -> size:int -> ?norm:bool ->
    ?stride:int -> ushort_bigarray -> unit

(* only supported if extension GL_OES_element_index_int is present ?? *)
val vertex_attrib_uint_pointer :
    index:int -> size:int -> ?norm:bool ->
    ?stride:int -> uint_bigarray -> unit

val vertex_attrib_float_pointer :
    index:int -> size:int -> ?norm:bool ->
    ?stride:int -> float_bigarray -> unit

val vertex_attrib_buffer_pointer :
    index:int -> size:int -> typ:storage_type ->
    ?norm:bool -> ?stride:int -> int -> unit

val draw_arrays : shape -> ?first:int -> count:int -> unit

val draw_ubyte_elements : shape -> count:int -> ubyte_bigarray -> unit

val draw_ushort_elements : shape -> count:int -> ushort_bigarray -> unit

(* only supported if extension GL_OES_element_index_int is present *)
val draw_uint_elements : shape -> count:int -> uint_bigarray -> unit

val draw_buffer_elements : shape -> count:int -> typ:storage_type -> int -> unit

(****************************************************************************)
(** {b  BUFFERS }                                                           *)
(****************************************************************************)

type buffer

val null_buffer : buffer
val int_of_buffer : buffer -> int
val buffer_of_int : int -> buffer

val is_buffer : buffer -> bool
val gen_buffer : unit -> buffer
val gen_buffers : int -> buffer array
val delete_buffer : buffer -> unit
val delete_buffers : buffer array -> unit

val bind_buffer : target:'a buffer_target -> buffer -> unit

val buffer_size :
    target:'a buffer_target -> size:int -> usage:buffer_usage -> unit

val buffer_data :
    target:'x buffer_target ->
    ('a, 'b, c_layout) Genarray.t -> usage:buffer_usage -> unit

val buffer_sub_data :
    target:'x buffer_target -> ?offset:int ->
    ('a, 'b, c_layout) Genarray.t -> unit

val get_buffer_size : target:'a buffer_target -> int

val get_buffer_usage : target:'a buffer_target -> buffer_usage

(****************************************************************************)
(** {b  SHADERS}                                                            *)
(****************************************************************************)

type shader

val null_shader : shader
val int_of_shader : shader -> int
val shader_of_int : int -> shader
val is_shader : shader -> bool
val create_shader : shader_type -> shader
val delete_shader : shader -> unit

val shader_source : shader -> string array -> unit
val compile_shader : shader -> unit
val release_shader_compiler : unit -> unit

val get_shader_type : shader -> shader_type
val get_shader_source : shader -> string
val get_shader_info_log : shader -> string
val get_shader_delete_status : shader -> bool
val get_shader_compile_status : shader -> bool

(****************************************************************************)
(** {b PROGRAMS  }                                                          *)
(****************************************************************************)

type program = int

val null_program : program
val int_of_program : program -> int
val program_of_int : int -> program

val is_program : program -> bool
val create_program : unit -> program
val delete_program : program -> unit

val attach_shader : program -> shader -> unit
val detach_shader : program -> shader -> unit

val link_program : program -> unit
val use_program : program -> unit
val validate_program : program -> bool

val get_attached_shaders : program -> shader array
val get_program_info_log : program -> string
val get_program_delete_status : program -> bool
val get_program_link_status : program -> bool


(****************************************************************************)
(**  {b PROGRAMS ATTRIBUTES & UNIFORMS }                                    *)
(****************************************************************************)

val string_of_type : 'a gl_type -> string
val glsl_string_of_type : 'a gl_type -> string

val get_active_attribs : program -> (string * int * attribute_type * int) list
val get_attrib_location : program -> string -> int
val bind_attrib_location : program -> int -> string -> unit

val get_active_uniforms : program -> (string * int * uniform_type * int) list
val get_uniform_location : program -> string -> int

(****************************************************************************)
(** {b  UNIFORMS}                                                           *)
(****************************************************************************)

val uniform_1i : loc:int -> int -> unit
val uniform_2i : loc:int -> int -> int -> unit
val uniform_3i : loc:int -> int -> int -> int -> unit
val uniform_4i : loc:int -> int -> int -> int -> int -> unit

val uniform_1iv : loc:int -> ?count:int -> int array -> unit
val uniform_2iv : loc:int -> ?count:int -> int array -> unit
val uniform_3iv : loc:int -> ?count:int -> int array -> unit
val uniform_4iv : loc:int -> ?count:int -> int array -> unit

val uniform_1b : loc:int -> bool -> unit
val uniform_2b : loc:int -> bool -> bool -> unit
val uniform_3b : loc:int -> bool -> bool -> bool -> unit
val uniform_4b : loc:int -> bool -> bool -> bool -> bool -> unit

val uniform_1bv : loc:int -> ?count:int -> bool array -> unit
val uniform_2bv : loc:int -> ?count:int -> bool array -> unit
val uniform_3bv : loc:int -> ?count:int -> bool array -> unit
val uniform_4bv : loc:int -> ?count:int -> bool array -> unit

val uniform_1f : loc:int -> float -> unit
val uniform_2f : loc:int -> float -> float -> unit
val uniform_3f : loc:int -> float -> float -> float -> unit
val uniform_4f : loc:int -> float -> float -> float -> float -> unit

val uniform_1fv : loc:int -> ?count:int -> float array -> unit
val uniform_2fv : loc:int -> ?count:int -> float array -> unit
val uniform_3fv : loc:int -> ?count:int -> float array -> unit
val uniform_4fv : loc:int -> ?count:int -> float array -> unit

val uniform_matrix_2fv :
    loc:int -> ?count:int -> ?transp:bool -> float array -> unit

val uniform_matrix_3fv :
    loc:int -> ?count:int -> ?transp:bool -> float array -> unit

val uniform_matrix_4fv :
    loc:int -> ?count:int -> ?transp:bool -> float array -> unit

(****************************************************************************)
(**   {b RASTERIZATION }                                                    *)
(****************************************************************************)

val depth_range : near:float -> far:float -> unit
val viewport : x:int -> y:int -> w:int -> h:int -> unit

val enable : cap -> unit
val disable : cap -> unit
val is_enabled : cap -> bool

val line_width : float -> unit

val front_face : orientation -> unit
val cull_face : face:face -> unit

val polygon_offset : factor:float -> units:float -> unit

(****************************************************************************)
(**  TEXTURES                                                               *)
(****************************************************************************)

type texture

val null_texture : texture
val int_of_texture : texture -> int
val texture_of_int : int -> texture

val is_texture : texture -> bool
val gen_texture : unit -> texture
val gen_textures : int -> texture array
val delete_texture : texture -> unit
val delete_textures : texture array -> unit
val active_texture : texture -> unit
val bind_texture : target:texture_target -> texture -> unit
val tex_parameter : target:texture_target -> 'a texture_parameter -> 'a texture_value -> unit
val generate_mipmap : target:texture_target -> unit

(****************************************************************************)
(**  {b TEXTURE IMAGES }                                                    *)
(****************************************************************************)

type image = {
    width : int ;
    height : int ;
    format : image_format ;
    data : ubyte_bigarray
  }

val tex_image_2d :
    target:texture_image_target -> ?level:int -> image -> unit

val tex_null_image_2d :
    target:texture_image_target -> ?level:int -> int -> int -> internal_image_format -> unit

val tex_sub_image_2d :
    target:texture_image_target -> ?level:int ->
    ?xoffset:int -> ?yoffset:int -> image -> unit

type rectangle = int * int * int * int  (* x, y, width, height *)

val copy_tex_image_2d :
    target:texture_image_target -> ?level:int ->
    format:image_format -> rectangle -> unit

val copy_tex_sub_image_2d :
    target:texture_image_target -> ?level:int ->
    ?xoffset:int -> ?yoffset:int -> rectangle -> unit

(****************************************************************************)
(**  {b PER-FRAGMENT OPERATIONS }                                           *)
(****************************************************************************)

type rgba = { r : float ; g : float ; b : float ; a : float }

val rgba : r:float -> g:float -> b:float -> a:float -> rgba

val scissor : x:int -> y:int -> w:int -> h:int -> unit

val sample_coverage_aux : float -> invert:bool -> unit

val sample_coverage : ?invert:bool -> float -> unit

val stencil_func : func:cmp_func -> ref:int -> mask:int -> unit

val stencil_func_separate :
    face:face -> func:cmp_func -> ref:int -> mask:int -> unit

val stencil_op :
    sfail:stencil_op ->
    dpfail:stencil_op -> dppass:stencil_op -> unit

val stencil_op_separate :
    face:face -> sfail:stencil_op ->
    dpfail:stencil_op -> dppass:stencil_op -> unit

val depth_func : func:cmp_func -> unit

val blend_equation : blend_mode -> unit

val blend_equation_separate :
    rgb:blend_mode -> alpha:blend_mode -> unit

val blend_func : src:src_blend_func -> dst:dst_blend_func -> unit

val blend_func_separate :
    src_rgb:src_blend_func -> dst_rgb:dst_blend_func ->
    src_alpha:src_blend_func -> dst_alpha:dst_blend_func -> unit

val blend_color : rgba -> unit

(****************************************************************************)
(**  {b WHOLE FRAMEBUFFER OPERATIONS }                                      *)
(****************************************************************************)

val color_mask : red:bool -> green:bool -> blue:bool -> alpha:bool -> unit

val depth_mask : bool -> unit
val stencil_mask : int -> unit

val stencil_mask_separate : face:face -> int -> unit

val clear : which_buffer list -> unit

val clear_color : rgba -> unit
val clear_depth : float -> unit
val clear_stencil : int -> unit

val read_pixels : x:int -> y:int -> image -> unit

(****************************************************************************)
(**  {b RENDERBUFFERS   }                                                   *)
(****************************************************************************)

type renderbuffer

val null_renderbuffer : renderbuffer
val int_of_renderbuffer : renderbuffer -> int
val renderbuffer_of_int : int -> renderbuffer
val is_renderbuffer : renderbuffer -> bool

val gen_renderbuffer : unit -> renderbuffer
val gen_renderbuffers : int -> renderbuffer array

val delete_renderbuffer : renderbuffer -> unit
val delete_renderbuffers : renderbuffer array -> unit

val draw_buffers : buffer_attachment array -> unit

val bind_renderbuffer : target:renderbuffer_target -> renderbuffer -> unit

val renderbuffer_storage :
    target:renderbuffer_target -> format:renderbuffer_format ->
    width:int -> height:int -> unit

(****************************************************************************)
(** {b  FRAMEBUFFERS }                                                      *)
(****************************************************************************)

type framebuffer

val null_framebuffer : framebuffer
val int_of_framebuffer : framebuffer -> int
val framebuffer_of_int : int -> framebuffer
val is_framebuffer : (framebuffer [@untagged]) -> bool

val gen_framebuffer : unit -> framebuffer
val gen_framebuffers : int -> framebuffer array
val delete_framebuffer : framebuffer -> unit
val delete_framebuffers : framebuffer array -> unit

val bind_framebuffer : target:framebuffer_target -> framebuffer -> unit

val framebuffer_renderbuffer :
    target:framebuffer_target -> attach:framebuffer_attachment ->
    target2:renderbuffer_target -> renderbuffer -> unit

val framebuffer_texture_2d :
    target:framebuffer_target -> attach:framebuffer_attachment ->
    target2:texture_image_target -> texture -> level:int -> unit

val check_framebuffer_status :
    target:framebuffer_target -> framebuffer_status

(****************************************************************************)
(**  {b MISCELLANEOUS }                                                     *)
(****************************************************************************)

val error_to_string : error -> string
val get_error : unit -> error

(** show all erros (in a loop) and therefore really reset the error flag
    error code is printed together with the given message *)
val show_errors : string -> unit

val get_vendor : unit -> string
val get_renderer : unit -> string
val get_version : unit -> string
val get_shading_language_version : unit -> string

val get_extensions : unit -> string

val flush : unit -> unit
val finish : unit -> unit

type precision_info =
  { min : int; max : int; precision : int }

val get_shader_precision : shader_type -> precision -> precision_info
