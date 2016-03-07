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
(* shaders.mli: interface of Gles3 library                                  *)
(****************************************************************************)

open Gles3
open Buffers
open Textures

(** High-level functions to manage shaders *)

(** A record type to hold a shader *)
type shader = { name : string; ty : shader_type; src : string; }

val load_shader : shader_type -> string -> shader

type 'a program

val compile : ?version:string -> ?precision:string ->string * shader list -> unit program
(** [compile (name, shaders)] compile the givens shaders,
    the [name] is just for clear error messages. *)

(** Functions to call a shader, after instanciating all its variables
    It produces a Failure exception giving the undefined variable if
    it is not the case. *)
val draw_arrays : 'a program -> shape -> ?first:int -> int -> 'a
val draw_ushort_elements : 'a program -> shape -> ushort_bigarray -> 'a
val draw_ubyte_elements : 'a program -> shape -> ubyte_bigarray -> 'a
val draw_uint_elements : 'a program -> shape -> uint_bigarray -> 'a
val draw_buffer_elements : 'a program -> shape -> 'b element_buffer -> 'a

(** Functions to parametrize a shader by an attribute variable, the
    [norm] and [stride] parameters are still fixied. No version is
    provided for buffer as you should probably use dynamic buffers
    ?  *)
val byte_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> (byte_bigarray -> 'a) program
val ubyte_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> (ubyte_bigarray -> 'a) program
val short_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> (short_bigarray -> 'a) program
val ushort_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> (ushort_bigarray -> 'a) program
val uint_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> (uint_bigarray -> 'a) program
val float_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> (float_bigarray -> 'a) program

(** Functions to give a fixed value to an attribute variable *)
val byte_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> byte_bigarray -> 'a program
val ubyte_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> ubyte_bigarray -> 'a program
val short_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> short_bigarray -> 'a program
val ushort_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> ushort_bigarray -> 'a program
val uint_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> uint_bigarray -> 'a program
val float_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> float_bigarray -> 'a program
val buffer_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string -> 'b array_buffer -> 'a program

val int1_uniform : 'a program -> string -> (int -> 'a) program
val bool1_uniform : 'a program -> string -> (bool -> 'a) program
val float1_uniform : 'a program -> string -> (float -> 'a) program
val int2_uniform : 'a program -> string -> (int -> int -> 'a) program
val bool2_uniform : 'a program -> string -> (bool -> bool -> 'a) program
val float2_uniform : 'a program -> string -> (float -> float -> 'a) program
val int3_uniform : 'a program -> string -> (int -> int -> int -> 'a) program
val bool3_uniform : 'a program -> string -> (bool -> bool -> bool -> 'a) program
val float3_uniform : 'a program -> string -> (float -> float -> float -> 'a) program
val int4_uniform : 'a program -> string -> (int -> int -> int -> int -> 'a) program
val bool4_uniform : 'a program -> string -> (bool -> bool -> bool -> bool -> 'a) program
val float4_uniform : 'a program -> string -> (float -> float -> float -> float -> 'a) program

val int1_cst_uniform : 'a program -> string -> int -> 'a program
val bool1_cst_uniform : 'a program -> string -> bool -> 'a program
val float1_cst_uniform : 'a program -> string -> float -> 'a program
val int2_cst_uniform : 'a program -> string -> int -> int -> 'a program
val bool2_cst_uniform : 'a program -> string -> bool -> bool -> 'a program
val float2_cst_uniform : 'a program -> string -> float -> float -> 'a program
val int3_cst_uniform : 'a program -> string -> int -> int -> int -> 'a program
val bool3_cst_uniform : 'a program -> string -> bool -> bool -> bool -> 'a program
val float3_cst_uniform : 'a program -> string -> float -> float -> float -> 'a program
val int4_cst_uniform : 'a program -> string -> int -> int -> int -> int -> 'a program
val bool4_cst_uniform : 'a program -> string -> bool -> bool -> bool -> bool -> 'a program
val float4_cst_uniform : 'a program -> string -> float -> float -> float -> float -> 'a program

(** Functions to parametrize a shader by a uniform variable, using array. *)
val int1v_uniform : 'a program -> string -> (int array -> 'a) program
val int2v_uniform : 'a program -> string -> (int array -> 'a) program
val int3v_uniform : 'a program -> string -> (int array -> 'a) program
val int4v_uniform : 'a program -> string -> (int array -> 'a) program
val bool1v_uniform : 'a program -> string -> (bool array -> 'a) program
val bool2v_uniform : 'a program -> string -> (bool array -> 'a) program
val bool3v_uniform : 'a program -> string -> (bool array -> 'a) program
val bool4v_uniform : 'a program -> string -> (bool array -> 'a) program
val float1v_uniform : 'a program -> string -> (float array -> 'a) program
val float2v_uniform : 'a program -> string -> (float array -> 'a) program
val float3v_uniform : 'a program -> string -> (float array -> 'a) program
val float4v_uniform : 'a program -> string -> (float array -> 'a) program
val float_mat2_uniform : 'a program -> string -> (float array -> 'a) program
val float_mat3_uniform : 'a program -> string -> (float array -> 'a) program
val float_mat4_uniform : 'a program -> string -> (float array -> 'a) program
val texture_2d_uniform : 'a program -> string -> (gc_texture -> 'a) program

(** Functions to give a fixed value to a uniform variable, using array. *)
val int1v_cst_uniform : 'a program -> string -> int array -> 'a program
val int2v_cst_uniform : 'a program -> string -> int array -> 'a program
val int3v_cst_uniform : 'a program -> string -> int array -> 'a program
val int4v_cst_uniform : 'a program -> string -> int array -> 'a program
val bool1v_cst_uniform : 'a program -> string -> bool array -> 'a program
val bool2v_cst_uniform : 'a program -> string -> bool array -> 'a program
val bool3v_cst_uniform : 'a program -> string -> bool array -> 'a program
val bool4v_cst_uniform : 'a program -> string -> bool array -> 'a program
val float1v_cst_uniform : 'a program -> string -> float array -> 'a program
val float2v_cst_uniform : 'a program -> string -> float array -> 'a program
val float3v_cst_uniform : 'a program -> string -> float array -> 'a program
val float4v_cst_uniform : 'a program -> string -> float array -> 'a program
val float_mat2_cst_uniform :
  'a program -> string -> float array -> 'a program
val float_mat3_cst_uniform :
  'a program -> string -> float array -> 'a program
val float_mat4_cst_uniform :
  'a program -> string -> float array -> 'a program
val texture_2d_cst_uniform : 'a program -> string -> gc_texture -> 'a program
