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
(* textures.mli: interface of Gles3 library                                 *)
(****************************************************************************)

open Gles3

(** highlevel functions to initialise texture (only
    texture 2D are supported yet) *)

type gc_texture = {
  tex_index : texture;
}

type gc_framebuffer = {
  framebuffer_index : framebuffer
}

type gc_renderbuffer = {
  renderbuffer_index : renderbuffer
}

val gen_gc_texture : unit -> gc_texture
val gen_gc_framebuffer	: unit -> gc_framebuffer
val gen_gc_renderbuffer	: unit -> gc_renderbuffer
				    
val image_to_texture2d : image -> ?level:int -> texture_parameter list -> gc_texture
(** transform an image into a 2D texture *)

type framebuffer_texture =
    { tex : gc_texture; framebuffer : gc_framebuffer }

val framebuffer_texture : int -> int -> internal_image_format -> texture_parameter list ->
  framebuffer_texture

val framebuffer_depth_texture : int -> int -> internal_image_format -> texture_parameter list ->
  framebuffer_texture
