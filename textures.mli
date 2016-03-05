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

type ntexture = {
  index : texture;
  n : int
}

val image_to_texture2d : image -> ?level:int -> texture_parameter list -> ntexture
(** transform an image into a 2D texture *)

val frame_buffer_texture : int -> int -> internal_image_format -> texture_parameter list ->
  ntexture * framebuffer

val frame_buffer_depth_texture : int -> int -> internal_image_format -> texture_parameter list ->
  ntexture * framebuffer
