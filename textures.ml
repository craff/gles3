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
(* textures.ml: interface of Gles3 library                                  *)
(****************************************************************************)

open Gles3

let need_mipmap =
  let fn = function
    | `nearest_mipmap_nearest | `nearest_mipmap_linear
    | `linear_mipmap_nearest | `linear_mipmap_linear -> true
    | `nearest | `linear -> false
  in
  function
  | `texture_min_filter x -> fn x
  | _ -> false

let tex_count = ref 0

(* FIXME: Gc.finalise + collect free numbers ... *)
type ntexture = {
  index : texture;
  n : int
}

let image_to_texture2d : image -> ?level:int -> texture_parameter list -> ntexture =
  fun image ?(level=0) ls ->
    let n = !tex_count in
    incr tex_count;
    active_texture n;
    let index = gen_texture () in
    bind_texture `texture_2d index;
    let need_mipmap_ref = ref false in
    List.iter (fun param ->
      tex_parameter `texture_2d param;
      need_mipmap_ref := need_mipmap param || !need_mipmap_ref) ls;
    tex_image_2d ~target:`texture_2d ~level image;
    if !need_mipmap_ref then generate_mipmap `texture_2d;
    { index; n }

let frame_buffer_texture width height format ls =
  let render = gen_renderbuffer () in
  bind_renderbuffer `renderbuffer render;
  renderbuffer_storage ~target:`renderbuffer ~format:`depth_component16 ~width ~height;

  let n = !tex_count in
  incr tex_count;
  active_texture n;
  let tex = gen_texture () in
  bind_texture `texture_2d tex;
  List.iter (fun param ->
    tex_parameter `texture_2d param) ls;
  tex_null_image_2d `texture_2d width height format;

  let buf = gen_framebuffer () in
  bind_framebuffer `framebuffer buf;
  framebuffer_texture_2d ~target:`framebuffer ~attach:`color_attachment0
    ~target2:`texture_2d tex ~level:0;
  framebuffer_renderbuffer ~target:`framebuffer ~attach:`depth_attachment
    ~target2:`renderbuffer render;

  let status = check_framebuffer_status `framebuffer in
  (match status with
    `framebuffer_complete -> ()
  | `framebuffer_incomplete_attachment -> failwith "incomplete attachement"
  | `framebuffer_incomplete_dimensions -> failwith "incomplete dimensions"
  | `framebuffer_incomplete_missing_attachment -> failwith "missing attachement"
  | `framebuffer_unsupported -> failwith "unsupported");

  bind_framebuffer `framebuffer null_framebuffer;

  ({ index=tex; n}, buf)

let frame_buffer_depth_texture width height format ls =
  let n = !tex_count in
  incr tex_count;
  active_texture n;
  let tex = gen_texture () in
  bind_texture `texture_2d tex;
  List.iter (fun param ->
    tex_parameter `texture_2d param) ls;
  tex_null_image_2d `texture_2d width height format;

  let buf = gen_framebuffer () in
  bind_framebuffer `framebuffer buf;
  framebuffer_texture_2d ~target:`framebuffer ~attach:`depth_attachment
    ~target2:`texture_2d tex ~level:0;
  draw_buffers [|`none|];

  let status = check_framebuffer_status `framebuffer in
  (match status with
    `framebuffer_complete -> ()
  | `framebuffer_incomplete_attachment -> failwith "incomplete attachement"
  | `framebuffer_incomplete_dimensions -> failwith "incomplete dimensions"
  | `framebuffer_incomplete_missing_attachment -> failwith "missing attachement"
  | `framebuffer_unsupported -> failwith "unsupported");

  bind_framebuffer `framebuffer null_framebuffer;

  ({ index=tex; n}, buf)
