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

type gc_texture = {
  tex_index : texture;
}

let gen_gc_texture () =
  let tex = gen_texture () in
  let res = { tex_index = tex } in
  Gc.finalise (fun _ -> delete_texture tex) res;
  res

type gc_renderbuffer = {
  renderbuffer_index : renderbuffer
}

let gen_gc_renderbuffer () =
  let buf = gen_renderbuffer () in
  let res = { renderbuffer_index = buf } in
  Gc.finalise (fun _ -> delete_renderbuffer buf) res;
  res

type gc_framebuffer = {
  framebuffer_index : framebuffer
}

let gen_gc_framebuffer () =
  let buf = gen_framebuffer () in
  let res = { framebuffer_index = buf } in
  Gc.finalise (fun _ -> delete_framebuffer buf) res;
  res
			 
let image_to_texture2d : image -> ?level:int -> texture_parameter list -> gc_texture =
  fun image ?(level=0) ls ->
    let tex = gen_gc_texture () in
    active_texture tex.tex_index;
    bind_texture `texture_2d tex.tex_index;
    let need_mipmap_ref = ref false in
    List.iter (fun param ->
      tex_parameter `texture_2d param;
      need_mipmap_ref := need_mipmap param || !need_mipmap_ref) ls;
    tex_image_2d ~target:`texture_2d ~level image;
    if !need_mipmap_ref then generate_mipmap `texture_2d;
    tex

type framebuffer_texture =
    { tex : gc_texture; framebuffer : gc_framebuffer }
      
let framebuffer_texture width height format ls =
  let render = gen_renderbuffer () in
  bind_renderbuffer `renderbuffer render;
  renderbuffer_storage ~target:`renderbuffer ~format:`depth_component16 ~width ~height;

  let tex = gen_gc_texture () in
  active_texture tex.tex_index;
  bind_texture `texture_2d tex.tex_index;
  List.iter (fun param ->
    tex_parameter `texture_2d param) ls;
  tex_null_image_2d `texture_2d width height format;

  let buf = gen_gc_framebuffer () in
  bind_framebuffer `framebuffer buf.framebuffer_index;
  framebuffer_texture_2d ~target:`framebuffer ~attach:`color_attachment0
    ~target2:`texture_2d tex.tex_index ~level:0;
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
  let res = {tex; framebuffer = buf} in
  Gc.finalise (fun _ -> delete_renderbuffer render) res;
  res

let framebuffer_depth_texture width height format ls =
  let tex = gen_gc_texture () in
  active_texture tex.tex_index;
  bind_texture `texture_2d tex.tex_index;
  List.iter (fun param ->
    tex_parameter `texture_2d param) ls;
  tex_null_image_2d `texture_2d width height format;

  let buf = gen_gc_framebuffer () in
  bind_framebuffer `framebuffer buf.framebuffer_index;
  framebuffer_texture_2d ~target:`framebuffer ~attach:`depth_attachment
    ~target2:`texture_2d tex.tex_index ~level:0;
  draw_buffers [|`none|];

  let status = check_framebuffer_status `framebuffer in
  (match status with
    `framebuffer_complete -> ()
  | `framebuffer_incomplete_attachment -> failwith "incomplete attachement"
  | `framebuffer_incomplete_dimensions -> failwith "incomplete dimensions"
  | `framebuffer_incomplete_missing_attachment -> failwith "missing attachement"
  | `framebuffer_unsupported -> failwith "unsupported");

  bind_framebuffer `framebuffer null_framebuffer;
  {tex; framebuffer = buf}

  
