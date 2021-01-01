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
open Gles3.Type

type texture_parameter_binding =
  TPB : 'a texture_parameter * 'a texture_value -> texture_parameter_binding

let texture_wrap_s x = TPB(gl_texture_wrap_s, x)
let texture_wrap_t x = TPB(gl_texture_wrap_t, x)
let texture_min_filter x = TPB(gl_texture_min_filter, x)
let texture_mag_filter x = TPB(gl_texture_mag_filter, x)
let texture_compare_mode x = TPB(gl_texture_compare_mode, x)

let need_mipmap : texture_parameter_binding -> bool =
  let fn : min_filter texture_value -> bool = fun x ->
    if x = gl_nearest || x = gl_linear then false else true
  in
  function TPB(p, x) ->
    match eq_tex_parameter gl_texture_min_filter p with
    | True -> fn x
    | False -> false

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

let image_to_texture2d : image -> ?level:int -> texture_parameter_binding list -> gc_texture =
  fun image ?(level=0) ls ->
    let tex = gen_gc_texture () in
    active_texture tex.tex_index;
    bind_texture gl_texture_2d tex.tex_index;
    let need_mipmap_ref = ref false in
    List.iter (function TPB(param,v) as p ->
      tex_parameter gl_texture_2d param v;
      need_mipmap_ref := need_mipmap p || !need_mipmap_ref) ls;
    tex_image_2d ~target:gl_texture_2d_target ~level image;
    if !need_mipmap_ref then generate_mipmap gl_texture_2d;
    tex

type framebuffer_texture =
    { tex : gc_texture; framebuffer : gc_framebuffer }

let framebuffer_texture width height format ls =
  let render = gen_renderbuffer () in
  bind_renderbuffer gl_renderbuffer render;
  renderbuffer_storage ~target:gl_renderbuffer ~format:gl_depth_component16
    ~width ~height;
  let tex = gen_gc_texture () in
  active_texture tex.tex_index;
  bind_texture gl_texture_2d tex.tex_index;
  List.iter (function TPB(param,v) ->
    tex_parameter gl_texture_2d param v) ls;
  tex_null_image_2d gl_texture_2d_target width height format;

  let buf = gen_gc_framebuffer () in
  bind_framebuffer gl_framebuffer buf.framebuffer_index;
  framebuffer_texture_2d ~target:gl_framebuffer ~attach:gl_color_attachment0
    ~target2:gl_texture_2d_target tex.tex_index ~level:0;
  framebuffer_renderbuffer ~target:gl_framebuffer ~attach:gl_depth_attachment
    ~target2:gl_renderbuffer render;

  let status = check_framebuffer_status gl_framebuffer in
  (match status with
    x when x = gl_framebuffer_complete -> ()
  | x when x = gl_framebuffer_incomplete_attachment -> failwith "incomplete attachement"
  | x when x = gl_framebuffer_incomplete_dimensions -> failwith "incomplete dimensions"
  | x when x = gl_framebuffer_incomplete_missing_attachment -> failwith "missing attachement"
  | x when x = gl_framebuffer_unsupported -> failwith "unsupported"
  | _ -> failwith "unknown attachment status");

  bind_framebuffer gl_framebuffer null_framebuffer;
  let res = {tex; framebuffer = buf} in
  Gc.finalise (fun _ -> delete_renderbuffer render) res;
  res

let framebuffer_depth_texture width height format ls =
  let tex = gen_gc_texture () in
  active_texture tex.tex_index;
  bind_texture gl_texture_2d tex.tex_index;
  List.iter (function TPB(param,v) ->
    tex_parameter gl_texture_2d param v) ls;
  tex_null_image_2d gl_texture_2d_target width height format;

  let buf = gen_gc_framebuffer () in
  bind_framebuffer gl_framebuffer buf.framebuffer_index;
  framebuffer_texture_2d ~target:gl_framebuffer ~attach:gl_depth_attachment
    ~target2:gl_texture_2d_target tex.tex_index ~level:0;
  draw_buffers [| gl_no_buffer |];

  let status = check_framebuffer_status gl_framebuffer in
  (match status with
    x when x = gl_framebuffer_complete -> ()
  | x when x = gl_framebuffer_incomplete_attachment -> failwith "incomplete attachement"
  | x when x = gl_framebuffer_incomplete_dimensions -> failwith "incomplete dimensions"
  | x when x = gl_framebuffer_incomplete_missing_attachment -> failwith "missing attachement"
  | x when x = gl_framebuffer_unsupported -> failwith "unsupported"
  | _ -> failwith "unknown attachment status");

  bind_framebuffer gl_framebuffer null_framebuffer;
  {tex; framebuffer = buf}
