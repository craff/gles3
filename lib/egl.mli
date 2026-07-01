(****************************************************************************)
(* MLGles3: OpenGL ES3 interface for Objective Caml                         *)
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
(* egl.mli: interface of Egl companion library                              *)
(****************************************************************************)

(** EGL + X11.
    It constists of functions to open a window, read mouse and keyboard, ...
    should be ported to other platforms ... *)

type egl_context

type config = {
    red_size : int ;
    green_size : int ;
    blue_size : int ;
    alpha_size : int ;
    depth_size : int ;
    stencil_size : int ;
    samples : int
  }
(** configuration of openGL window buffer *)

(****************************************************************************)
(**  {b SETUP}                                                              *)
(****************************************************************************)

val initialize : ?config:config -> width:int -> height:int -> string -> egl_context

(** make the context current for GLes drawing *)
val make_current : egl_context -> unit

(** necessary if you want to use the same context in another domain (thread) *)
val detach : egl_context -> unit

(** deallocate the context *)
val terminate : egl_context-> unit

val swap_buffers : egl_context -> unit

val query_version : egl_context -> string
val query_vendor : egl_context -> string
val query_extensions : egl_context -> string
val query_client_apis : egl_context -> string
val query_config : egl_context -> config

(****************************************************************************)
(**  {b MAIN EVENT LOOP}                                                    *)
(****************************************************************************)

(* Enter the main event loop.  This function returns when either:
   - The function [exit_loop] (see below) is called from a callback
   - The Window Manager requires the window deletion and there is
     no [delete_callback] to intercept it. *)

val main_loop : egl_context -> unit

(* Can only be called from a callback: *)
val exit_loop : egl_context -> unit

(****************************************************************************)
(**  {b SETTING CALLBACKS}                                                  *)
(****************************************************************************)

val set_idle_callback :
    egl_context -> (unit -> unit) -> unit

val unset_idle_callback :
    egl_context -> unit

val set_reshape_callback :
  egl_context -> (width:int -> height:int -> unit) -> unit

val set_delete_callback :
  egl_context -> (unit -> unit) -> unit

val set_key_press_callback :
  egl_context -> (key:Key.t -> state:Modifier.t -> x:int -> y:int -> unit) -> unit

val set_key_release_callback :
  egl_context -> (key:Key.t -> state:Modifier.t -> x:int -> y:int -> unit) -> unit

val set_button_press_callback :
  egl_context -> (button:Button.t -> state:Modifier.t -> x:int -> y:int -> unit) -> unit

val set_button_release_callback :
  egl_context -> (button:Button.t -> state:Modifier.t -> x:int -> y:int -> unit) -> unit

val set_motion_notify_callback :
  egl_context -> (state:int -> x:int -> y:int -> unit) -> unit
