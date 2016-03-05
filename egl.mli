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
(* egl.mli: interface of Egl companion library                              *)
(****************************************************************************)

(** EGL + X11.
    It constists of functions to open a window, read mouse and keyboard, ...
    should be ported to other platforms ... *)

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

type button =
  | Button1  (* Left button *)
  | Button2  (* Middle button (scroll wheel button) *)
  | Button3  (* Right button *)
  | Button4  (* Scroll wheel up *)
  | Button5  (* Scroll wheel down *)
(** mouse buttons *)

type keysym = int
(** No interpretation of the keysym yet *)

(** Modifier/button states are bitwise or's of the following: *)

val mask_Shift : int
val mask_Lock : int
val mask_Control : int
val mask_Mod1 : int
val mask_Mod2 : int
val mask_Mod3 : int
val mask_Mod4 : int
val mask_Mod5 : int
val mask_Button1 : int
val mask_Button2 : int
val mask_Button3 : int
val mask_Button4 : int
val mask_Button5 : int

(****************************************************************************)
(**  {b SETUP}                                                              *)
(****************************************************************************)

val initialize : ?config:config -> width:int -> height:int -> string -> unit
external terminate : unit -> unit = "ml_gles2x_terminate"

external swap_buffers : unit -> unit = "ml_gles2x_swap_buffers"

external query_version : unit -> string = "ml_gles2x_query_version"
external query_vendor : unit -> string = "ml_gles2x_query_vendor"
external query_extensions : unit -> string = "ml_gles2x_query_extensions"
external query_client_apis : unit -> string = "ml_gles2x_query_client_apis"
external query_config : unit -> config = "ml_gles2x_query_config"

(****************************************************************************)
(**  {b MAIN EVENT LOOP}                                                    *)
(****************************************************************************)

(* Enter the main event loop.  This function returns when either:
   - The function [exit_loop] (see below) is called from a callback
   - The Window Manager requires the window deletion and there is
     no [delete_callback] to intercept it. *)

external main_loop : unit -> unit = "ml_gles2x_main_loop"

(* Can only be called from a callback: *)
external exit_loop : unit -> unit = "ml_gles2x_exit_loop"

(****************************************************************************)
(**  {b SETTING CALLBACKS}                                                  *)
(****************************************************************************)

external set_idle_callback :
    (unit -> unit) -> unit
	= "ml_gles2x_set_idle_callback"

external set_reshape_callback :
    (width:int -> height:int -> unit) -> unit
	= "ml_gles2x_set_reshape_callback"

external set_delete_callback :
    (unit -> unit) -> unit
	= "ml_gles2x_set_delete_callback"

external set_key_press_callback :
    (key:keysym -> state:int -> x:int -> y:int -> unit) -> unit
	= "ml_gles2x_set_key_press_callback"

external set_key_release_callback :
    (key:keysym -> state:int -> x:int -> y:int -> unit) -> unit
	= "ml_gles2x_set_key_release_callback"

external set_button_press_callback :
    (button:button -> state:int -> x:int -> y:int -> unit) -> unit
	= "ml_gles2x_set_button_press_callback"

external set_button_release_callback :
    (button:button -> state:int -> x:int -> y:int -> unit) -> unit
	= "ml_gles2x_set_button_release_callback"

external set_motion_notify_callback :
    (state:int -> x:int -> y:int -> unit) -> unit
	= "ml_gles2x_set_motion_notify_callback"
