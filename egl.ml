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
(* egl.ml: implementation of Egl companion library                          *)
(****************************************************************************)

type config = {
    red_size : int ;
    green_size : int ;
    blue_size : int ;
    alpha_size : int ;
    depth_size : int ;
    stencil_size : int ;
    samples : int
  }

type button =
  | Button1  (* Left button *)
  | Button2  (* Middle button (scroll wheel button) *)
  | Button3  (* Right button *)
  | Button4  (* Scroll wheel up *)
  | Button5  (* Scroll wheel down *)

type keysym = int

(* Modifier/button states are bitwise or's of the following: *)

let mask_Shift   = (1 lsl 0)
let mask_Lock    = (1 lsl 1)
let mask_Control = (1 lsl 2)
let mask_Mod1    = (1 lsl 3)
let mask_Mod2    = (1 lsl 4)
let mask_Mod3    = (1 lsl 5)
let mask_Mod4    = (1 lsl 6)
let mask_Mod5    = (1 lsl 7)
let mask_Button1 = (1 lsl 8)
let mask_Button2 = (1 lsl 9)
let mask_Button3 = (1 lsl 10)
let mask_Button4 = (1 lsl 11)
let mask_Button5 = (1 lsl 12)

let default_config =
  { red_size = 8 ;
    green_size = 8 ;
    blue_size = 8 ;
    alpha_size = 8 ;
    depth_size = 16 ;
    stencil_size = 0 ;
    samples = 0 }

(****************************************************************************)
(*   SETUP                                                                  *)
(****************************************************************************)

external initialize_aux :
    config:config -> width:int -> height:int -> string -> unit
    = "ml_gles2x_initialize"

let initialize ?(config=default_config) ~width ~height name =
  initialize_aux ~config ~width ~height name

external terminate : unit -> unit = "ml_gles2x_terminate"

external swap_buffers : unit -> unit = "ml_gles2x_swap_buffers"

external query_version : unit -> string = "ml_gles2x_query_version"
external query_vendor : unit -> string = "ml_gles2x_query_vendor"
external query_extensions : unit -> string = "ml_gles2x_query_extensions"
external query_client_apis : unit -> string = "ml_gles2x_query_client_apis"
external query_config : unit -> config = "ml_gles2x_query_config"

(****************************************************************************)
(*   MAIN EVENT LOOP                                                        *)
(****************************************************************************)

external main_loop : unit -> unit = "ml_gles2x_main_loop"
external exit_loop : unit -> unit = "ml_gles2x_exit_loop"

(****************************************************************************)
(*   SETTING CALLBACKS                                                      *)
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
