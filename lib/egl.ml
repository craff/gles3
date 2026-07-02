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
(* egl.ml: implementation of Egl companion library                          *)
(****************************************************************************)

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
    config -> int -> int -> string -> egl_context
    = "ml_egl_initialize"

external make_current : egl_context -> unit = "ml_egl_make_current" [@@noalloc]
external detach : egl_context -> unit = "ml_egl_detach" [@@noalloc]

let initialize ?(config=default_config) ~width ~height name =
  initialize_aux config width height name

external terminate : egl_context -> unit = "ml_egl_terminate"

external swap_buffers : egl_context -> unit = "ml_egl_swap_buffers"

external query_version : egl_context -> string = "ml_egl_query_version"
external query_vendor : egl_context -> string = "ml_egl_query_vendor"
external query_extensions : egl_context -> string = "ml_egl_query_extensions"
external query_client_apis : egl_context -> string = "ml_egl_query_client_apis"
external query_config : egl_context -> config = "ml_egl_query_config"

(****************************************************************************)
(*   MAIN EVENT LOOP                                                        *)
(****************************************************************************)

external main_loop : egl_context -> unit = "ml_egl_main_loop"
external exit_loop : egl_context -> unit = "ml_egl_exit_loop"

(****************************************************************************)
(*   SETTING CALLBACKS                                                      *)
(****************************************************************************)

external set_idle_callback : egl_context -> (unit -> unit) -> unit
  = "ml_egl_set_idle_callback"

external unset_idle_callback : egl_context -> unit
  = "ml_egl_unset_idle_callback"

external set_reshape_callback : egl_context -> (width:int -> height:int -> unit) -> unit
  = "ml_egl_set_reshape_callback"

external set_delete_callback : egl_context -> (unit -> unit) -> unit
  = "ml_egl_set_delete_callback"

external set_key_press_callback
         : egl_context -> (key:Key.t -> state:Modifier.t -> x:int -> y:int -> unit) -> unit
  = "ml_egl_set_key_press_callback"

external set_key_release_callback
         : egl_context -> (key:Key.t -> state:Modifier.t -> x:int -> y:int -> unit) -> unit
  = "ml_egl_set_key_release_callback"

external set_button_press_callback
         : egl_context -> (button:Button.t -> state:Modifier.t
            -> x:int -> y:int -> unit) -> unit
  = "ml_egl_set_button_press_callback"

external set_button_release_callback
         : egl_context -> (button:Button.t -> state:Modifier.t
            -> x:int -> y:int -> unit) -> unit
  = "ml_egl_set_button_release_callback"

external set_motion_notify_callback
         : egl_context -> (state:int -> x:int -> y:int -> unit) -> unit
  = "ml_egl_set_motion_notify_callback"
