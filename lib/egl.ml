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
    ('a -> 'b) -> config -> int -> int -> string -> unit
    = "ml_egl_initialize"

external make_current : unit -> unit = "ml_egl_make_current"

let no_callback _ = assert false

let initialize ?(config=default_config) ~width ~height name =
  initialize_aux no_callback config width height name

external terminate : unit -> unit = "ml_egl_terminate"

external swap_buffers : unit -> unit = "ml_egl_swap_buffers"

external query_version : unit -> string = "ml_egl_query_version"
external query_vendor : unit -> string = "ml_egl_query_vendor"
external query_extensions : unit -> string = "ml_egl_query_extensions"
external query_client_apis : unit -> string = "ml_egl_query_client_apis"
external query_config : unit -> config = "ml_egl_query_config"

(****************************************************************************)
(*   MAIN EVENT LOOP                                                        *)
(****************************************************************************)

external main_loop : unit -> unit = "ml_egl_main_loop"
external exit_loop : unit -> unit = "ml_egl_exit_loop"

(****************************************************************************)
(*   SETTING CALLBACKS                                                      *)
(****************************************************************************)

external set_idle_callback : (unit -> unit) -> unit
  = "ml_egl_set_idle_callback"

external unset_idle_callback : unit -> unit
  = "ml_egl_unset_idle_callback"

external set_reshape_callback : (width:int -> height:int -> unit) -> unit
  = "ml_egl_set_reshape_callback"

external set_delete_callback : (unit -> unit) -> unit
  = "ml_egl_set_delete_callback"

external set_key_press_callback
         : (key:Key.t -> state:Modifier.t -> x:int -> y:int -> unit) -> unit
  = "ml_egl_set_key_press_callback"

external set_key_release_callback
         : (key:Key.t -> state:Modifier.t -> x:int -> y:int -> unit) -> unit
  = "ml_egl_set_key_release_callback"

external set_button_press_callback
         : (button:Button.t -> state:Modifier.t
            -> x:int -> y:int -> unit) -> unit
  = "ml_egl_set_button_press_callback"

external set_button_release_callback
         : (button:Button.t -> state:Modifier.t
            -> x:int -> y:int -> unit) -> unit
  = "ml_egl_set_button_release_callback"

external set_motion_notify_callback
         : (state:int -> x:int -> y:int -> unit) -> unit
  = "ml_egl_set_motion_notify_callback"
