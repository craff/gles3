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
(* camera.ml: interface of Gles3 library                                    *)

type camera = {
    mutable position : float array;
    mutable forward  : float array;
    mutable up       : float array;
    mutable right    : float array;
    mutable speed    : float;
    mutable r_speed  : float;
    mutable u_speed  : float;
    mutable t_speed  : float;
    mutable last_update : float;
    mutable focal    : float;
    mutable near     : float;
    mutable far      : float;
  }

val projection : float -> camera -> Matrix.matrix

val rotate_right : camera -> float -> unit

val tilt_right : camera -> float -> unit

val rotate_up : camera -> float -> unit

val advance : camera -> float -> unit

val update : camera -> unit

val new_camera :
           ?position:float array ->
           ?forward:float array ->
           ?right:float array ->
           ?up:float array ->
           ?speed:float ->
           ?r_speed:float ->
           ?t_speed:float ->
           ?u_speed:float ->
           ?near:float ->
           ?far:float ->
           ?focal:float ->
           unit -> camera
