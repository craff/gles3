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

let projection ratio camera =
  Matrix.mul
    (Matrix.perspective camera.focal ratio camera.near camera.far)
    (Matrix.lookat camera.position
                   (Vector.add camera.position camera.forward)
                   camera.up)

let rotate_right camera angle =
  let forward = Vector.comblin (sin angle) camera.right
                               (cos angle) camera.forward in
  let right   = Vector.comblin (cos angle) camera.right
                            (-. sin angle) camera.forward in
  camera.right   <- right;
  camera.forward <- forward

let tilt_right camera angle =
  let up    = Vector.comblin (sin angle) camera.right
                             (cos angle) camera.up in
  let right = Vector.comblin (cos angle) camera.right
                          (-. sin angle) camera.up in
  camera.right <- right;
  camera.up    <- up

let rotate_up camera angle =
  let forward = Vector.comblin (sin angle) camera.up
                               (cos angle) camera.forward in
  let up      = Vector.comblin (cos angle) camera.up
                            (-. sin angle) camera.forward in
  camera.up      <- up;
  camera.forward <- forward

let advance camera x =
  let position = Vector.comblin 1.0 camera.position x camera.forward in
  camera.position <- position

let update camera =
  let t = Unix.gettimeofday () in
  let dt = t -. camera.last_update in
  camera.last_update <- t;
  advance      camera (dt *. camera.speed  );
  rotate_right camera (dt *. camera.r_speed);
  tilt_right   camera (dt *. camera.t_speed);
  rotate_up    camera (dt *. camera.u_speed)

let new_camera ?(position=[|0.0;0.0;0.0|])
               ?(forward =[|1.0;0.0;0.0|])
               ?(right   =[|0.0;1.0;0.0|])
               ?(up      =[|0.0;0.0;1.0|])
               ?(speed   = 0.0)
               ?(r_speed = 0.0)
               ?(t_speed = 0.0)
               ?(u_speed = 0.0)
               ?(near    = 0.1)
               ?(far     = 10.)
               ?(focal   = 45.)
               ()
  = { position; forward; right; up
    ; speed; r_speed; t_speed; u_speed
    ; near ; far
    ; focal
    ; last_update = Unix.gettimeofday () }
