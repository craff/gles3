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
(* matrix.ml: interface of Gles3 library                                    *)
(****************************************************************************)

type t = { mutable x : float;
	   mutable y : float;
	   mutable z : float }

let sub {x;y;z} {x=x';y=y';z=z'} =
  {x=x -. x';y= y -. y';z= z -. z'}
let add {x;y;z} {x=x';y=y';z=z'} =
  {x=x +. x'; y=y +. y'; z=z +. z'}
let addq ({x;y;z} as r) {x=x';y=y';z=z'} =
  r.x <- x +. x';
  r.y <- y +. y';
  r.z <- z +. z'
let add_alpha {x;y;z} alpha {x=x';y=y';z=z'} =
  {x = x +. alpha *. x';
   y = y +. alpha *. y';
   z = z +. alpha *. z' }
let addq_alpha ({x;y;z} as r) alpha {x=x';y=y';z=z'} =
  r.x <- x +. alpha *. x';
  r.y <- y +. alpha *. y';
  r.z <- z +. alpha *. z'
let norm2 {x;y;z} =
  x*.x +. y*.y +. z*.z
let norm v =
  sqrt (norm2 v)
let dist v v' =
  norm (sub v v')
let mul a {x;y;z} =
  {x=a *. x; y=a *. y; z=a *. z}
let normalize v =
  mul (1.0 /. norm v) v
let dot {x;y;z} {x=x';y=y';z=z'} =
  x *. x' +. y *. y' +. z *. z'
