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

val sub : t -> t -> t
val add : t -> t -> t
val add_alpha : t -> float -> t -> t
val addq : t -> t -> unit
val addq_alpha : t -> float -> t -> unit
val norm2 : t -> float
val norm : t -> float
val dist : t -> t -> float
val mul : float -> t -> t
val normalize : t -> t
val dot : t -> t -> float
