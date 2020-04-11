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
(* buffers.ml: interface of Gles3 library                                   *)
(****************************************************************************)

open Gles3

let to_byte_bigarray a =
  let res = create_byte_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Genarray.set res [|i|] x) a;
  res
let to_ubyte_bigarray a =
  let res = create_ubyte_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Genarray.set res [|i|] x) a;
  res
let to_short_bigarray a =
  let res = create_short_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Genarray.set res [|i|] x) a;
  res
let to_ushort_bigarray a =
  let res = create_ushort_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Genarray.set res [|i|] x) a;
  res
let to_uint_bigarray a =
  let res = create_uint_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Genarray.set res [|i|] (Int32.of_int x)) a;
  res
let to_float_bigarray a =
  let res = create_float_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Genarray.set res [|i|] x) a;
  res

type ('a, 'b) buffer = {
  index : Gles3.buffer;
  ty : storage_type;
  usage : buffer_usage;
  target : 'b;
  array : 'a;
  size : int;
}

type 'a array_buffer =  ('a , [`array_buffer]) buffer
type 'a element_buffer =  ('a , [`element_array_buffer]) buffer

let create_array_buffer fn ty usage size =
  let index = gen_buffer () in
  let a = fn size in
  let t : [`array_buffer] = `array_buffer in
  let target = (t:>buffer_target) in
  bind_buffer target index;
  buffer_size ~target ~size ~usage;
  buffer_data ~target a ~usage;
  bind_buffer target null_buffer;
  let res = { index; ty; usage; target=t; array=a; size } in
  Gc.finalise (fun res -> delete_buffer res.index; Printf.eprintf "DELETE BUFFERS\n%!") res;
  res

let create_element_buffer fn ty usage size =
  let index = gen_buffer () in
  let a = fn size in
  let t : [`element_array_buffer] = `element_array_buffer in
  let target = (t:>buffer_target) in
  bind_buffer target index;
  buffer_size ~target ~size ~usage;
  buffer_data ~target a ~usage;
  bind_buffer target null_buffer;
  let res = { index; ty; usage; target=t; array=a; size } in
  Gc.finalise (fun res -> delete_buffer res.index; Printf.eprintf "DELETE BUFFERS\n%!") res;
  res

let create_byte_array_buffer = create_array_buffer create_byte_bigarray `byte
let create_byte_element_buffer = create_element_buffer create_byte_bigarray `byte
let create_ubyte_array_buffer = create_array_buffer create_ubyte_bigarray `ubyte
let create_ubyte_element_buffer = create_element_buffer create_ubyte_bigarray `ubyte
let create_short_array_buffer = create_array_buffer create_short_bigarray `short
let create_short_element_buffer = create_element_buffer create_short_bigarray `short
let create_ushort_array_buffer = create_array_buffer create_ushort_bigarray `ushort
let create_ushort_element_buffer = create_element_buffer create_ushort_bigarray `ushort
let create_uint_array_buffer = create_array_buffer create_uint_bigarray `uint
let create_uint_element_buffer = create_element_buffer create_uint_bigarray `uint
let create_float_array_buffer = create_array_buffer create_float_bigarray `float
let create_float_element_buffer = create_element_buffer create_float_bigarray `float

let create_mmapped_byte_array_buffer = create_array_buffer create_mmapped_byte_bigarray `byte
let create_mmapped_byte_element_buffer = create_element_buffer create_mmapped_byte_bigarray `byte
let create_mmapped_ubyte_array_buffer = create_array_buffer create_mmapped_ubyte_bigarray `ubyte
let create_mmapped_ubyte_element_buffer = create_element_buffer create_mmapped_ubyte_bigarray `ubyte
let create_mmapped_short_array_buffer = create_array_buffer create_mmapped_short_bigarray `short
let create_mmapped_short_element_buffer = create_element_buffer create_mmapped_short_bigarray `short
let create_mmapped_ushort_array_buffer = create_array_buffer create_mmapped_ushort_bigarray `ushort
let create_mmapped_ushort_element_buffer = create_element_buffer create_mmapped_ushort_bigarray `ushort
let create_mmapped_uint_array_buffer = create_array_buffer create_mmapped_uint_bigarray `uint
let create_mmapped_uint_element_buffer = create_element_buffer create_mmapped_uint_bigarray `uint
let create_mmapped_float_array_buffer = create_array_buffer create_mmapped_float_bigarray `float
let create_mmapped_float_element_buffer = create_element_buffer create_mmapped_float_bigarray `float

let to_array_buffer fn ty usage a =
  let size = Array.length a in
  let index = gen_buffer () in
  let a = fn a in
  let t : [`array_buffer] = `array_buffer in
  let target = (t:>buffer_target) in
  bind_buffer target index;
  buffer_size ~target ~size ~usage;
  buffer_data ~target a ~usage;
  bind_buffer target null_buffer;
  let res = { index; ty; usage; target=t; array=a; size } in
  Gc.finalise (fun res -> delete_buffer res.index; Printf.eprintf "DELETE BUFFERS\n%!") res;
  res

let to_element_buffer fn ty usage a =
  let size = Array.length a in
  let index = gen_buffer () in
  let a = fn a in
  let t : [`element_array_buffer] = `element_array_buffer in
  let target = (t:>buffer_target) in
  bind_buffer target index;
  buffer_size ~target ~size ~usage;
  buffer_data ~target a ~usage;
  bind_buffer target null_buffer;
  let res = { index; ty; usage; target=t; array=a; size } in
  Gc.finalise (fun res -> delete_buffer res.index; Printf.eprintf "DELETE BUFFERS\n%!") res;
  res

let to_byte_array_buffer = to_array_buffer to_byte_bigarray `byte
let to_ubyte_array_buffer = to_array_buffer to_ubyte_bigarray `ubyte
let to_short_array_buffer = to_array_buffer to_short_bigarray `short
let to_ushort_array_buffer = to_array_buffer to_ushort_bigarray `ushort
let to_uint_array_buffer = to_array_buffer to_uint_bigarray `uint
let to_float_array_buffer = to_array_buffer to_float_bigarray `float

let to_byte_element_buffer = to_element_buffer to_byte_bigarray `byte
let to_ubyte_element_buffer = to_element_buffer to_ubyte_bigarray `ubyte
let to_short_element_buffer = to_element_buffer to_short_bigarray `short
let to_ushort_element_buffer = to_element_buffer to_ushort_bigarray `ushort
let to_uint_element_buffer = to_element_buffer to_uint_bigarray `uint
let to_float_element_buffer = to_element_buffer to_float_bigarray `float
