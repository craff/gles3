open Gles3

let to_byte_bigarray a =
  let res = create_byte_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Array1.set res i x) a;
  res
let to_ubyte_bigarray a =
  let res = create_ubyte_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Array1.set res i x) a;
  res
let to_short_bigarray a =
  let res = create_short_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Array1.set res i x) a;
  res
let to_ushort_bigarray a =
  let res = create_ushort_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Array1.set res i x) a;
  res
let to_uint_bigarray a =
  let res = create_uint_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Array1.set res i (Int32.of_int x)) a;
  res
let to_float_bigarray a =
  let res = create_float_bigarray (Array.length a) in
  Array.iteri (fun i x -> Bigarray.Array1.set res i x) a;
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

let to_array_buffer fn ty usage a =
  let size = Array.length a in
  let index = gen_buffer () in
  let a = fn a in
  let t : [`array_buffer] = `array_buffer in
  let target = (t:>buffer_target) in
  bind_buffer target index;
  buffer_size ~target ~size ~usage;
  buffer_data ~target a ~usage;
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
