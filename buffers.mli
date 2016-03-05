open Gles3

(** {b Function for buffer manipulation} *)

(** Functions to convert array to bigarray *)
val to_byte_bigarray : int array -> byte_bigarray
val to_ubyte_bigarray : int array -> ubyte_bigarray
val to_short_bigarray : int array -> short_bigarray
val to_ushort_bigarray : int array -> ushort_bigarray
val to_uint_bigarray : int array -> uint_bigarray
val to_float_bigarray : float array -> float_bigarray

(** Higher level buffer type *)
type ('a, 'b) buffer = {
  index : Gles3.buffer;
  ty : storage_type;
  usage : buffer_usage;
  target : 'b;
  array : 'a;
  size : int;
}

(** Two types to forbid mixing buffer usage *)
type 'a array_buffer =  ('a , [`array_buffer]) buffer
type 'a element_buffer =  ('a , [`element_array_buffer]) buffer

(** Functions creating uninitialized buffers *)
val create_byte_array_buffer : buffer_usage -> int -> byte_bigarray array_buffer
val create_ubyte_array_buffer : buffer_usage -> int -> ubyte_bigarray array_buffer
val create_short_array_buffer : buffer_usage -> int -> short_bigarray array_buffer
val create_ushort_array_buffer : buffer_usage -> int -> ushort_bigarray array_buffer
val create_uint_array_buffer : buffer_usage -> int -> uint_bigarray array_buffer
val create_float_array_buffer : buffer_usage -> int -> float_bigarray array_buffer

val create_byte_element_buffer : buffer_usage -> int -> byte_bigarray element_buffer
val create_ubyte_element_buffer : buffer_usage -> int -> ubyte_bigarray element_buffer
val create_short_element_buffer : buffer_usage -> int -> short_bigarray element_buffer
val create_ushort_element_buffer : buffer_usage -> int -> ushort_bigarray element_buffer
val create_uint_element_buffer : buffer_usage -> int -> uint_bigarray element_buffer
val create_float_element_buffer : buffer_usage -> int -> float_bigarray element_buffer

(** Functions creating initialized buffers from an array*)
val to_byte_array_buffer : buffer_usage -> int array -> byte_bigarray array_buffer
val to_ubyte_array_buffer : buffer_usage -> int array -> ubyte_bigarray array_buffer
val to_short_array_buffer : buffer_usage -> int array -> short_bigarray array_buffer
val to_ushort_array_buffer : buffer_usage -> int array -> ushort_bigarray array_buffer
val to_uint_array_buffer : buffer_usage -> int array -> uint_bigarray array_buffer
val to_float_array_buffer : buffer_usage -> float array -> float_bigarray array_buffer

val to_byte_element_buffer : buffer_usage -> int array -> byte_bigarray element_buffer
val to_ubyte_element_buffer : buffer_usage -> int array -> ubyte_bigarray element_buffer
val to_short_element_buffer : buffer_usage -> int array -> short_bigarray element_buffer
val to_ushort_element_buffer : buffer_usage -> int array -> ushort_bigarray element_buffer
val to_uint_element_buffer : buffer_usage -> int array -> uint_bigarray element_buffer
val to_float_element_buffer : buffer_usage -> float array -> float_bigarray element_buffer
