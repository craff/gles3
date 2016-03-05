open Gles3

(** highlevel functions to initialise texture (only
    texture 2D are supported yet) *)

type ntexture = {
  index : texture;
  n : int
}

val image_to_texture2d : image -> ?level:int -> texture_parameter list -> ntexture
(** transform an image into a 2D texture *)

val frame_buffer_texture : int -> int -> internal_image_format -> texture_parameter list ->
  ntexture * framebuffer

val frame_buffer_depth_texture : int -> int -> internal_image_format -> texture_parameter list ->
  ntexture * framebuffer
