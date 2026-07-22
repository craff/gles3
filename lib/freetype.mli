type weight =
  | Thin
  | Light
  | Regular
  | Medium
  | Bold
  | Black

type slant =
  | Roman
  | Italic
  | Oblique

val find_font : ?family:string -> ?weight:weight -> ?slant:slant ->
                unit -> string

type bitmap =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
  Bigarray.Genarray.t

type glyph =
{
  left    : int;
  top     : int;
  advance : int;
  space   : bool;
  image : ([`Image | `Texture ], int, Bigarray.int8_unsigned_elt) Gles3.image;
}

val load_glyph
    : font:string -> size:int -> Uchar.t -> glyph

type alignment =
  Left | Center | Right | Justify

type sized_texture =
  { width : int
  ; height : int
  ; texture : Textures.gc_texture }

val texture_of_text
    : font:string -> size:int -> ?line_stretch:float -> ?alignment:alignment
      ->string -> sized_texture
