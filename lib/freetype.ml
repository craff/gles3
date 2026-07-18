type bitmap =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
  Bigarray.Genarray.t

type glyph_aux =
{
  bitmap  : bitmap;
  width   : int;
  height  : int;
  stride  : int;
  left    : int;
  top     : int;
  advance : int;
}

type glyph =
{
  left    : int;
  top     : int;
  advance : int;
  space   : bool;
  image : ([`Image | `Texture ], int, Bigarray.int8_unsigned_elt) Gles3.image;
}

external init : unit -> unit
  = "ml_ft_init"

let _ = init ()

external load_glyph_aux :
  string -> int -> Uchar.t -> glyph_aux = "ml_ft_load_glyph"

module Key = struct
  type t = (int * Uchar.t)
  let hash = Hashtbl.hash
  let equal = ( = )
end

module FontTbl = Weakvalue.Make(Key)

let fonts = Hashtbl.create 101

let load_glyph ~font =
  let wt =
    try Hashtbl.find fonts font with Not_found ->
      let wt = FontTbl.create 101 in
      Hashtbl.add fonts font wt;
      wt
  in
  fun ~size char ->
  try
    let res = FontTbl.find wt (size, char) in
    res
  with Not_found ->
    let g = load_glyph_aux font size char in
    let alignment =
      let rec fn a =
        if a > 8 then failwith "unsupported alignment";
        if g.stride mod a = 0 && g.stride - g.width < a then a
        else fn (2*a)
      in
      fn 1
    in
    let open Gles3.Type in
    let image =
      Gles3.build_image ~width:g.width ~alignment
        ~height:g.height ~format:gl_luminance
        g.bitmap
    in
    let res =
      { left = g.left; top = g.top; advance = g.advance; image;
        space = (char = Uchar.of_char ' ')
      }
    in
    FontTbl.add wt (size, char) res;
    res

let str_to_uchars str =
  let res = ref [] in
  let index = ref 0 in
  while !index < String.length str do
    let d = String.get_utf_8_uchar str !index in
    if Uchar.utf_decode_is_valid d then
      begin
        res := Uchar.utf_decode_uchar d :: !res;
        index := !index + Uchar.utf_decode_length d;
      end
    else
      let msg =
        Printf.sprintf "invalide unicode char as %d in %s" !index str
      in
      failwith msg
  done;
  !res

type alignment =
  Left | Center | Right | Justify

type sized_texture =
  { width : int
  ; height : int
  ; texture : Textures.gc_texture }

let texture_of_text ~font ~size ?(line_stretch=1.0) ?(alignment=Left) text =
  if text = "" then failwith "texture_of_text: empty";
  let lines = String.split_on_char '\n' text in
  let nb_lines = ref 0 in
  let max_width = ref 0 in
  let do_line str =
    let str = if str = "" then " " else str in
    incr nb_lines;
    let chars = str_to_uchars str in
    let chars = List.rev_map (load_glyph ~font ~size) chars in
    let below = ref 0 in
    let first_left = max 0 (- (List.hd chars).left) in
    let width = ref first_left in
    let pen = ref first_left in
    let spaces = ref (-1) in
    List.iter (fun c ->
        below := max !below (c.image.height + c.top - size);
        width := !pen + c.left + c.image.width;
        spaces := !spaces + if c.space then 10 else 1;
        pen := !pen + c.advance) chars;
    let width = !width in
    let height = size + !below in
    let baseline = !below in
    max_width := max width !max_width;
    (width, height, baseline, !spaces, chars)
  in
  let lines = List.rev_map do_line lines in
  let nb_lines = !nb_lines in
  let (_, hn, bn, _, _) = List.hd lines in
  let line_space = int_of_float (float size *. line_stretch) in
  let text_height = line_space * (nb_lines - 1) + size + bn in
  let text_width = !max_width in
(*  Printf.printf "%dx%d line_space:%d (%d,%d)\n%!"
    text_width text_height line_space hn bn;*)
  let texture = Textures.gen_gc_texture () in
  Gles3.(bind_texture Type.gl_texture_2d texture.tex_index);
  Gles3.(tex_null_image_2d Type.gl_texture_2d_target
           text_width text_height Type.gl_luminance);
  let data = Bigarray.(Genarray.create Int8_unsigned c_layout
                         [|text_height; text_width|])
  in
  Bigarray.Genarray.fill data 0;
  let null_image = Gles3.(build_image
                            ~width:text_width
                            ~height:text_height
                            ~format:Type.gl_luminance
                            data)
  in (*
  List.iteri (fun i (width, height, baseline, spaces, chars) ->
      List.iter (fun c ->
          let yoffset =
            (line_space * i) + baseline
          in
          for i = 0 to text_width - 1 do
            Bigarray.Genarray.set null_image.data [|yoffset; i|] 255;
            Bigarray.Genarray.set null_image.data [|yoffset+1; i|] 255;
            done) chars) lines; *)
  Gles3.(tex_image_2d ~target:Type.gl_texture_2d_target
           null_image);

  List.iteri (fun i (width, height, baseline, spaces, chars) ->
      let space_factor = float (text_width - width) /. float spaces in
      let pen = ref (match alignment with
                     | Left -> 0.0
                     | Center -> float (text_width - width) /. 2.0
                     | Right -> float (text_width - width)
                     | Justify -> 0.0) in
      let first_left = max 0 (- (List.hd chars).left) in
      pen := !pen +. float first_left;
      List.iter (fun c ->
          let xoffset = int_of_float !pen + c.left in
          let yoffset =
            (line_space * i) + baseline + c.top - c.image.height
          in
(*          Printf.printf "%dx%d+%d,%d\n%!" c.image.width c.image.height
            xoffset yoffset;*)
          Gles3.(tex_sub_image_2d ~target:Type.gl_texture_2d_target
                   ~xoffset ~yoffset c.image);
          let extra = match alignment with
            | Justify -> (if c.space then 10. else 1.) *. space_factor
            | _ -> 0.0
          in
          pen := !pen +. extra +. float c.advance) chars) lines;
  Gles3.(Type.(tex_parameter gl_texture_2d gl_texture_wrap_s Type.gl_clamp_to_edge));
  Gles3.(Type.(tex_parameter gl_texture_2d gl_texture_wrap_t Type.gl_clamp_to_edge));

  Gles3.(generate_mipmap Type.gl_texture_2d);
  { width =  text_width
  ; height = text_height
  ; texture }
