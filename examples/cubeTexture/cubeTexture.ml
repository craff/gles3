open Egl
open Gles3
open Gles3.Type
open Shaders
open Buffers
open Matrix
open Textures

(** simple example, using vertex buffers + one simple texture*)

(** keep the current width,height and ratio in a reference *)
let gwidth = ref 800 and gheight = ref 600
let ratio = ref (float !gwidth /. float !gheight)

(** initialization of the main window, and its viewport *)
let ctxt = initialize !gwidth !gheight "test_gles2"
let _ = viewport ~x:0 ~y:0 ~w:!gwidth ~h:!gheight


(** display all available informations about the context *)
let _ =
  Printf.eprintf "Vendor: %s\n%!" (get_vendor ());
  Printf.eprintf "Renderer: %s\n%!" (get_renderer ());
  Printf.eprintf "Version: %s\n%!" (get_version ());
  Printf.eprintf "GLSL Version: %s\n%!" (get_shading_language_version ());
  Printf.eprintf "Extensions: %s\n%!" (get_extensions ())

(* We load and compile our shaders. *)
let prg : unit Shaders.program =
  let open Shaders in
  let vertex   = of_string gl_vertex_shader  Vertex_light.str  in
  let fragment = of_string gl_fragment_shader Fragment_light.str in
  compile ("light_shader", [vertex ; fragment])

(** after compilation, prg : unit program ... It can only be used
   if we set all its uniform and atribute variables *)

(** we define the cube vertices as a buffer.
   notice the flat structure of the array (3 coordinates per point)
   and the repetition of the same points because they will have
   different normals *)
let vertices = to_float_array_buffer gl_static_draw
  [|0.;0.;0.;
    0.;0.;1.;
    0.;1.;1.;
    0.;1.;0.;

    1.;0.;0.;
    1.;0.;1.;
    1.;1.;1.;
    1.;1.;0.;

    0.;0.;0.;
    1.;0.;0.;
    1.;1.;0.;
    0.;1.;0.;

    0.;0.;1.;
    0.;1.;1.;
    1.;1.;1.;
    1.;0.;1.;

    0.;0.;0.;
    0.;0.;1.;
    1.;0.;1.;
    1.;0.;0.;

    0.;1.;0.;
    1.;1.;0.;
    1.;1.;1.;
    0.;1.;1.;
  |]

(** we set the vertices in the shader *)
let prg = buffer_cst_attr prg "in_position" vertices

(** the normals associated to each vertex, in the same orders *)
let normals = to_float_array_buffer gl_static_draw
  [|
    -1.;0.;0.;
    -1.;0.;0.;
    -1.;0.;0.;
    -1.;0.;0.;

    1.;0.;0.;
    1.;0.;0.;
    1.;0.;0.;
    1.;0.;0.;

    0.;0.;-1.;
    0.;0.;-1.;
    0.;0.;-1.;
    0.;0.;-1.;

    0.;0.;1.;
    0.;0.;1.;
    0.;0.;1.;
    0.;0.;1.;

    0.;-1.;0.;
    0.;-1.;0.;
    0.;-1.;0.;
    0.;-1.;0.;

    0.;1.;0.;
    0.;1.;0.;
    0.;1.;0.;
    0.;1.;0.;
  |]

(** we set the normals vertices in the shader *)
let prg = buffer_cst_attr prg "in_normal" normals

(** a very simple 4x4 texture *)
let texture2 = image_to_texture2d
                 (build_image
                    ~width:4 ~height:4 ~format:gl_luminance
                    (to_ubyte_bigarray [|102;153;204;255;
			                 153;204;255;102;
			                 204;255;102;153;
                                         255;102;153;204;
                       |]))
                 [texture_min_filter gl_nearest;
		  texture_mag_filter gl_nearest;
		  texture_wrap_s gl_repeat;
		  texture_wrap_t gl_repeat]

let texture1 =
  (Freetype.texture_of_text
    ~font:"/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf"
    ~size:64 ~alignment:Justify
    "Hello camllers!\n\n\
     Gles3 bindings\n\
     X11+WL backend\n\n\
     By C. Raffalli")

let x0, x1, y0, y1 =
  if texture1.width > texture1.height then
    let d = 0.5 *. (1. -. float texture1.width /. float texture1.height) in
    (0.,  1., d, 1. -. d)
  else
    let d = 0.5 *. (1. -. float texture1.height /. float texture1.width) in
    (d, 1. -. d, 0., 1.)

(** we define the texture coordinates of each vertex
   above 1 is possible as we use repeat *)
let tex_coordinates = to_float_array_buffer gl_static_draw
  [|
    x1;y0;
    x1;y1;
    x0;y1;
    x0;y0;

    x0;y0;
    x0;y1;
    x1;y1;
    x1;y0;

    0.;0.;
    0.;1.;
    1.;1.;
    1.;0.;

    0.;0.;
    0.;1.;
    1.;1.;
    1.;0.;

    x0;y0;
    x0;y1;
    x1;y1;
    x1;y0;

    x1;y0;
    x0;y0;
    x0;y1;
    x1;y1;
  |]

(** we set the corresponding attribute variable in the shader *)
let prg = buffer_cst_attr prg "in_tex_coordinates" tex_coordinates

(** and associated to the corresponding variable *)
let prg = texture_2d_cst_uniform prg "texture1" texture1.texture
let prg = texture_2d_cst_uniform prg "texture2" texture2

(** we define the elements (here 12 triangles), as index in the above array *)
let elements = to_uint_element_buffer gl_static_draw
  [|0;1;2;    2;3;0;
    4;5;6;    6;7;4;
    8;9;10;   10;11;8;
    12;13;14; 14;15;12;
    16;17;18; 18;19;16;
    20;21;22; 22;23;20 |]

(** the modelView matrix of the cube defining the position of the cube,
   from the current time *)
let modelView t =
  mul (rotateY (10.*.t/.11.))
    (mul (rotateZ (6.*.t/.7.)) (translate (-0.5) (-0.5) (-0.5)))

let center = [|0.;0.;0.|]
let lightPos = [|0.0;1.0;4.0|]
let eyePos = [|0.;0.;3.5|]
let eyeUp = [|1.0;1.0;0.0|]

(** the projection matrix: beware, it depends from the screen ratio *)
let projection () =
  (mul (perspective 45.0 !ratio 1. 5.) (lookat eyePos center eyeUp))

(** these varying we tranform the shader program into a function *)
let prg : (float array -> unit) program = float_mat4_uniform prg "ModelView"

let prg : (float array -> float array -> unit) program = float_mat4_uniform prg "Projection"
(** Beware: the first argument in the last to be set, hence here
   the projection matrix comes before the modelView *)

(** we se all the remaning uniform variables about lighting *)
let prg = float4v_cst_uniform prg "color" [|0.3;0.3;1.0;1.0|]
let prg = float1_cst_uniform prg "specular" 0.5
let prg = float1_cst_uniform prg "shininess" 15.
let prg = float3v_cst_uniform prg "lightPos" lightPos
let prg = float4v_cst_uniform prg "lightDiffuse" [|0.7;0.7;0.7;1.0|]
let prg = float4v_cst_uniform prg "lightAmbient" [|0.2;0.2;0.2;1.0|]

(** we can now define a function drawing the cube using
   Shaders.draw_uint_elements *)
let dessine_cube t = draw_buffer_elements prg gl_triangles elements (projection ()) (modelView t)

(** some last initializations of openGL state *)
let _ =
  enable gl_depth_test;
  clear_color { r = 0.1; g = 0.1; b = 0.1; a = 1.0 }

(** two references to compute the frame rates *)
let lasttime = ref (Unix.gettimeofday ())
let frames = ref 0

(** the main drawing function, not mush to say, half of it
   if the computation of the frame rates *)
let draw () =
  clear [ gl_color_buffer ; gl_depth_buffer];
  let t = Unix.gettimeofday () in
  dessine_cube t;
  swap_buffers ctxt;
  show_errors "after draw";
  incr frames;
  let delta = t -. !lasttime in
  if delta > 5.0 then(
    let fps = float !frames /. delta in
    Printf.eprintf "fps: %.2f\n%!" fps;
    frames := 0;
    lasttime := t
  )

(** call back for key and mouse, just for testing *)
let _ = set_key_press_callback ctxt (fun ~key ~state ~x ~y ->
            if key = Key.Escape then exit_loop ctxt;
            Printf.printf "key: %s state: %d x:%d y:%d\n%!"
              (Key.name key) (state :> int) x y)

let _ = set_button_press_callback ctxt (fun ~button ~state ~x ~y ->
            Printf.printf "button: %s state: %d x:%d y:%d\n%!"
              (Button.name button) (state :> int) x y)

(** when there is nothing to do, we draw *)
let _ = set_idle_callback ctxt draw

(** the reshape callback, changing the viewport and ratio
   when the window is resized *)
let _ = set_reshape_callback ctxt (fun ~width ~height ->
  gwidth := width; gheight := height;
  ratio := float width /. float height;
  viewport ~x:0 ~y:0 ~w:width ~h:height)

let _ = draw () (** draw once outsize the loop, because all exceptions are caught
                   inside the main loop *)

(** we now start the main loop ! *)
let _ = main_loop ctxt
