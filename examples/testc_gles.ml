open Egl
open Gles3
open Shaders
open Buffers
open Matrix
open Textures

(** simple example, using vertex buffers + one simple texture*)

(** keep the current width,height and ratio in a reference *)
let gwidth = ref 800 and gheight = ref 600
let ratio = ref (float !gwidth /. float !gheight)
(** initialization of the main window, and its viewport *)
let _ = initialize !gwidth !gheight "test_gles2";
  viewport ~x:0 ~y:0 ~w:!gwidth ~h:!gheight

(** display all available informations about the context *)
let _ =
  Printf.eprintf "Vendor: %s\n%!" (get_vendor ());
  Printf.eprintf "Renderer: %s\n%!" (get_renderer ());
  Printf.eprintf "Version: %s\n%!" (get_version ());
  Printf.eprintf "GLSL Version: %s\n%!" (get_shading_language_version ());
  Printf.eprintf "Extensions: %s\n%!" (get_extensions ())

let shadow_shader =
  ("shadow_shader",
   [ load_shader `vertex_shader   "vertex_shadow.glsl";
     load_shader `fragment_shader "fragment_shadow.glsl"; ])


let shade = compile shadow_shader

(** we define our shaders, with the type expected by Shaders.compile.
   the string are just use to report errors *)
let light_shader =
  ("light_shader",
   [ load_shader `vertex_shader   "vertex_light_with_shadow.glsl";
     load_shader `fragment_shader "fragment_light_with_shadow.glsl"; ])

(** we compile the shader with Shaders.compile *)
let prg = compile light_shader

(** after compilation, prg : unit program ... It can only be used
   if we set all its uniform and atribute variables *)

(** we define the cube vertices as a buffer.
   notice the flat structure of the array (3 coordinates per point)
   and the repetition of the same points because they will have
   different normals *)
let vertices = to_float_array_buffer `static_draw
  [|0.;0.;0.;
    0.;0.;1.;
    0.;1.;1.;
    0.;1.;0.;

    1.;0.;0.;
    1.;1.;0.;
    1.;1.;1.;
    1.;0.;1.;

    0.;0.;0.;
    0.;1.;0.;
    1.;1.;0.;
    1.;0.;0.;

    0.;0.;1.;
    1.;0.;1.;
    1.;1.;1.;
    0.;1.;1.;

    0.;0.;0.;
    1.;0.;0.;
    1.;0.;1.;
    0.;0.;1.;

    0.;1.;0.;
    0.;1.;1.;
    1.;1.;1.;
    1.;1.;0.;
  |]

(** we set the vertices in the shader *)
let prg = buffer_cst_attr prg "in_position" vertices
let shade = buffer_cst_attr shade "in_position" vertices


(** the normals associated to each vertex, in the same orders *)
let normals = to_float_array_buffer `static_draw
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

(** we define the texture coordinates of each vertex
   above 1 is possible as we use repeat *)
let tex_coordinates = to_float_array_buffer `static_draw
  [|
    0.;0.;
    0.;5.;
    5.;5.;
    5.;0.;

    0.;5.;
    5.;5.;
    5.;0.;
    0.;0.;

    0.;0.;
    0.;5.;
    5.;5.;
    5.;0.;

    0.;5.;
    5.;5.;
    5.;0.;
    0.;0.;

    0.;0.;
    5.;0.;
    5.;5.;
    0.;5.;

    0.;5.;
    5.;5.;
    5.;0.;
    0.;0.;
  |]

(** we set the corresponding attribute variable in the shader *)
let prg = buffer_cst_attr prg "in_tex_coordinates" tex_coordinates

(** a very 4x4 texture *)
let image = {
  width=4; height=4;format=`luminance;data=to_ubyte_bigarray [|128;128;255;255;
							       128;128;255;255;
							       255;255;128;128;
							       255;255;128;128|]
}
(** tranformed to a texture *)
let texture = image_to_texture2d image [`texture_min_filter `nearest;
					`texture_mag_filter `nearest;
					`texture_wrap_s `repeat;
					`texture_wrap_t `repeat]
(** and associated to the corresponding variable *)
let prg = texture_2d_cst_uniform prg "texture1" texture

(** we define the elements (here 12 triangles), as index in the above array *)
let elements = to_uint_element_buffer `static_draw
  [|
    0;1;2;2;3;0;
    4;5;6;6;7;4;
    8;9;10;10;11;8;
    12;13;14;14;15;12;
    16;17;18;18;19;16;
    20;21;22;22;23;20 |]

(** the modelView matrix of the cube defining the position of the cube,
    from the current time *)
let t1 = translate (-0.5) (-0.5) (-0.5)
let t2 = mul (scale 0.25) t1
let r t = mul (rotateY (5.*.t/.11.)) (rotateZ (6.*.t/.7.))

let modelView t = mul (r t) t1
let modelViewA t = mul (r t) (mul (translate (0.70) (0.) (0.)) t2)
let modelViewB t = mul (r t) (mul (translate (-0.70) (0.) (0.)) t2)
let modelViewC t = mul (r t) (mul (translate (0.) (0.70) (0.)) t2)
let modelViewD t = mul (r t) (mul (translate (0.) (-0.70) (0.)) t2)
let modelViewE t = mul (r t) (mul (translate (0.) (0.) (0.70)) t2)
let modelViewF t = mul (r t) (mul (translate (0.) (0.) (-0.70)) t2)

let center = [|0.;0.;0.|]
let lightPos = [|0.0;2.0;4.0|]
let eyePos = [|0.;0.;3.5|]
let eyeUp = [|1.0;1.0;0.0|]

(** the projection matrix: beware, it depends from the screen ratio *)
let projection () =
  (mul (perspective 45.0 !ratio 1. 5.) (lookat eyePos center eyeUp))


let shadow_projection =
  (mul (perspective 30.0 1.0 3. 7.) (lookat lightPos center eyeUp))

(** these varying we tranform the shader program into a function *)
let prg : (float array -> unit) program = float_mat4_uniform prg "ModelView"
(** notice the change of type.   *)
let prg : (float array -> float array -> unit) program = float_mat4_uniform prg "Projection"
(** Beware: the first argument in the last to be set, hence here
   the projection matrix comes before the modelView *)

let shade : (float array -> unit) program = float_mat4_uniform shade "ModelView"
(** notice the change of type.   *)
let shade : (float array -> unit) program = float_mat4_cst_uniform shade "Projection" shadow_projection

(** we se all the remaning uniform variables about lighting *)
let prg = float4v_cst_uniform prg "color" [|0.0;0.0;1.0;1.0|]
let prg = float1_cst_uniform prg "specular" 0.5
let prg = float1_cst_uniform prg "shininess" 15.
let prg = float3v_cst_uniform prg "lightPos" lightPos
let prg = float4v_cst_uniform prg "lightDiffuse" [|0.7;0.7;0.7;1.0|]
let prg = float4v_cst_uniform prg "lightAmbient" [|0.2;0.2;0.2;1.0|]


let (maptex, mapbuf) = frame_buffer_depth_texture 512 512 `depth_component24
  [`texture_min_filter `linear;
   `texture_mag_filter `linear;
   `texture_compare_mode `compare_ref_to_texture;
   `texture_wrap_s `clamp_to_edge;
   `texture_wrap_t `clamp_to_edge]

let prg = float_mat4_cst_uniform prg "shadowproj" shadow_projection
let prg = texture_2d_cst_uniform prg "shadowmap" maptex

(** we can now define a function drawing the cube using
   Shaders.draw_uint_elements *)
let dessine_cubes t =
  cull_face ~face:`back;
  let f = draw_buffer_elements prg `triangles elements (projection ()) in
  f (modelView t);
  f (modelViewA t);
  f (modelViewB t);
  f (modelViewC t);
  f (modelViewD t);
  f (modelViewE t);
  f (modelViewF t)

let shadow_cubes t =
  cull_face ~face:`front;
  let f = draw_buffer_elements shade `triangles elements in
  f (modelView t);
  f (modelViewA t);
  f (modelViewB t);
  f (modelViewC t);
  f (modelViewD t);
  f (modelViewE t);
  f (modelViewF t)

let dessine_shadow = ref false

(** some last initializations of openGL state *)
let _ =
  enable `depth_test;
  enable `cull_face;
  clear_color { r = 0.1; g = 0.1; b = 0.1; a = 1.0 }

(** two references to compute the frame rates *)
let lasttime = ref (Unix.gettimeofday ())
let frames = ref 0

(** the main drawing function, not mush to say, half of it
   if the computation of the frame rates *)
let draw () =
  let t = Unix.gettimeofday () in
  bind_framebuffer `framebuffer mapbuf;
  clear [  `depth_buffer];
  viewport ~x:0 ~y:0 ~w:512 ~h:512;
  shadow_cubes t;
  show_errors "after shadow";
  bind_framebuffer `framebuffer null_framebuffer;

  clear [  `color_buffer ; `depth_buffer];
  viewport ~x:0 ~y:0 ~w:!gwidth ~h:!gheight;
  if !dessine_shadow then shadow_cubes t else dessine_cubes t;
  swap_buffers ();
  show_errors "after draw";
  incr frames;
  let delta = t -. !lasttime in
  if delta > 5.0 then(
    let fps = float !frames /. delta in
    Printf.eprintf "fps: %.2f\n%!" fps;
    frames := 0;
    lasttime  := t
  )

(** call back for key and mouse, just for testing *)
let _ = set_key_press_callback (fun ~key ~state ~x ~y ->
  if key = 65307 then exit_loop ();
  if key = 115 then
    dessine_shadow := not !dessine_shadow;
  Printf.printf "key: %d state: %d\n%!" key state)

let _ = set_button_press_callback (fun ~button ~state ~x ~y ->
  Printf.printf "button: %d state: %d\n%!" (Obj.magic button) state)

(** when there is nothing to do, we draw *)
let _ = set_idle_callback draw

(** the reshape callback, changing the viewport and ratio
   when the window is resized *)
let _ = set_reshape_callback (fun ~width ~height ->
  gwidth := width; gheight := height;
  ratio := float width /. float height)

let _ = draw () (** draw once outsize the loop, because all exceptions are caught
                   inside the main loop *)

(** we now start the main loop ! *)
let _ = main_loop ()
