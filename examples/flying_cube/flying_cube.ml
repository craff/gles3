open Gles3.Type

let window_width  : int ref   = ref 400 (* Window width.  *)
let window_height : int ref   = ref 400 (* Window height. *)
let window_ratio  : float ref = ref 1.0 (* Window ratio.  *)

let _ =
  (* Initialise the main window. *)
  Egl.initialize !window_width !window_height "test_gles";
  (* Initialise its viewport. *)
  Gles3.viewport ~x:0 ~y:0 ~w:!window_width ~h:!window_height;
  (* Setup the reshape callback. *)
  let reshape ~width ~height =
    window_width := width; window_height := height;
    window_ratio := (float width) /. (float height);
    Gles3.viewport ~x:0 ~y:0 ~w:width ~h:height
  in
  Egl.set_reshape_callback reshape

(* The vertices of a cube (3 coordinates per point). *)
let vertices : Gles3.float_bigarray = Buffers.to_float_bigarray
  [| 0.0;0.0;0.0; 0.0;0.0;1.0; 0.0;1.0;1.0; 0.0;1.0;0.0;    (* Left   face. *)
     1.0;0.0;0.0; 1.0;0.0;1.0; 1.0;1.0;1.0; 1.0;1.0;0.0;    (* Right  face. *)
     0.0;0.0;0.0; 1.0;0.0;0.0; 1.0;1.0;0.0; 0.0;1.0;0.0;    (* Bottom face. *)
     0.0;0.0;1.0; 1.0;0.0;1.0; 1.0;1.0;1.0; 0.0;1.0;1.0;    (* Top    face. *)
     0.0;0.0;0.0; 1.0;0.0;0.0; 1.0;0.0;1.0; 0.0;0.0;1.0;    (* Back   face. *)
     0.0;1.0;0.0; 1.0;1.0;0.0; 1.0;1.0;1.0; 0.0;1.0;1.0; |] (* Front  face. *)

(* The duplicates are due to the normals, which depend on the face. *)

(* The normals associated to each vertex (in the same order). *)
let normals  : Gles3.float_bigarray = Buffers.to_float_bigarray
  [| -1.0; 0.0; 0.0; -1.0; 0.0; 0.0; -1.0; 0.0; 0.0; -1.0; 0.0; 0.0;
      1.0; 0.0; 0.0;  1.0; 0.0; 0.0;  1.0; 0.0; 0.0;  1.0; 0.0; 0.0;
      0.0; 0.0;-1.0;  0.0; 0.0;-1.0;  0.0; 0.0;-1.0;  0.0; 0.0;-1.0;
      0.0; 0.0; 1.0;  0.0; 0.0; 1.0;  0.0; 0.0; 1.0;  0.0; 0.0; 1.0;
      0.0;-1.0; 0.0;  0.0;-1.0; 0.0;  0.0;-1.0; 0.0;  0.0;-1.0; 0.0;
      0.0; 1.0; 0.0;  0.0; 1.0; 0.0;  0.0; 1.0; 0.0;  0.0; 1.0; 0.0; |]

(* The triangles as index in the array of vertices. *)
let triangles : Gles3.uint_bigarray  = Buffers.to_uint_bigarray
  [|  0; 1; 2;  2; 3; 0;    (* Left   face. *)
      4; 5; 6;  6; 7; 4;    (* Right  face. *)
      8; 9;10; 10;11; 8;    (* Bottom face. *)
     12;13;14; 14;15;12;    (* Top    face. *)
     16;17;18; 18;19;16;    (* Back   face. *)
     20;21;22; 22;23;20; |] (* Front  face. *)

(* We load and compile our shaders. *)
let prg : unit Shaders.program =
  let open Shaders in
  let vertex   = load_shader gl_vertex_shader   "vertex_light.glsl"   in
  let fragment = load_shader gl_fragment_shader "fragment_light.glsl" in
  compile ("light_shader", [vertex ; fragment])

(* Note that [prg] cannot be used until uniforms and atributes are set. *)

(* We set the input of the shader to be the vertices and the normals. *)
let prg = Shaders.float_cst_attr prg "in_position" vertices
let prg = Shaders.float_cst_attr prg "in_normal"   normals

(* We set the uniform parameters of the shader. *)
let prg = Shaders.float4v_cst_uniform prg "color"        [|0.6;0.2;0.8;1.0|]
let prg = Shaders.float1_cst_uniform prg  "specular"     0.5
let prg = Shaders.float1_cst_uniform prg  "shininess"    10.0
let prg = Shaders.float3v_cst_uniform prg "lightPos"     [|0.0;1.0;4.0|]
let prg = Shaders.float4v_cst_uniform prg "lightDiffuse" [|0.7;0.7;0.7;1.0|]
let prg = Shaders.float4v_cst_uniform prg "lightAmbient" [|0.2;0.2;0.2;1.0|]

(* We abstract away the "ModelView" parameter. *)
let prg : (float array -> unit) Shaders.program =
  Shaders.float_mat4_uniform prg "ModelView"

(* We abstract away the "Projection" parameter. *)
let prg : (float array -> float array -> unit) Shaders.program =
  Shaders.float_mat4_uniform prg "Projection"

(* Drawing function for the cube (depending on window ratio and time). *)
let draw_cube : float -> float -> unit = fun ratio t ->
  let (<*>) = Matrix.mul in
  let modelView =
    Matrix.rotateY (10.0 *. t /. 11.0)
      <*> Matrix.rotateZ (6.0  *. t /.  7.0)
      <*> Matrix.translate (-0.5) (-0.5) (-0.5)
      <*> Matrix.translate (cos(t)) (sin(t)) (0.0)
  in
  let projection =
    Matrix.perspective 45.0 ratio 1.0 5.0
      <*> Matrix.lookat [|0.0;0.0;3.5|] [|0.0;0.0;0.0|] [|1.0;1.0;0.0|]
  in
  Shaders.draw_uint_elements prg gl_triangles triangles projection modelView

(* The main drawing function. *)
let draw : unit -> unit = fun () ->
  Gles3.clear [gl_color_buffer; gl_depth_buffer];
  draw_cube !window_ratio (Unix.gettimeofday ());
  Gles3.show_errors "cube";
  Egl.swap_buffers ()

let _ =
  (* Some initialisation of the OpenGL state. *)
  Gles3.enable gl_depth_test;
  Gles3.disable gl_cull_face;
  Gles3.cull_face gl_back;
  Gles3.clear_color Gles3.({r=0.1; g=0.1; b=0.1; a=1.0});
  (* When there is nothing to do, we draw. *)
  Egl.set_idle_callback draw;
  (* Draw once to get exceptions (they are all captured by [main_loop]. *)
  draw ();
  (* Run the main loop. *)
  Egl.main_loop ()
