open Egl
open Gles3
open Shaders
open Buffers
open Matrix

(** simple example, using vertex arrays*)

(** keep the current width,height and ratio in a reference *)
let gwidth = ref 800 and gheight = ref 600
let ratio = ref (float !gwidth /. float !gheight)
(** initialization of the main window, and its viewport *)
let _ = initialize !gwidth !gheight "test_gles";
  viewport ~x:0 ~y:0 ~w:!gwidth ~h:!gheight

(** display all available informations about the context *)
let _ =
  Printf.eprintf "Vendor: %s\n%!" (get_vendor ());
  Printf.eprintf "Renderer: %s\n%!" (get_renderer ());
  Printf.eprintf "Version: %s\n%!" (get_version ());
  Printf.eprintf "GLSL Version: %s\n%!" (get_shading_language_version ());
  Printf.eprintf "Extensions: %s\n%!" (get_extensions ())

(** we define our shaders, with the type expected by Shaders.compile.
   the string are just use to report errors *)
let light_shader =
  ("light_shader",
  [{ name = "vertex_main";
     ty   = `vertex_shader;
     src  = "
   uniform mat4 ModelView,Projection;

   uniform vec4 lightDiffuse,lightAmbient,color;
   uniform vec3 lightPos;

   in vec3 in_position;
   in vec3 in_normal;

   out vec4 diffuse,ambient,m_position;
   out vec3 normal,halfVector;

   void main()
   {
     // only works for orthogonal matrices
     mat3 NormalMatrix=mat3(ModelView[0].xyz,ModelView[1].xyz,ModelView[2].xyz);
     /* first transform the normal into eye space and
     normalize the result */
     normal = normalize(NormalMatrix * in_normal);

     /* pass the halfVector to the fragment shader */
     m_position = ModelView * vec4(in_position,1.0);
     halfVector = normalize(lightPos - 2.0 * m_position.xyz);

     /* Compute the diffuse, ambient and globalAmbient terms */
     diffuse = color * lightDiffuse;
     ambient = color * lightAmbient;
     gl_Position = Projection * m_position;
   }"};
   { name = "fragment_main";
     ty   = `fragment_shader;
     src  = "
   uniform vec3 lightPos;
   uniform float specular,shininess;
   in vec3 normal,halfVector;
   in vec4 diffuse,ambient,m_position;
   out vec4 FragColor;

   void main()
   {
     vec3 halfV,lightDir;
     float NdotL,NdotHV;

     lightDir = normalize(lightPos - m_position.xyz);

     /* The ambient term will always be present */
     vec4 color = ambient;
     /* compute the dot product between normal and ldir */

     NdotL = dot(normal,lightDir);
     if (NdotL > 0.0) {
        color += diffuse * NdotL;
        halfV = normalize(halfVector);
        NdotHV = max(dot(normal,halfV),0.0);
        color += specular * pow(NdotHV, shininess);
     }

     FragColor=color;
    }"};
  ])

(** we compile the shader with Shaders.compile *)
let prg : unit program = compile light_shader

(** after compilation, prg : unit program ... It can only be used
   if we set all its uniform and atribute variables *)

(** we define the cube vertices as a big array.
   notice the flat structure of the array (3 coordinates per point)
   and the repetition of the same points because they will have
   different normals *)
let vertices = to_float_bigarray
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
    1.;0.;1.;
    1.;1.;1.;
    0.;1.;1.;

    0.;0.;0.;
    1.;0.;0.;
    1.;0.;1.;
    0.;0.;1.;

    0.;1.;0.;
    1.;1.;0.;
    1.;1.;1.;
    0.;1.;1.;
  |]

(** we set the vertices in the shader *)
let prg = float_cst_attr prg "in_position" vertices

(** the normals associated to each vertex, in the same orders *)
let normals = to_float_bigarray
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
let prg = float_cst_attr prg "in_normal" normals

(** we define the elements (here 12 triangles), as index in the above array *)
let elements = to_uint_bigarray [|
  0;1;2;    2;3;0;
  4;5;6;    6;7;4;
  8;9;10;   10;11;8;
  12;13;14; 14;15;12;
  16;17;18; 18;19;16;
  20;21;22; 22;23;20 |]

(** the modelView matrix of the cube defining the position of the cube,
   from the current time *)
let modelView t =
  (mul (rotateY (10.*.t/.11.))
     (mul (rotateZ (6.*.t/.7.)) (translate (-0.5) (-0.5) (-0.5))))

let center = [|0.;0.;0.|]
let lightPos = [|0.0;1.0;4.0|]
let eyePos = [|0.;0.;3.5|]
let eyeUp = [|1.0;1.0;0.0|]

(** the projection matrix: beware, it depends from the screen ratio *)
let projection () =
  (mul (perspective 45.0 !ratio 1. 5.) (lookat eyePos center eyeUp))

(** these varying we tranform the shader program into a function *)
let prg : (float array -> unit) program = float_mat4_uniform prg "ModelView"
(** notice the change of type.   *)
let prg : (float array -> float array -> unit) program = float_mat4_uniform prg "Projection"
(** Beware: the first argument in the last to be set, hence here
   the projection matrix comes before the modelView *)

(** we se all the remaning uniform variables about lighting *)
let prg = float4v_cst_uniform prg "color" [|0.0;0.0;1.0;1.0|]
let prg = float1_cst_uniform prg "specular" 0.5
let prg = float1_cst_uniform prg "shininess" 10.
let prg = float3v_cst_uniform prg "lightPos" lightPos
let prg = float4v_cst_uniform prg "lightDiffuse" [|0.7;0.7;0.7;1.0|]
let prg = float4v_cst_uniform prg "lightAmbient" [|0.2;0.2;0.2;1.0|]

(** we can now define a function drawing the cube using
   Shaders.draw_uint_elements *)
let dessine_cube t = draw_uint_elements prg `triangles elements (projection ()) (modelView t)

(** some last initializations of openGL state *)
let _ =
  enable `depth_test;
  cull_face `back;
  clear_color { r = 0.1; g = 0.1; b = 0.1; a = 1.0 }

(** two references to compute the frame rates *)
let lasttime = ref (Unix.gettimeofday ())
let frames = ref 0

(** the main drawing function, not mush to say, half of it
   if the computation of the frame rates *)
let draw () =
  clear [ `color_buffer ; `depth_buffer];
  let t = Unix.gettimeofday () in
  dessine_cube t;
  swap_buffers ();
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
let _ = set_key_press_callback (fun ~key ~state ~x ~y ->
  if key = 65307 then exit_loop ();
  Printf.printf "key: %d state: %d\n%!" key state)

let _ = set_button_press_callback (fun ~button ~state ~x ~y ->
  Printf.printf "button: %d state: %d\n%!" (Obj.magic button) state)

(** when there is nothing to do, we draw *)
let _ = set_idle_callback draw

(** the reshape callback, changing the viewport and ratio
   when the window is resized *)
let _ = set_reshape_callback (fun ~width ~height ->
  gwidth := width; gheight := height;
 ratio := float width /. float height;
  viewport ~x:0 ~y:0 ~w:width ~h:height)

let _ = draw () (** draw once outsize the loop, because all exceptions are caught
                   inside the main loop *)

(** we now start the main loop ! *)
let _ = main_loop ()
