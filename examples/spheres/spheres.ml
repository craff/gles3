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
(* matrix.ml: interface of Gles3 library                                    *)
(****************************************************************************)

open Egl
open Gles3
open Gles3.Type
open Shaders
open Buffers
open Matrix
open Textures

(** Type des sphères *)
type sphere = {
  rayon : float;
  masse : float;
  centre : Vector3.t;
  vitesse : Vector3.t;
  acceleration : Vector3.t;
  mutable active : bool;
  process : int;
  id: int;
}

let nb_spheres, nb_spheres_per_sec,
  rayon, dt, size, density,
  viscosity, radial_viscosity, wall_abs, nb_cores, debug =
  let open Arg in
  let nb_spheres = ref 1000 in
  let nb_spheres_per_sec = ref 50.0 in
  let rayon = ref 0.2 in
  let size = ref 10.0 in
  let dt = ref 0.005 in
  let density = ref 1.0 in
  let viscosity = ref 5.0 in
  let radial_viscosity = ref 5.0 in
  let wall_abs = ref 0.1 in
  let nb_cores = ref (Domain.recommended_domain_count ()) in
  let debug = ref false in
  let spec = [
    ("--nb"     , Set_int nb_spheres,
     "particule number (default 1000)");
    ("--nbs"    , Set_float nb_spheres_per_sec,
     "number of incoming particules per second (default 50)");
    ("--radius" , Set_float rayon,
     "particule radius (default 0.2)");
    ("--dt"  , Set_float dt,
     "time step interval (default 0.005)");
    ("--size"   , Set_float size,
     "size of the containing square (default 10.0)");
    ("--density", Set_float density,
     "density of the sphere");
    ("--visc"   , Set_float viscosity,
     "fluid viscosity (default 5.0)");
    ("--radial-visc" , Set_float radial_viscosity,
     "fluid radial viscosity, use to get better numerical stability (default 5.0)");
    ("--wall-abs" , Set_float wall_abs,
     "absorbed energy by wall (default 0.1)");
    ("-j", Set_int nb_cores,
     "number of cores to use (default 1)");
    ("--debug", Set debug,
     "print some debugging information")
  ] in
  parse spec (fun s -> raise (Bad s)) (Sys.argv.(0) ^ " [options]");
  !nb_spheres, !nb_spheres_per_sec,
  !rayon, !dt, !size, !density,
  !viscosity, !radial_viscosity, !wall_abs, !nb_cores, !debug

let masse = 4.0 /. 3.0 *. 3.14 *. rayon *. rayon *. rayon *. density

let random_sphere index  =
  let open Vector3 in
  { centre  = {x=Random.float (min 10.0 (rayon *. 10.0));
	       y=Random.float (min 10.0 (rayon *. 10.0));
	       z=Random.float (min 10.0 (rayon *. 10.0)) +. 10.0};
    vitesse = {x=6.0 ;y=6.0;z= 0.3 };
    acceleration ={x=0.0;y=0.0;z=0.0};
    rayon; masse; active = false; process = index mod nb_cores; id = index }

let spheres = Array.init nb_spheres random_sphere

let sync =
  let counts = Array.make nb_cores 0 in
  (fun process ->
    let n = counts.(process) + 1 in
    counts.(process) <- n;
    while (Array.fold_left (fun acc x -> acc || x < n) false counts) do () done;
  )

let print_sphere ch s =
  let open Vector3 in
  Printf.fprintf ch "(%.5f,%.5f,%.5f)" s.centre.x s.centre.y s.centre.z

let collisions_complete : (sphere -> sphere -> unit) -> sphere list -> unit
  = fun do_collision l ->
    let rec f = function (* fun l -> match l with *)
      | [] | [_] -> ()
      | s::l' -> List.iter (do_collision s) l'; f l'
    in f l

let collisions_entre : (sphere -> sphere -> unit) -> sphere list -> sphere list -> unit
  = fun do_collision l l' ->
    List.iter (fun s -> List.iter (do_collision s) l) l'

type grille = {
  inv_diametre : float;
  spheres : sphere array array; (* element de la liste *)
  htbl     : int array array; (* premier index en fonction du hash *)
  next   : int array array; (* index du suivant dans la liste, -1, fin de liste *)
  mutable free : int array;
}

let size_ratio = 7
let hash i j k = abs(Hashtbl.hash(i,j,k))

let add_grille pid i j k s g =
  let m = Array.length g.htbl.(pid) in
  let p = hash i j k mod m in
  let i = g.free.(pid) in
  g.free.(pid) <- i + 1;
  let old = g.htbl.(pid).(p) in
  g.htbl.(pid).(p) <- i;
  g.next.(pid).(i) <- old;
  g.spheres.(pid).(i) <- s

let iter_grille f i j k g =
  let m = Array.length g.htbl.(0) in
  let p = hash i j k mod m in
  for pid = 0 to nb_cores -1 do
    let n = ref g.htbl.(pid).(p) in
    let spheres = g.spheres.(pid) in
    let next = g.next.(pid) in
    while (!n >= 0) do
      f spheres.(!n);
      n := next.(!n)
    done
  done

let ajoute : int -> grille -> sphere -> unit = fun pid g s ->
  let open Vector3 in
  let {x;y;z} = mul (g.inv_diametre) s.centre in
  add_grille pid (truncate x) (truncate y) (truncate z) s g

let dummy_sphere = random_sphere (-1)

let grille =
  {
    inv_diametre = 1.0 /. (3.5 *. rayon);
    next = Array.init nb_cores (fun _ -> Array.make (1 + nb_spheres/ nb_cores) (-1));
    spheres = Array.init nb_cores (fun _ -> Array.make (1 + nb_spheres/ nb_cores) dummy_sphere);
    htbl = Array.init nb_cores
	     (fun _ -> Array.make (size_ratio * nb_spheres + 31) (-1));
    free = Array.make nb_cores 0
  }

(** simple example, using vertex buffers + one simple texture*)

(** keep the current width,height and ratio in a reference *)
let gwidth = ref 800 and gheight = ref 600
let ratio = ref (float !gwidth /. float !gheight)
(** initialization of the main window, and its viewport *)
let _ = initialize !gwidth !gheight "test_gles2";
  viewport ~x:0 ~y:0 ~w:!gwidth ~h:!gheight

let shadow_shader =
  ("shadow_shader",
   [ of_string gl_vertex_shader   Vertex_shadow.str;
     of_string gl_fragment_shader Fragment_shadow.str; ])

let shade = compile shadow_shader

(** we define our shaders, with the type expected by Shaders.compile.
   the string are just use to report errors *)
let light_shader =
  ("light_shader",
   [ of_string gl_vertex_shader   Vertex_light_with_shadow.str;
     of_string gl_fragment_shader Fragment_light_with_shadow.str; ])

let implicit_shadow_shader =
  ("implicit_shadow_shader",
   [ of_string gl_vertex_shader Vertex_implicit_shadow.str;
     of_string gl_fragment_shader Sphere_shadow.str;
     of_string gl_fragment_shader Fragment_implicit_shadow.str;
   ])

let ishade = compile implicit_shadow_shader

let light_implicit_shader =
  ("light_implicit_shader",
   [ of_string gl_vertex_shader Vertex_shadow.str;
     of_string gl_fragment_shader Sphere.str;
     of_string gl_fragment_shader Fragment_light_implicit_with_shadow.str;
   ])

(** we compile the shader with Shaders.compile *)
let prg = compile light_shader
let iprg = compile light_implicit_shader

let vertices = to_float_array_buffer gl_static_draw
  [|0.;0.;0.;
    0.;1.;0.;
    1.;1.;0.;
    1.;0.;0.|]

let elements = to_uint_element_buffer gl_static_draw
  [|1;0;2;   2;0;3|]

(** we set the vertices in the shader *)
let prg = buffer_cst_attr prg "in_position" vertices
let shade = buffer_cst_attr shade "in_position" vertices

(** the normals associated to each vertex, in the same orders *)
let normals = to_float_array_buffer gl_static_draw
  [|
    0.;0.;1.;
    0.;0.;1.;
    0.;0.;1.;
    0.;0.;1.|]
(** we set the normals vertices in the shader *)
let prg = buffer_cst_attr prg "in_normal" normals

(** we define the texture coordinates of each vertex
   above 1 is possible as we use repeat *)
let tex_coordinates = to_float_array_buffer gl_static_draw
  [|
    0.;0.;
    0.;5.;
    5.;5.;
    5.;0.|]

(** we set the corresponding attribute variable in the shader *)
let prg = buffer_cst_attr prg "in_tex_coordinates" tex_coordinates

(** a very 4x4 texture *)
let image = {
  width=4; height=4;format=gl_luminance;data=to_ubyte_bigarray [|128;128;255;255;
							       128;128;255;255;
							       255;255;128;128;
							       255;255;128;128|]
}
(** tranformed to a texture *)
let texture = image_to_texture2d image [texture_min_filter gl_nearest;
					texture_mag_filter gl_nearest;
					texture_wrap_s gl_repeat;
					texture_wrap_t gl_repeat]
(** and associated to the corresponding variable *)
let prg = texture_2d_cst_uniform prg "texture1" texture

let ivertices = to_float_array_buffer gl_static_draw
    [|-1.;-1.;-1.;
      -1.;-1.;1.;
      -1.;1.;1.;
      -1.;1.;-1.;
      1.;-1.;-1.;
      1.;-1.;1.;
      1.;1.;1.;
      1.;1.;-1.;
  |]

let ielements = to_uint_element_buffer gl_static_draw
  [|1;2;0;   0;2;3;
    5;4;6;   6;4;7;
    1;0;5;   5;0;4;
    2;6;3;   3;6;7;
    0;3;4;   4;3;7;
    1;5;2;   2;5;6;
  |]

let iprg = buffer_cst_attr iprg "in_position" ivertices
let ishade = buffer_cst_attr ishade "in_position" ivertices

let center = [|size/.2.0;size/.2.0;size/.2.0|]
let lightPos = [|-.size;0.5 *. size;2.5 *. size|]
let eyePos = [|-1.6 *. size;0.5*.size;0.9 *. size|]
let eyeUp = [|0.0;0.0;1.0|]
let near = 1.0
let far = 5. *. size

(** the projection matrix: beware, it depends from the screen ratio *)
let projection () =
  (mul (perspective 45.0 !ratio near far) (lookat eyePos center eyeUp))

let shadow_projection =
  (mul (perspective 40.0 1.0 7.0 (3.5 *. size)) (lookat lightPos center eyeUp))

(** these varying we tranform the shader program into a function *)
let prg : (float array -> unit) program = float_mat4_uniform prg "ModelView"
let iprg : (float array -> unit) program = float_mat3_uniform iprg "NormalMatrix"
let iprg : (float array -> float array -> unit) program = float_mat4_uniform iprg "InvModelView"
let iprg : (float array -> float array ->  float array -> unit) program = float_mat4_uniform iprg "ModelView"

(** notice the change of type.   *)
let prg : (float array -> float array -> unit) program = float_mat4_uniform prg "Projection"
let iprg : (float array -> float array -> float array -> float array -> unit) program = float_mat4_uniform iprg "Projection"
(** Beware: the first argument in the last to be set, hence here
   the projection matrix comes before the modelView *)

let shade : (float array -> unit) program = float_mat4_uniform shade "ModelView"
(** notice the change of type.   *)
let shade : (float array -> unit) program = float_mat4_cst_uniform shade "Projection" shadow_projection

let ishade : (float array -> unit) program = float_mat4_uniform ishade "InvModelView"
let ishade : (float array -> float array -> unit) program = float_mat4_uniform ishade "ModelView"
(** notice the change of type.   *)
let ishade : (float array -> float array -> unit) program = float_mat4_cst_uniform ishade "Projection" shadow_projection

(** we se all the remaning uniform variables about lighting *)
let prg = float4v_cst_uniform prg "color" [|0.0;0.0;1.0;1.0|]
let prg = float1_cst_uniform prg "specular" 0.5
let prg = float1_cst_uniform prg "shininess" 15.
let prg = float3v_cst_uniform prg "lightPos" lightPos
let prg = float4v_cst_uniform prg "lightDiffuse" [|0.7;0.7;0.7;1.0|]
let prg = float4v_cst_uniform prg "lightAmbient" [|0.2;0.2;0.2;1.0|]

let iprg = float4v_cst_uniform iprg "color" [|0.6;0.6;0.2;1.0|]
let iprg = float1_cst_uniform iprg "specular" 0.5
let iprg = float1_cst_uniform iprg "shininess" 15.
let iprg = float3v_cst_uniform iprg "lightPos" lightPos
let ishade = float3v_cst_uniform ishade "lightPos" lightPos
let iprg = float4v_cst_uniform iprg "lightDiffuse" [|0.7;0.7;0.7;1.0|]
let iprg = float4v_cst_uniform iprg "lightAmbient" [|0.2;0.2;0.2;1.0|]
let iprg = float3v_cst_uniform iprg "eyePos" eyePos

let shadow_map_size = 800

let depthmap = framebuffer_depth_texture shadow_map_size
                 shadow_map_size gl_depth_component24
  [texture_min_filter gl_linear;
   texture_mag_filter gl_linear;
   texture_compare_mode gl_compare_ref_to_texture;
   texture_wrap_s gl_clamp_to_edge;
   texture_wrap_t gl_clamp_to_edge]

let prg = float_mat4_cst_uniform prg "shadowproj" shadow_projection
let prg = texture_2d_cst_uniform prg "shadowmap" depthmap.tex
let iprg = float_mat4_cst_uniform iprg "shadowproj" shadow_projection
let iprg = texture_2d_cst_uniform iprg "shadowmap" depthmap.tex

let dessine_sphere index s =
  if s.active then
    let x = s.centre.Vector3.x in
    let y = s.centre.Vector3.y in
    let z = s.centre.Vector3.z in
    let r = s.rayon in
    let m = mul (translate x y z) (scale r) in
    let im = mul (scale (1. /. r)) (translate (-.x) (-.y) (-.z)) in
    let n = idt3 in
    draw_buffer_elements iprg gl_triangles ielements (projection ()) m im n

let shadow_sphere index s =
  let open Bigarray.Array1 in
  if s.active then
    let x = s.centre.Vector3.x in
    let y = s.centre.Vector3.y in
    let z = s.centre.Vector3.z in
    let r = s.rayon in
    let m = mul (translate x y z) (scale r) in
    let im = mul (scale (1. /. r)) (translate (-.x) (-.y) (-.z)) in
    draw_buffer_elements ishade gl_triangles ielements m im


let dessine_sol () =
  let m = mul Vector3.(translate (-0.5 *. size) (-0.5 *. size) 0.) (scale (2.0 *. size)) in
  draw_buffer_elements prg gl_triangles elements (projection ()) m

let dessine_shadow = ref false

(** some last initializations of openGL state *)
let _ =
  enable gl_depth_test;
  disable gl_cull_face;
  clear_color { r = 0.1; g = 0.1; b = 0.1; a = 1.0 }

(** call back for key and mouse, just for testing *)
let _ = set_key_press_callback (fun ~key ~state ~x ~y ->
  if key = 65307 then exit_loop ();
  if key = 115 then
    dessine_shadow := not !dessine_shadow;
  Printf.printf "key: %d state: %d\n%!" key state)

let _ = set_button_press_callback (fun ~button ~state ~x ~y ->
  Printf.printf "button: %d state: %d\n%!" (Obj.magic button) state)

(** the reshape callback, changing the viewport and ratio
   when the window is resized *)
let _ = set_reshape_callback (fun ~width ~height ->
  gwidth := width; gheight := height;
  ratio := float width /. float height)


let sum_voisins f i j k = f i j k +.
  f (i+1) j k +. f i (j+1) k +. f i j (k+1) +.
  f (i-1) j k +. f i (j-1) k +. f i j (k-1) +.
  f (i+1) (j+1)  k+. f (i+1) j (k+1)+. f i (j+1) (k+1)+. f (i+1) (j-1) k+. f (i+1) j (k-1)+. f i (j+1) (k-1)+.
  f (i-1) (j+1)  k+. f (i-1) j (k+1)+. f i (j-1) (k+1)+. f (i-1) (j-1) k+. f (i-1) j (k-1)+. f i (j-1) (k-1)+.
  f (i+1) (j+1) (k+1)+. f (i+1) (j-1) (k+1)+. f (i+1) (j+1) (k-1)+. f (i+1) (j-1) (k-1) +.
  f (i-1) (j+1) (k+1)+. f (i-1) (j-1) (k+1)+. f (i-1) (j+1) (k-1)+. f (i-1) (j-1) (k-1)

let do_voisins f i j k =
  f (i+1) j k; f i (j+1) k; f i j (k+1);
  f (i+1) (j+1)  k; f (i+1) j (k+1); f i (j+1) (k+1);
  f (i+1) (j-1) k; f (i+1) j (k-1); f i (j+1) (k-1);
  f (i+1) (j+1) (k+1); f (i+1) (j-1) (k+1); f (i+1) (j+1) (k-1);
  f (i+1) (j-1) (k-1)

let proj a d =
  let open Vector3 in
  mul (dot a d /. norm2 d) d

let force x r =
  let s = 0.8 in
  let coef = 1.0 and coefmax = 5. in
  let r2 = r *. r in
  let x2 = x *. x in
  if x2 < 2. *. r2 then
    let v = (x2  -. r2) in
    (viscosity *. v, if x2 < s*.r2 then -.coefmax else max(-.coefmax) (-. coef *. v *. (x2 -. r2) /. (x2 -. s *. r2)))
  else (0.0, 0.0)

(* test is two spheres intersect and have speed vector that rapproches them.
   it yes, change speed arcording to a physical model where all sphere have
   same density *)
let bounce s1 s2 =
  if s1 != s2 then begin
    let open Vector3 in
    let dir = sub s1.centre s2.centre in
    let ndir = norm dir in
    let visco, delta = force ndir (s1.rayon +. s2.rayon) in
    if delta <> 0.0 then
      begin
	let f = mul (delta /. ndir) dir in
	addq_alpha s1.acceleration (  delta /. ndir *. s2.masse) f;
	addq_alpha s2.acceleration (-.delta /. ndir *. s1.masse) f;
	let dv = sub s2.vitesse s1.vitesse in
	addq_alpha s1.acceleration (   visco *. s2.masse) dv;
	addq_alpha s2.acceleration (-. visco *. s1.masse) dv;
	let pdv = proj dv dir in
	addq_alpha s1.acceleration (   radial_viscosity *. s2.masse) pdv;
	addq_alpha s2.acceleration (-. radial_viscosity *. s1.masse) pdv
      end
  end

let collisions : int -> grille ->
                 (sphere -> sphere -> unit) -> sphere array -> unit
  = fun process grille do_collision l ->
    Array.iter (fun s ->
      if s.process = process && s.active then begin
	let open Vector3 in
	let {x;y;z} = mul (grille.inv_diametre) s.centre in
	let i = truncate x and j = truncate y and k = truncate z in
	do_voisins (fun i' j' k' ->
	  iter_grille (fun s' -> do_collision s s') i' j' k' grille) i j k;
	iter_grille (do_collision s) i j k grille
      end) spheres

let cur_nb = Array.make nb_cores 0


let one_step process t dt =
  let frot = -0.001 in
  let gravity = 9.8 in
  let open Vector3 in
  sync process;
  Array.fill grille.htbl.(process) 0 (Array.length grille.htbl.(process)) (-1);
  grille.free.(process) <- 0;
  Array.iter (fun s ->
    if s.process = process && s.active then begin ajoute process grille s;
    end) spheres;
  Array.iter (fun s ->
    if s.process = process && s.active then begin
      let c = frot *. norm s.vitesse in
      s.acceleration.x <- c *. s.vitesse.x;
      s.acceleration.y <- c *. s.vitesse.y +. sin(1.0 *. cos(t)) *. gravity *. s.masse;
      s.acceleration.z <- c *. s.vitesse.z -. cos(1.0 *. cos(t)) *. gravity *. s.masse;
    end;
  ) spheres;
  sync process;
  collisions process grille bounce spheres;
  sync process;
  Array.iter (fun s ->
    if s.process = process && s.active then begin
      addq_alpha s.vitesse (dt /. s.masse) s.acceleration;
      s.vitesse.x <-
	if s.centre.x < s.rayon && s.vitesse.x < 0. then -. wall_abs *. s.vitesse.x else
	  if size -. s.centre.x < s.rayon && s.vitesse.x > 0. then -. wall_abs *. s.vitesse.x else
	    s.vitesse.x;
      s.vitesse.y <- if s.centre.y < s.rayon && s.vitesse.y < 0. then -. wall_abs *. s.vitesse.y else
	  if size -. s.centre.y < s.rayon && s.vitesse.y > 0. then -. wall_abs *. s.vitesse.y else
	    s.vitesse.y;
      s.vitesse.z <- if s.centre.z < s.rayon && s.vitesse.z < 0. then -. wall_abs *. s.vitesse.z else
	  s.vitesse.z;
      addq_alpha s.centre dt s.vitesse;
    end
  ) spheres

(** two references to compute the frame rates *)
let lasttime = Array.init (nb_cores+1) (fun _ -> Unix.gettimeofday ())
(** two references to compute the frame rates *)
let lasttimeframe = Array.init (nb_cores+1) (fun _ -> Unix.gettimeofday ())
let frames = Array.make (nb_cores+1) 0


    (** the main drawing function, not mush to say, half of it
   if the computation of the frame rates *)
let draw () =
  let t = Unix.gettimeofday () in
  bind_framebuffer gl_framebuffer depthmap.framebuffer.framebuffer_index;
  clear [  gl_color_buffer ; gl_depth_buffer];
  viewport ~x:0 ~y:0 ~w:shadow_map_size ~h:shadow_map_size;
  cull_face ~face:gl_front;
  Array.iteri shadow_sphere spheres;
  show_errors "after shadow";
  bind_framebuffer gl_framebuffer null_framebuffer;

  clear [  gl_color_buffer ; gl_depth_buffer];
  viewport ~x:0 ~y:0 ~w:!gwidth ~h:!gheight;
  if !dessine_shadow then (
    Array.iteri shadow_sphere spheres;
  ) else (
    cull_face ~face:gl_back;
    dessine_sol ();
    Array.iteri dessine_sphere spheres;
  );
  swap_buffers ();
  show_errors "after draw";
  frames.(nb_cores) <- frames.(nb_cores) + 1;
  let delta = t -. lasttimeframe.(nb_cores) in
  if delta > 5.0 then(
    let fps = float frames.(nb_cores) /. delta in
    Printf.eprintf "fps: %.2f\n%!" fps;
    frames.(nb_cores) <- 0;
    lasttimeframe.(nb_cores) <- t
  )

(** the main drawing function, not mush to say, half of it
   if the computation of the frame rates *)
let rec run process t0 t =
  let t' = Unix.gettimeofday () in
  let t  = t +. dt in
  let delay = t -. t' in
  if delay > dt then ignore (Unix.select [] [] [] delay);
  while cur_nb.(process) < nb_spheres && float cur_nb.(process) < nb_spheres_per_sec *. (t -. t0) do
    let s = spheres.(cur_nb.(process)) in
    if s.process = process then s.active <- true;
    cur_nb.(process) <- cur_nb.(process) + 1
  done;
  one_step process t dt;
  frames.(process) <- frames.(process) + 1;
  let delta = t' -. lasttime.(process) in
  if process = 0 && delta > 5.0 then(
    let cps = float frames.(process) /. delta in
    let ratio = delta /. (float frames.(process) *. dt) in
    Printf.eprintf "cps: %.2f, speed ratio: %f, spheres: %d\n%!" cps ratio cur_nb.(process);
    frames.(process) <- 0;
    lasttime.(process) <- t'
  );
  run process t0 t


(** when there is nothing to do, we draw *)
let _ = set_idle_callback draw

let _ = draw () (** draw once outsize the loop, because all exceptions are caught
                   inside the main loop *)

(** we now start the main loop ! *)
let _ =
  let t0 = Unix.gettimeofday () in
  let rec f pids i =
    if i = 0 then
      begin
        main_loop ();
      end
    else
      begin
        let pid = ignore (Domain.spawn (fun () -> run (i-1) t0 t0)); (i-1) in
        f (pid::pids) (i-1)
      end
  in f [] nb_cores
