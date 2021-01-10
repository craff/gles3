open Gles3.Type

(**** Graph stuff with Kruskal's algorithm **********************************)

module UnionFind =
  struct
    type t = (int, int) Hashtbl.t

    let create : int -> t = Hashtbl.create

    let find : int -> t -> int = fun k uf ->
      let rec repr k = try repr (Hashtbl.find uf k) with Not_found -> k in
      let r = repr k in if r <> k then Hashtbl.replace uf k r; r

    let join : int -> int -> t -> unit = fun k1 k2 uf ->
      Hashtbl.add uf k1 k2

    let union : int -> int -> t -> unit = fun k1 k2 uf ->
      let k1 = find k1 uf in
      let k2 = find k2 uf in
      join k1 k2 uf
  end

module Graph =
  struct
    type edge =
      { link : int * int
      ; cost : int }

    type 'a graph =
      { edges : edge list
      ; nodes : 'a array }

    let kruskal : 'a graph -> 'a graph = fun gr ->
      let edges = List.sort (fun e1 e2 -> e1.cost - e2.cost) gr.edges in
      let uf = UnionFind.create (Array.length gr.nodes) in
      let fn edges e =
        let (k1, k2) = e.link in
        let k1 = UnionFind.find k1 uf in
        let k2 = UnionFind.find k2 uf in
        if k1 <> k2 then (UnionFind.join k1 k2 uf; e::edges) else edges
      in
      { gr with edges = List.fold_left fn [] edges }
  end

(**** Maze representation ***************************************************)

type cell =
  { x             : int
  ; y             : int
  ; mutable north : bool
  ; mutable south : bool
  ; mutable west  : bool
  ; mutable east  : bool }

type maze =
  { cells : cell array
  ; w     : int
  ; h     : int }

let new_cell : int -> int -> cell = fun x y ->
  { x ; y ; north = false ; south = false ; west = false ; east = false }

let init_maze : int -> int -> maze = fun w h ->
  let init_cell i = new_cell (i mod w) (i / w) in
  { w ; h ; cells = Array.init (w * h) init_cell }

let get_cell : maze -> int -> int -> cell = fun m x y ->
  m.cells.(y * m.w + x)

(**** Building the maze structure *******************************************)

let gather_edges : maze -> (int * int) list = fun m ->
  let edges = ref [] in
  let add_edge n1 n2 = edges := (n1, n2) :: !edges in
  for x = 0 to m.w - 1 do
    for y = 0 to m.h - 1 do
      (* Vertical edge. *)
      if y <> m.h - 1 then add_edge (y * m.w + x) ((y + 1) * m.w + x);
      (* Horizontal edge. *)
      if x <> m.w - 1 then add_edge (y * m.w + x) (y * m.w + x + 1)
    done
  done;
  !edges

let init_maze : int -> int -> maze = fun w h ->
  let m = init_maze w h in
  Random.self_init ();
  let doors =
    let open Graph in
    let edges = gather_edges m in
    let random_cost link = { link ; cost = Random.int (10 * m.w * m.h) } in
    let edges = List.map random_cost edges in
    let gr = kruskal { nodes = m.cells ; edges } in
    List.map (fun e -> e.link) gr.edges
  in
  let add_door m (c1,c2) =
    let c1 = get_cell m (c1 mod m.w) (c1 / m.w) in
    let c2 = get_cell m (c2 mod m.w) (c2 / m.w) in
    if c1.x = c2.x && c2.y = c1.y + 1 then      (* c2 north of c1 *)
      (c1.north <- true; c2.south <- true)
    else if c1.x = c2.x && c2.y = c1.y - 1 then (* c2 south of c1 *)
      (c1.south <- true; c2.north <- true)
    else if c1.y = c2.y && c2.x = c1.x + 1 then (* c2 east  of c1 *)
      (c1.east  <- true; c2.west  <- true)
    else if c1.y = c2.y && c2.x = c1.x - 1 then (* c2 west  of c1 *)
      (c1.west  <- true; c2.east  <- true)
    else assert false (* sould not happen. *)
  in
  List.iter (add_door m) doors; m

(**** Printing **************************************************************)

let print_maze : out_channel -> maze -> unit = fun oc m ->
  (* Top border. *)
  output_string oc "┌";
  for x = 0 to m.w - 2 do
    let cell = get_cell m x (m.h - 1) in
    let wall = if cell.east then "─" else "┬" in
    Printf.fprintf oc "──%s" wall
  done;
  output_string oc "──┐\n";
  (* First line. *)
  let print_line y =
    output_string oc "│";
    for x = 0 to m.w - 1 do
      let cell = get_cell m x y in
      let wall = if cell.east && x <> m.w - 1 then " " else "│" in
      Printf.fprintf oc "  %s" wall
    done;
    output_string oc "\n"
  in
  print_line (m.h - 1);
  (* Remaining lines (with their above separator. *)
  for y = m.h - 2 downto 0 do
    (* Printing the separator. *)
    let wall = if (get_cell m 0 y).north then "│" else "├" in
    output_string oc wall;
    for x = 0 to m.w - 2 do
      let corner_w = (get_cell m x y).north in
      let corner_e = (get_cell m (x+1) y).north in
      let corner_n = (get_cell m x (y+1)).east in
      let corner_s = (get_cell m x y).east in
      output_string oc (if corner_w then "  " else "──");
      let corner =
        match (corner_w, corner_e, corner_n, corner_s) with
        | (false, false, false, false) -> "┼"
        | (true , false, false, false) -> "├"
        | (false, true , false, false) -> "┤"
        | (false, false, true , false) -> "┬"
        | (false, false, false, true ) -> "┴"
        | (true , true , false, false) -> "│"
        | (true , false, true , false) -> "┌"
        | (true , false, false, true ) -> "└"
        | (false, true , true , false) -> "┐"
        | (false, true , false, true ) -> "┘"
        | (false, false, true , true ) -> "─"
        | (true , true , true , false) -> "╷"
        | (true , true , false, true ) -> "╵"
        | (true , false, true , true ) -> "╶"
        | (false, true , true , true ) -> "╴"
        | (true , true , true , true ) -> "?"
      in
      output_string oc corner
    done;
    let wall = if (get_cell m (m.w - 1) y).north then "  │" else "──┤" in
    output_string oc wall;
    (* Printing the line. *)
    output_string oc "\n";
    print_line y
  done;
  (* Bottom border. *)
  output_string oc "└";
  for x = 0 to m.w - 2 do
    let cell = get_cell m x 0 in
    let wall = if cell.east then "─" else "┴" in
    Printf.fprintf oc "──%s" wall
  done;
  output_string oc "──┘\n"

(**** Creating the maze *****************************************************)

let maze =
  let (w, h) =
    try
      match Sys.argv with
      | [| _ |]         -> (10, 10)
      | [| _ ; s |]     -> let n = int_of_string s in (n, n)
      | [| _ ; w ; h |] -> (int_of_string w, int_of_string h)
      | _               -> raise Not_found
    with _ ->
      Printf.printf "Usage: %s [w [h]]\n%!" Sys.argv.(0); exit 1
  in
  init_maze w h

let _ =
  print_maze stdout maze;
  flush stdout

(**** OpenGL stuff **********************************************************)
let window_width  : int ref   = ref 400 (* Window width.  *)
let window_height : int ref   = ref 400 (* Window height. *)
let window_ratio  : float ref = ref 1.0 (* Window ratio.  *)

let _ =
  (* Initialise the main window. *)
  Egl.initialize !window_width !window_height "maze";
  (* Initialise its viewport. *)
  Gles3.viewport ~x:0 ~y:0 ~w:!window_width ~h:!window_height;
  (* Setup the reshape callback. *)
  let reshape ~width ~height =
    window_width := width; window_height := height;
    window_ratio := (float width) /. (float height);
    Gles3.viewport ~x:0 ~y:0 ~w:width ~h:height
  in
  Egl.set_reshape_callback reshape

(* The vertices of a wall on the X axis. *)
let x_wall : Gles3.float_bigarray = Buffers.to_float_bigarray
  (* Left, right, bottom, top, back, front *)
  [| 0.0;0.0;0.0;  0.0;0.0;0.5;  0.0;0.1;0.5;  0.0;0.1;0.0;
     1.0;0.0;0.0;  1.0;0.0;0.5;  1.0;0.1;0.5;  1.0;0.1;0.0;
     0.0;0.0;0.0;  1.0;0.0;0.0;  1.0;0.1;0.0;  0.0;0.1;0.0;
     0.0;0.0;0.5;  1.0;0.0;0.5;  1.0;0.1;0.5;  0.0;0.1;0.5;
     0.0;0.0;0.0;  1.0;0.0;0.0;  1.0;0.0;0.5;  0.0;0.0;0.5;
     0.0;0.1;0.0;  1.0;0.1;0.0;  1.0;0.1;0.5;  0.0;0.1;0.5; |]

(* The vertices of a wall on the Y axis. *)
let y_wall : Gles3.float_bigarray = Buffers.to_float_bigarray
  (* Left, right, bottom, top, back, front *)
  [| 0.0;0.0;0.0;  0.0;1.0;0.0;  0.0;1.0;0.5;  0.0;0.0;0.5;
     0.1;0.0;0.0;  0.1;1.0;0.0;  0.1;1.0;0.5;  0.1;0.0;0.5;
     0.0;0.0;0.0;  0.0;1.0;0.0;  0.1;1.0;0.0;  0.1;0.0;0.0;
     0.0;0.0;0.5;  0.0;1.0;0.5;  0.1;1.0;0.5;  0.1;0.0;0.5;
     0.0;0.0;0.0;  0.0;0.0;0.5;  0.1;0.0;0.5;  0.1;0.0;0.0;
     0.0;1.0;0.0;  0.0;1.0;0.5;  0.1;1.0;0.5;  0.1;1.0;0.0;
  |]

(* The vertices for pillars. *)
let pillar : Gles3.float_bigarray = Buffers.to_float_bigarray
  (* Left, right, bottom, top, back, front *)
  [| 0.0;0.0;0.0;  0.0;0.0;0.5;  0.0;0.1;0.5;  0.0;0.1;0.0;
     0.1;0.0;0.0;  0.1;0.0;0.5;  0.1;0.1;0.5;  0.1;0.1;0.0;
     0.0;0.0;0.0;  0.1;0.0;0.0;  0.1;0.1;0.0;  0.0;0.1;0.0;
     0.0;0.0;0.5;  0.1;0.0;0.5;  0.1;0.1;0.5;  0.0;0.1;0.5;
     0.0;0.0;0.0;  0.1;0.0;0.0;  0.1;0.0;0.5;  0.0;0.0;0.5;
     0.0;0.1;0.0;  0.1;0.1;0.0;  0.1;0.1;0.5;  0.0;0.1;0.5; |]



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
  let vertex   = of_string gl_vertex_shader  Vertex_light.str  in
  let fragment = of_string gl_fragment_shader Fragment_light.str in
  compile ("light_shader", [vertex ; fragment])

(* We set the uniform parameters of the shader. *)
let prg = Shaders.float4v_cst_uniform prg "color"        [|0.6;0.2;0.8;1.0|]
let prg = Shaders.float1_cst_uniform prg  "specular"     0.5
let prg = Shaders.float1_cst_uniform prg  "shininess"    10.0
let prg = Shaders.float3v_cst_uniform prg "lightPos"     [|0.0;1.0;4.0|]
let prg = Shaders.float4v_cst_uniform prg "lightDiffuse" [|0.7;0.7;0.7;1.0|]
let prg = Shaders.float4v_cst_uniform prg "lightAmbient" [|0.2;0.2;0.2;1.0|]

(* We set the input of the shader to be the vertices and the normals. *)
let prg = Shaders.float_cst_attr prg "in_normal" normals
let prg_x = Shaders.float_cst_attr prg "in_position" x_wall
let prg_y = Shaders.float_cst_attr prg "in_position" y_wall
let prg_c = Shaders.float_cst_attr prg "in_position" pillar

(* We abstract away the "ModelView" and "Projection" parameters. *)
let prg_x : (float array -> unit) Shaders.program =
  Shaders.float_mat4_uniform prg_x "ModelView"

let prg_x : (float array -> float array -> unit) Shaders.program =
  Shaders.float_mat4_uniform prg_x "Projection"

let prg_y : (float array -> unit) Shaders.program =
  Shaders.float_mat4_uniform prg_y "ModelView"

let prg_y : (float array -> float array -> unit) Shaders.program =
  Shaders.float_mat4_uniform prg_y "Projection"

let prg_c : (float array -> unit) Shaders.program =
  Shaders.float_mat4_uniform prg_c "ModelView"

let prg_c : (float array -> float array -> unit) Shaders.program =
  Shaders.float_mat4_uniform prg_c "Projection"

open Camera

let camera =
  Camera.new_camera ~position:[|0.0;0.0;2.0|] ~forward:[|0.0;0.0;-1.0|]
    ~right:[|1.0;0.0;0.0|] ~up:[|0.0;1.0;0.0|] ~far:30.0 ()

let _ =
  let handle_key_press ~key ~state ~x ~y =
    match (key, state land 0x1) with
    | (65307, _) -> exit 0
    | (65363, 0) -> camera.r_speed <-  0.4
    | (65361, 0) -> camera.r_speed <- -0.4
    | (65363, 1) -> camera.t_speed <-  0.4
    | (65361, 1) -> camera.t_speed <- -0.4
    | (65362, _) -> camera.u_speed <-  0.4
    | (65364, _) -> camera.u_speed <- -0.4
    | (32   , _) -> camera.speed   <-  0.4
    | (_    , _) -> Printf.printf "unset key: %d state: %d\n%!" key state
  in
  Egl.set_key_press_callback handle_key_press

let _ =
  let handle_key_release ~key ~state ~x ~y =
    match key, state land 0x1 with
    | (65363, 0) -> camera.r_speed <- 0.0
    | (65361, 0) -> camera.r_speed <- 0.0
    | (65363, 1) -> camera.t_speed <- 0.0
    | (65361, 1) -> camera.t_speed <- 0.0
    | (65362, _) -> camera.u_speed <- 0.0
    | (65364, _) -> camera.u_speed <- 0.0
    | (32   , _) -> camera.speed   <- 0.0
    | (_       ) -> ()
  in
  Egl.set_key_release_callback handle_key_release

let _ =
  let last_x = ref 0 in
  let last_y = ref 0 in
  let valid = ref false in
  let handle_motion ~state ~x ~y =
    if !valid then
      let dx = x - !last_x in
      let dy = y - !last_y in
      last_x := x; last_y := y;
      let alpha_x = float_of_int dx /. 100.0 in
      let alpha_y = -. (float_of_int dy /. 100.0) in
      Printf.printf "Position: (%i, %i) (%i, %i) (%f, %f)\n%!" x y dx dy alpha_x alpha_y;
      rotate_right camera alpha_x;
      rotate_up camera alpha_y;
    else
      (last_x := x; last_y := y; valid := true)
  in
  Egl.set_motion_notify_callback handle_motion

(* Drawing function for the cube (depending on window ratio and time). *)
let draw_maze : float -> unit = fun ratio ->
  let (<*>) = Matrix.mul in
  let cx = (1.1 *. float_of_int maze.w +. 0.1) /. 2.0 in
  let cy = (1.1 *. float_of_int maze.h +. 0.1) /. 2.0 in
  let projection = projection ratio camera in
  let modelview =
    Matrix.scale 0.2
      <*> Matrix.translate (-. cx) (-. cy) 0.0
  in
  let draw_x x y =
    let modelView = modelview <*>
      let x =  1.1 *. float_of_int x +. 0.1 in
      let y =  1.1 *. float_of_int y in
      Matrix.translate x y 0.0
    in
    Shaders.draw_uint_elements prg_x gl_triangles triangles
      projection modelView;
  in
  let draw_y x y =
    let modelView = modelview <*>
      let x = 1.1 *. float_of_int x in
      let y = 1.1 *. float_of_int y +. 0.1 in
      Matrix.translate x y 0.0
    in
    Shaders.draw_uint_elements prg_y gl_triangles triangles
      projection modelView;
  in
  for x = 0 to maze.w - 1 do draw_x x 0; draw_x x maze.h done;
  for y = 0 to maze.h - 1 do draw_y 0 y; draw_y maze.w y done;
  for x = 0 to maze.w - 1 do
    for y = 0 to maze.h - 1 do
      let c = get_cell maze x y in
      if y <> maze.h - 1 && not c.north then draw_x x (y+1);
      if x <> maze.w - 1 && not c.east  then draw_y (x+1) y
    done
  done;
  for x = 0 to maze.w do
    for y = 0 to maze.h do
      let x = 1.1 *. float_of_int x in
      let y = 1.1 *. float_of_int y in
      let modelView = modelview <*> Matrix.translate x y 0.0 in
      Shaders.draw_uint_elements prg_c gl_triangles triangles
        projection modelView;
    done
  done

(* The main drawing function. *)
let draw : unit -> unit = fun () ->
  Gles3.clear [gl_color_buffer; gl_depth_buffer];
  Camera.update camera;
  draw_maze !window_ratio;
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
