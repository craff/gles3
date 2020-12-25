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
(* shaders.mli: interface of Gles3 library                                  *)
(****************************************************************************)

open Gles3
open Gles3.Type
open Buffers
open Textures

(** Here is a short introduction to shaders (that are mandatory in GLES) in
    the hope that you can use gles3 with no prior knowledge of GLES/OpenGL.

    A shader is a program (in GLES, lookit up) that will run on your graphic
    card (GPU) and participates to the rendering of the scene. The are two
    types of shaders:

    - vertex_shader : you only send point, lines, triangles to OpenGL (that
      does not means you can only display points, lines of triangles). And
      the vertex shader is run for each vertex. It allows to compute some
      "out" variables whose values are lineraly interpolated and passed to
      the fragment shader. The only role of the vertex shader are this
      computation of "out" variables. One variable "gl_position" does not
      need to be declared but must be computed by the vertex shader : it is
      the position of the vertex. It is use to know which pixel on the screen
      are in the point/line/triangle and for the interpolation.

      The simplest posible vertex shader is therefore:
      ======================================
      in vec3 in_position;
      void main()
      {
        gl_Position = (in_position, 1.0);
      }
      ======================================
      Which takes an attribute variables (see below) in_position as a vector
      with 3 coordinates and passes it directly to the fragment
      shader. gl_Position is a 4 vector: GLES/OpenGL uses projective
      coordinates, you need to add a 1.0 in the end.

      Usually, the vertex shader do some matrix multiplication to compute the
      postition, and other annexe computation. For instance, if what you draw
      is a piece of plane, you can compute the normal in the vertex
      shader. But if you draw a curved surface, it is much much better to
      compute the normal in the fragment shader for each pixel.

    - fragment_shader : (fragment means pixel, except with antialiasing with
      more the one fragment per pixel) that computes the final color to
      render on the screen, but also its depth (distance to the camera). This
      is this adjustment of depth that allows to draw non plane surface. This
      computation of color usually uses some lighting models which depends
      of the position of the light source, and for specular light, the
      position of the eye. A lot of examples are available on internet and a
      few with this library.

      Remark: you can also discard the pixel and draw nothing. discard +
      depth modification allows to draw a curved surface by drawing a
      polyhedra containing the surface at the level of the vertex shader.
      Then, for each pixel of the polyhedra, the fragment shader computes the
      first point of the surface on the half line from the eye to the pixel
      being drawn. If there is no such point you discard the pixel, otherwise
      you adjust the depth. Look at the testd or spheres examples.

  Now, there are some terminology about variables used by shaders:

    - uniform variables: they to not depend on the vertex/pixel being drawn.
      these can be for instance the coordinates of the eye, the light
      source. It may be the color if your object as a uniform color. They are
      accessible both by the vertex and fragment shader.

    - attribute variables: (declared with "in" in the fragment shader) which
      have a distinct value for each vertices. Typically the position of the
      vertex !

    - the "out" variables declared in the vertex shader which are declared as
      "in" variable in the fragment shader. All these variables are linearly
      interpolated from the value at each vertex to compute the value at each
      pixel, using the gl_Position variable as said above.

    - the fragment shader should either call discard to display nothing or
      set the out vec4 FragColor variable (the fourth components for
      transparency). Optionnaly gl_FragDepth to can be adjusted (look on
      internet to see what transformation you have to do to find the correct
      depth) to draw curved surfaces.

  Remark: some effects like transparency, can not (yet?) be done using
  shaders. You can instruct GLES that the color produced by the fragment
  shader has to be combined with the color already in the screen buffer with
  a fixed set of function (lookup blending on internet and blend in
  gles3.mli). The same is true for the "depth test" performed to know if the
  color must be actually drawn. The normal test is to draw the pixel if it is
  nearer to the eye than the previously drawn pixel at te same position (if
  any), but you can choose the test from a fixed set of function (see
  "depth_func").  You can also use "scissors" of "stencils" to render some
  part of the screen not drawable.

  FOR ALL THESE FUNCTIONS: look at gles3.mli and texture.mli, the examples
  and internet.

  When you understand shaders, you can write some (or use some found with
  gles3 or internet). Then use them following the comments in this file.*)

(** A record type to hold a shader, it has a name for message, a type
    `vertex_shader of `fragment_shader and its source code.

    Note: because the type is fixed, if you want to use the same function in
    both a vertex and fragment shader, you have to give it twice.  *)
type shader = { name : string; ty : shader_type; src : string; }

(** [load_shader ty filename] allows to load a shader from a file *)
val load_shader : shader_type -> string -> shader

(** [of_stirng ty shader] takes a shader directly from a string *)
val of_string : shader_type -> string -> shader

(** gles3 try to give a reasonnably type safe interface to shaders trough
    the abstract type of 'a program. *)
type 'a program

(** [compile (name, shaders)] compile the givens shaders, the [name] is just
    for clear error messages. It performs both what GLES calls compilation
    and linking.

    The optional parameter version is the gles version "300 es" by default
    and floating point precision "highp" by default. It should not be set
    in the shader because GLES requires the same for every shader. So if
    you take a shader from internet, remove such indication if any!

    The resulting unit program can in general not be used directly because you
    have to give the value of the attribute and uniform variables. This is
    test at the OCaml level (but not yet at compile time as it would require
    to ask the GPU to compile the shader at compile time ... but why not in
    future version, using GADT ?

    The idea is that you will set these variables using two possibilities
    (orthogonal to the concept of uniform/attribute variables, giving four
    possibilities).  To understand the difference, assume you are drawing
    many objects of the same color. You can pass the color as a "constant"
    and if you draw all your objects consecutively, there will only be one
    function call for all the sphere to pass the color. These function calls
    to GLES/OpenGl are the bottle neck of 3D drwing as they correspond to
    exchange of data between the CPU memory and the GPU memory.

    The position of the object (a matrix ?) can be passed as function parameter,
    transforming our "unit program" into a "(float_bigarray -> unit)" program
    if your position uses floats.
 *)
val compile : ?version:string -> ?precision:string ->string * shader list
              -> unit program

(** Here are the functions to transform a program into a function, after
    instanciating of turning into parameters all its uniform and attribute
    variables. It produces a Failure exception giving the undefined variable
    name if it is not the case.

    In GLES, you almost never draw one triangle! Your program must have some
    attribute arrays describing as many properties you want for each vertex
    (position, normals, color if non constant, coordinate in a texture,
    ...) Once set, you can "draw these" using the function below.

    Beware of the size constraints of these attributes! If you have one
    float attribute variable and a vec3 attribute variable, the former
    will be an array of size N if you have N vertices while the latter will
    be of size 3*N. Again sizes of array are checked from OCaml.
 *)

(** [draw prg shape ~first last] will "draw directly" your array. For
    instance if shape = `triangles, it will consider each three consecutive
    values in your arrays as the parameter associated to the three vertices of
    a triangle. In this case, [last - first] should be a multiple of
    three. Some error message is send at runtime if sizes are incorrect.

    As already said, all the variable of the shader must have been set. GLES3 is
    testing that.

    In fact, [draw prg shape ~first last] is returning a function that will
    do the drawing, except when prd is of type "unit program" (all variables
    have been set as constant).

    Again beware of size: if you have an attribute which is a vec4, each
    triangle will use 12 consectitive elements.  *)
val draw_arrays : 'a program -> shape -> ?first:int -> int -> 'a

(** Very often, you want to use the same vertex many time. the four function
    below allows this. For instance, [draw_ubyte_elements prg shape elements]
    will draw (more precisely return a function that will draw) the elements
    corresponding to the indices in the array [elemnts]. For instance, if
    shape is [`triangles] and [elements] corresponds to (5,7,6,1,5,7), this
    will draw two triangles taking their data at index (5,7,6) and (1,5,7) in
    the array that were or will be provided as attribute. *)
val draw_ushort_elements : 'a program -> shape -> ushort_bigarray -> 'a
val draw_ubyte_elements : 'a program -> shape -> ubyte_bigarray -> 'a
val draw_uint_elements : 'a program -> shape -> uint_bigarray -> 'a

(** Same as above using buffer. Buffer are more efficient than arrays because
    you can give to OpenGL some information like `static_draw telling this
    array is constant. This implies that the buffer will be transferred only
    once to GPU memory.

    You can even use dynamic buffer which the CPU will write and the GPU will
    read and you pass only once the address of the buffer and GLES will try
    to optimise memory transfer.

    As the tendency of GLES is to remove old less efficient features, even
    when the new features are more complex... Better use buffer because array
    might disappear one day? Just look at what was removed from OpenGL 1/2,
    like the function to draw a single triangle... *)
val draw_buffer_elements : 'a program -> shape -> 'b element_buffer -> 'a

(** Functions to parametrize a shader by attribute variables, the
    [norm] and [stride] parameters are still fixed. No version is
    provided for buffer as you should probably use dynamic buffers and
    pass their adress once?

    For instance [uint_attr prg ~norm ~stride varname] will transform a ['a
    program] into a [(uint_bigarray -> 'a) program] that holds the value of
    the attribute variable name "varname". If this variable is a "vec4" these
    values will be used four by four. So the index 0 will correspond to the
    first 4 elements of the array and the index 1 to the elements from index
    4 to 7. This is in fact mandatory as the index of the elements can not
    depends on the size of each variables because you can have attribute of
    different sizes.

    Remark: for attribute, all types are converted to float in the shader !!!

    The optional parameter ~norm tells to normalize vector before using them.

    The optional parameter ~stride concerns alignement. For instance if
    you use a byte_bigarray for a vec3, but you want to align you vec3 and
    32 bytes, then stride = 4. stride = 0 (the default) means to use the
    natural size.
 *)
val byte_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                -> (byte_bigarray -> 'a) program
val ubyte_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                 -> (ubyte_bigarray -> 'a) program
val short_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                 -> (short_bigarray -> 'a) program
val ushort_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                  -> (ushort_bigarray -> 'a) program
val uint_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                -> (uint_bigarray -> 'a) program
val float_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                 -> (float_bigarray -> 'a) program

(** Functions to give a fixed value to an attribute variable. Again, this
    means you save function calls if you use you program more that once
    consecutively. *)
val byte_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                    -> byte_bigarray -> 'a program
val ubyte_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                     -> ubyte_bigarray -> 'a program
val short_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                     -> short_bigarray -> 'a program
val ushort_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                      -> ushort_bigarray -> 'a program
val uint_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                    -> uint_bigarray -> 'a program
val float_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                     -> float_bigarray -> 'a program

(** The recommanded way: use buffer ! not array ! *)
val buffer_cst_attr : 'a program -> ?norm:bool -> ?stride:int -> string
                      -> 'b array_buffer -> 'a program

(** The next functions are passing uniform variables from scalar to vec4, to
    the vertex and fragment shaders, this is the "dynamic version changing
    the type of your program". The string is the variable name, as above. *)
val int1_uniform : 'a program -> string -> (int -> 'a) program
val bool1_uniform : 'a program -> string -> (bool -> 'a) program
val float1_uniform : 'a program -> string -> (float -> 'a) program
val int2_uniform : 'a program -> string -> (int -> int -> 'a) program
val bool2_uniform : 'a program -> string -> (bool -> bool -> 'a) program
val float2_uniform : 'a program -> string -> (float -> float -> 'a) program
val int3_uniform : 'a program -> string -> (int -> int -> int -> 'a) program
val bool3_uniform : 'a program -> string -> (bool -> bool -> bool -> 'a) program
val float3_uniform : 'a program -> string
                     -> (float -> float -> float -> 'a) program
val int4_uniform : 'a program -> string
                   -> (int -> int -> int -> int -> 'a) program
val bool4_uniform : 'a program -> string
                    -> (bool -> bool -> bool -> bool -> 'a) program
val float4_uniform : 'a program -> string
                     -> (float -> float -> float -> float -> 'a) program

(** The same as constant. *)
val int1_cst_uniform : 'a program -> string -> int -> 'a program
val bool1_cst_uniform : 'a program -> string -> bool -> 'a program
val float1_cst_uniform : 'a program -> string -> float -> 'a program
val int2_cst_uniform : 'a program -> string -> int -> int -> 'a program
val bool2_cst_uniform : 'a program -> string -> bool -> bool -> 'a program
val float2_cst_uniform : 'a program -> string -> float -> float -> 'a program
val int3_cst_uniform : 'a program -> string -> int -> int -> int -> 'a program
val bool3_cst_uniform : 'a program -> string -> bool -> bool -> bool
                        -> 'a program
val float3_cst_uniform : 'a program -> string -> float -> float -> float
                         -> 'a program
val int4_cst_uniform : 'a program -> string -> int -> int -> int -> int
                       -> 'a program
val bool4_cst_uniform : 'a program -> string -> bool -> bool -> bool -> bool
                        -> 'a program
val float4_cst_uniform : 'a program -> string -> float -> float -> float -> float
                         -> 'a program

(** Functions to parametrize a shader by uniform array variable. *)
val int1v_uniform : 'a program -> string -> (int array -> 'a) program
val int2v_uniform : 'a program -> string -> (int array -> 'a) program
val int3v_uniform : 'a program -> string -> (int array -> 'a) program
val int4v_uniform : 'a program -> string -> (int array -> 'a) program
val bool1v_uniform : 'a program -> string -> (bool array -> 'a) program
val bool2v_uniform : 'a program -> string -> (bool array -> 'a) program
val bool3v_uniform : 'a program -> string -> (bool array -> 'a) program
val bool4v_uniform : 'a program -> string -> (bool array -> 'a) program
val float1v_uniform : 'a program -> string -> (float array -> 'a) program
val float2v_uniform : 'a program -> string -> (float array -> 'a) program
val float3v_uniform : 'a program -> string -> (float array -> 'a) program
val float4v_uniform : 'a program -> string -> (float array -> 'a) program
val float_mat2_uniform : 'a program -> string -> (float array -> 'a) program
val float_mat3_uniform : 'a program -> string -> (float array -> 'a) program
val float_mat4_uniform : 'a program -> string -> (float array -> 'a) program
val texture_2d_uniform : 'a program -> string -> (gc_texture -> 'a) program

(** Functions to give a fixed value to a uniform variable, using array. *)
val int1v_cst_uniform : 'a program -> string -> int array -> 'a program
val int2v_cst_uniform : 'a program -> string -> int array -> 'a program
val int3v_cst_uniform : 'a program -> string -> int array -> 'a program
val int4v_cst_uniform : 'a program -> string -> int array -> 'a program
val bool1v_cst_uniform : 'a program -> string -> bool array -> 'a program
val bool2v_cst_uniform : 'a program -> string -> bool array -> 'a program
val bool3v_cst_uniform : 'a program -> string -> bool array -> 'a program
val bool4v_cst_uniform : 'a program -> string -> bool array -> 'a program
val float1v_cst_uniform : 'a program -> string -> float array -> 'a program
val float2v_cst_uniform : 'a program -> string -> float array -> 'a program
val float3v_cst_uniform : 'a program -> string -> float array -> 'a program
val float4v_cst_uniform : 'a program -> string -> float array -> 'a program
val float_mat2_cst_uniform : 'a program -> string -> float array -> 'a program
val float_mat3_cst_uniform : 'a program -> string -> float array -> 'a program
val float_mat4_cst_uniform : 'a program -> string -> float array -> 'a program
val texture_2d_cst_uniform : 'a program -> string -> gc_texture -> 'a program

(** Probably useless function ... you can call it to "disable" some texture
    etc ... that are still enabled because passed as contants. *)
val clean : unit -> unit
