type matrix = float array

(** Basic functions on 4x4 matrices (in column major representation
    as expected by OpenGL/GLES*)

val pi : float
(** pi is usefull for ratations *)

val idt : float array
(** identity matrices *)

val mul : float array -> float array -> float array
(** multiplication *)

val perspective : float -> float -> float -> float -> float array
(** [perpective fovy ratio near far] computes a perspective matrix
    using [fovy] as focal angle, [ratio] for the ratio width/height of
    the screen, [near] ( > 0) as near plane distance and [far] (>
    near) as far plane distance.  Recall that nothing is drawn if not
    between the near and far plane, however, the precision of the
    depth test get worst when the near and far plane are further
    apart.
*)

val lookat : float array -> float array -> float array -> float array
(** [lookat position center up] computes an isometry matrice
    to place the camera at the given [position] looking at
    then [center] (this point will be in the center of the screen) and
    using [up] to be vertical.

    [up] and [center] must not be colinear. *)

val translate : float -> float -> float -> float array
(** [tranlate x y z] computes a translation matrix *)

val rotateX : float -> float array
(** [rotateX angle] rotation matrix around the X axes. *)

val rotateY : float -> float array
(** [rotateX angle] rotation matrix around the Y axes. *)

val rotateZ : float -> float array
(** [rotateX angle] rotation matrix around the Z axes. *)

val scale : float -> float array
(** [scaling] by a given factor. *)

val transpose : float array -> float array
(** [transpose] *)

val inverse : float array -> float array
(** [inverse] *)

val normalMatrix : float array -> float array
(** [normalMatrix m] returns the upper left 3x3 matrices in [inverse (transpose m)].*)
