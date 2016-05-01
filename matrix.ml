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

type matrix = float array

let idt = [| 1.0; 0.0; 0.0; 0.0;
	     0.0; 1.0; 0.0; 0.0;
	     0.0; 0.0; 1.0; 0.0;
	     0.0; 0.0; 0.0; 1.0 |]

let idt3 = [| 1.0; 0.0; 0.0;
	     0.0; 1.0; 0.0;
	     0.0; 0.0; 1.0; |]

let mul m2 m1 =
  Array.init 16 (fun i ->
    let j = i mod 4 in
    let i = i - j in
    m1.(i    ) *. m2.(      j) +.
    m1.(i + 1) *. m2.(4   + j) +.
    m1.(i + 2) *. m2.(4*2 + j) +.
    m1.(i + 3) *. m2.(4*3 + j))

let pi = 3.141592653589793

let perspective fovy aspect near far =
  let angle = fovy/.360.0*.pi in
  let f = cos(angle) /. sin(angle) in
  let g = f /. aspect in
  let d = near -. far in
  let a = (far +. near) /. d in
  let b = (2.0 *. far *. near) /. d in
  [|   g; 0.0; 0.0; 0.0;
     0.0;   f; 0.0; 0.0;
     0.0; 0.0;   a; -1.;
     0.0; 0.0;   b; 0.0 |]

let lookat eyePos centerPos eyeUp =
  let ex,ey,ez =
    match eyePos with
      [|ex;ey;ez|] -> ex,ey,ez
    | [|ex;ey;ez;ew|] -> ex/.ew, ey/.ew, ez/.ew
    | _ -> invalid_arg "lookat"
  in
  let cx,cy,cz =
    match centerPos with
      [|cx;cy;cz|] -> cx,cy,cz
    | [|cx;cy;cz;cw|] -> cx/.cw, cy/.cw, cz/.cw
    | _ -> invalid_arg "lookat"
  in
  let ux,uy,uz =
    match eyeUp with
      [|ux;uy;uz|] -> ux,uy,uz
    | [|ux;uy;uz;uw|] -> ux/.uw, uy/.uw, uz/.uw
    | _ -> invalid_arg "lookat"
  in
  let fx = cx -. ex and fy = cy -. ey and fz = cz -. ez in
  let fn = sqrt (fx *. fx +. fy *. fy +. fz *. fz) in
  let fx = fx /. fn and fy = fy /. fn and fz = fz /. fn in
  let un = sqrt (ux *. ux +. uy *. uy +. uz *. uz) in
  let ux = ux /. un and uy = uy /. un and uz = uz /. un in
  let sx =    fy *. uz -. fz *. uy in
  let sy = -. fx *. uz +. fz *. ux in
  let sz =    fx *. uy -. fy *. ux in
  let sn = sqrt (sx *. sx +. sy *. sy +. sz *. sz) in
  let ux = (   sy *. fz -. sz *. fy) /. sn in
  let uy = (-. sx *. fz +. sz *. fx) /. sn in
  let uz = (   sx *. fy -. sy *. fx) /. sn in
  let st = -.sx*.ex -.sy*.ey-.sz*.ez in
  let ut = -.ux*.ex -.uy*.ey-.uz*.ez in
  let ft = fx*.ex +.fy*.ey+.fz*.ez in
  [|sx;ux;-.fx;0.;
    sy;uy;-.fy;0.;
    sz;uz;-.fz;0.;
    st;ut;ft;1.|]

let translate x y z =
  [| 1.0; 0.0; 0.0; 0.0;
     0.0; 1.0; 0.0; 0.0;
     0.0; 0.0; 1.0; 0.0;
     x; y; z; 1.0 |]

let rotateX t =
  [|1.0;0.0;0.0;0.0;
    0.0; cos t;  sin t;0.0;
    0.0;-.sin t;cos t;0.0;
    0.0;0.0;0.0;1.0|]

let rotateY t =
  [|cos t;  0.0;sin t;0.0;
    0.0;    1.0;0.0;0.0;
    -.sin t;0.0;cos t;0.0;
    0.0;0.0;0.0;1.0|]

let rotateZ t =
  [|cos t;  sin t;0.0;0.0;
    -.sin t;cos t;0.0;0.0;
    0.0;0.0; 1.0;0.0;
    0.0;0.0;0.0;1.0|]

let scale a =
  [|a;0.0;0.0;0.0;
    0.0;a;0.0;0.0;
    0.0;0.0;a;0.0;
    0.0;0.0;0.0;1.0|]

let transpose a =
  [|a.(0); a.(4); a.(8); a.(12);
    a.(1); a.(5); a.(9); a.(13);
    a.(2); a.(6); a.(10); a.(14);
    a.(3); a.(7); a.(11); a.(15)|]

let inverse a =
  let a = Array.copy a in
  let b = Array.init 16 (fun i -> if i mod 5 = 0 then 1.0 else 0.0) in
  let get_a i j = a.(i*4+j) in
  let set_a i j x = a.(i*4+j) <- x in
  let get_b i j = b.(i*4+j) in
  let set_b i j x = b.(i*4+j) <- x in
  for i = 0 to 2 do
    let k = ref 1 in
    for j = i to 2 do
      if abs_float (get_a i j) > abs_float (get_a i !k) then k := j
    done;
    let k = !k in
    for j = 0 to 3 do
      let (x,y) = get_a i j, get_a k j in
      set_a i j y; set_a k j x;
      let (x,y) = get_b i j, get_b k j in
      set_b i j y; set_b k j x;
    done;
    let x = get_a i i in
    for j = i+1 to 3 do
      let y = get_a j i in
      let z = y /. x in
      set_a j i 0.0;
      for k = i+1 to 3 do
	set_a j k (get_a j k -. z *. get_a i k)
      done;
      for k = 0 to 3 do
	set_b j k (get_b j k -. z *. get_b i k)
      done;
    done;
  done;
  for i = 0 to 3 do
    let x = 1. /. get_a i i in
    for j = 0 to 3 do
      set_a i j (x *. get_a i j);
      set_b i j (x *. get_b i j);
    done
  done;
  for i = 3 downto 1 do
    for j = 0 to i-1 do
      let y = get_a j i in
      for k = 0 to 3 do
	set_b j k (get_b j k -. y *. get_b i k)
      done
    done
  done;
  b

let normalMatrix m =
  let m = inverse (transpose m) in
  [| m.(0); m.(1); m.(2);
     m.(4); m.(5); m.(6);
     m.(8); m.(9); m.(10) |]

(*
let test1 () =
  let a = Array.init 16 (fun _ -> Random.float 2.0 -. 1.0) in
  let b = inverse a in
  let c = mul a b in
  a,b,c
*)
