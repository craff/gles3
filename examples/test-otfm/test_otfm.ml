

let read_font fname =
  let ch = open_in fname in
  let size = in_channel_length ch in
  let str = Bytes.create size in
  really_input ch str 0 size;
  Bytes.to_string str

let font = read_font "/usr/share/fonts/liberation-fonts-ttf-2.1.2/LiberationMono-Regular.ttf"

let dfont : Otfm.decoder = Otfm.decoder (`String font)

let get_result = function
    | Error e -> Format.eprintf "%a\n%!" Otfm.pp_error e; exit 1
    | Ok n -> n

let get_opt_result = function
    | Error e -> Format.eprintf "%a\n%!" Otfm.pp_error e; exit 1
    | Ok (Some n) -> n
    | Ok None -> Format.eprintf "NO RESULT\n%!"; exit 1
open Unicodelib

let name = get_opt_result (Otfm.postscript_name dfont)

let _ = Printf.printf "name: %s\n%!" name

let get_glyphs dfont =
  let fn acc kind (u1,u2) id =
    match kind with
    | `Glyph       -> assert (u1 = u2); (u1,id)::acc
    | `Glyph_range ->
       let l = ref acc in
       for i = u1 to u2 do
         l := (i,id+i-u1) :: !l
       done;
       !l
  in
  let (_,ids) = get_result (Otfm.cmap dfont fn []) in
  let cmp (u1,_) (u2,_) = compare u1 u2 in
  List.sort cmp ids

let bezier_split ls =
  let rec fn first acc1 acc2 l =
    match l with
    | [] -> List.rev (List.rev (first::acc2)::acc1)
    | (true,x,y)::l ->
       fn first (List.rev ((x,y)::acc2)::acc1) [(x,y)] l
    | (false,x,y)::l ->
       fn first acc1 ((x,y)::acc2) l
  in
  match ls with
  | [] -> assert false
  | (false,_,_)::_ -> assert false
  | (true,x,y)::l -> fn (x,y) [] [(x,y)] l

let glyphs = get_glyphs dfont

let _ = Printf.printf "%d glyphs\n%!" (List.length glyphs)

let string_to_ids str =
  List.rev (UTF8.fold (fun acc chr ->
                let n = Uchar.to_int chr in
                let id = List.assoc n glyphs in
                let loc = get_result (Otfm.loca dfont id) in
                let descr =
                  match loc with
                  | None -> None
                  | Some loc -> Some (get_result (Otfm.glyf dfont loc))
                in
                ((id,loc,descr)::acc)) [] str)

let hw = string_to_ids "Hello World été"

let print_descr ch (descr, (minx,miny,maxx,maxy)) =
  Printf.printf "bb:(%d,%d,%d,%d) " minx miny maxx maxy;
  match descr with
  | `Simple ls ->
     let lss = List.map bezier_split ls in
     List.iter (fun ls ->
         Printf.fprintf ch "[";
         List.iter (fun l ->
             Printf.fprintf ch "{";
             List.iter (fun (x,y) ->
                 Printf.fprintf ch "(%d,%d)" x y) l;
             Printf.fprintf ch "}") ls;
         Printf.fprintf ch "]") lss
  | `Composite ls ->
     Printf.printf "composite: \n%!";
     List.iter (fun (id,(tx,ty),tr) ->
         Printf.printf "    id: %d, x += %d, y += %d" id tx ty;
         (match tr with
         | None -> ()
         | Some (a,b,c,d) -> Printf.printf "mat: [[%g,%g],[%g,%g]]" a b c d);
         print_newline ()
       ) ls

let _ = List.iter (fun (n,loc,descr) ->
            let loc = if loc = None then "NOLOC" else "" in
            Printf.printf "%d %s\n%!" n loc;
            match descr with
            | None -> ()
            | Some d -> Printf.printf "  %a\n%!" print_descr d
          ) hw;
        print_newline ()
