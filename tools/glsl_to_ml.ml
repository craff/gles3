open Printf

let glsl_file =
  match Sys.argv with
  | [|_; f|] -> f
  | _        -> eprintf "Usage: %s file.glsl > file.ml\n%!" Sys.argv.(0);
                exit 1

let _ =
  let ic = open_in glsl_file in
  printf "let str = {glsl|";
  try while true do
    let c = input_char ic in
    output_char stdout c
  done with End_of_file ->
    close_in ic;
    printf "|glsl}\n%!"

