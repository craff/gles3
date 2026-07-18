module Make(H : Hashtbl.HashedType) = struct
  module T = Hashtbl.Make(H)

  type 'a t = ('a Weak.t) T.t

  let create n =
    T.create n

  let clear =
    T.clear

  let remove =
    T.remove

  let add tbl key value =
    let w = Weak.create 1 in
    Weak.set w 0 (Some value);
    T.replace tbl key w

  let replace = add

  let find_opt tbl key =
    match T.find_opt tbl key with
    | None ->
        None
    | Some w ->
        begin match Weak.get w 0 with
        | Some v ->
            Some v
        | None ->
            (* Dead entry: remove it lazily. *)
            T.remove tbl key;
            None
        end

  let find tbl key =
    match find_opt tbl key with
    | Some v -> v
    | None -> raise Not_found

  let mem tbl key =
    match find_opt tbl key with
    | Some _ -> true
    | None -> false

  let iter f tbl =
    T.filter_map_inplace
      (fun key w ->
         match Weak.get w 0 with
         | Some v -> f key v; Some w
         | None -> None)
      tbl

  let fold f tbl acc =
    let res = ref acc in
    T.iter (fun key v -> acc := f key v acc) tbl;
    !res

  let length tbl =
    let res = ref 0 in
    T.iter (fun _ _ -> incr res) tbl;
    !res

end
