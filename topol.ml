(*       Autor: Jonasz Aleszkiewicz *)
(* Code Review: Kacper Bal          *)

open List

exception Cykliczne

let topol dep =
  if dep = []
  then []
  else
    (* Wywołuje [f a b] na każdym [a b] że [a] ma być przed [b] w wyniku. *)
    let iter_edges f =
      dep |> iter (fun (y, row) -> iter (fun x -> f x y) row)
    in

    (* Przypisujemy każdemu wierzchołkowi liczbę całkowitą. *)
    let ids = Hashtbl.create (length dep) in
    let next_id = ref 0 in
    let new_label x =
      if Option.is_none @@ Hashtbl.find_opt ids x
      then (
        Hashtbl.add ids x !next_id;
        incr next_id)
    in
    iter_edges (fun a b ->
        new_label a;
        new_label b);
    dep |> iter (fun (b, _) -> new_label b);
    let id_of a = Hashtbl.find ids a in

    (* Tworzymy tablicę żeby dało się tę zamianę odwrócić. *)
    let n_nodes = Hashtbl.length ids in
    let labels = Array.make n_nodes (fst @@ hd dep) in
    ids |> Hashtbl.iter (fun k v -> labels.(v) <- k);
    let label_of a = labels.(a) in

    (* "Stopniem" nazywamy liczbę krawędzi wchodzących do wierzchołka. *)
    let degrees = Array.make n_nodes 0 in
    (* [graph.(a)] to lista wierzchołków, do których jest krawędź z [a]. *)
    let graph = Array.make n_nodes [] in
    iter_edges (fun a b ->
        let a' = id_of a and b' = id_of b in
        degrees.(b') <- degrees.(b') + 1;
        graph.(a') <- b' :: graph.(a'));

    (* Na kolejce zawsze będą wszystkie wierzchołki o "stopniu" 0. *)
    let queue = ref [] in
    degrees |> Array.iteri (fun i x -> if x = 0 then queue := i :: !queue);

    let result = ref [] in
    while !queue <> [] do
      let a = hd !queue in
      result := a :: !result;
      queue := tl !queue;

      graph.(a)
      |> iter (fun b ->
             degrees.(b) <- degrees.(b) - 1;
             if degrees.(b) = 0 then queue := b :: !queue)
    done;

    if length !result < n_nodes
    then (* Nie przeszliśmy wszystkich wierzchołków. *)
      raise Cykliczne
    else map label_of !result
