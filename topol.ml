exception Cykliczne

let update key f map = map |> PMap.add key (f (map |> PMap.find key))

let topol dependencies =
  (* Mapa z każdego wierzchołka na 0. *)
  let zeroes =
    List.fold_left
      (fun t (b, aa) ->
        List.fold_left (fun t a -> t |> PMap.add a 0) t aa |> PMap.add b 0)
      PMap.empty dependencies
  in

  (* Nazwy wierzchołków. *)
  let nodes = PMap.foldi (fun a _ t -> a :: t) zeroes [] in

  (* Graf to mapa z nazwy wierzchołka na listę nazw wierzchołków, do których
     istnieje krawędź. *)
  (* "Stopniem" nazywamy liczbę krawędzi wchodzących do wierzchołka. *)
  let graph_with_no_edges = PMap.map (fun _ -> []) zeroes in
  let graph, degrees =
    List.fold_left
      (fun acc (b, aa) ->
        List.fold_left
          (fun (graph, degrees) a ->
            ( graph |> update a (fun bb -> b :: bb),
              degrees |> update b (fun x -> x + 1) ))
          acc aa)
      (graph_with_no_edges, zeroes)
      dependencies
  in

  (* Na kolejce na początku są wierzchołki, które na początku mają stopień 0. *)
  let queue =
    PMap.foldi (fun a degree t -> if degree = 0 then a :: t else t) degrees []
  in

  (* [degrees] to aktualne stopnie wszystkich wierzchołków; [queue] to lista
     wierzchołków mających aktualnie stopień 0. Aktualny graf to [graph] bez
     wierzchołków, które są w [queue]. Wynik procedury to wierzchołki
     z aktualnego grafu, posortowane topologicznie, całość złączona z [acc].
     Chyba, że istnieje cykl, wtedy wynikiem jest jakiś mniejszy podzbiór tych
     wierzchołków. *)
  let rec walk degrees queue acc =
    match queue with
    | [] -> acc
    | a :: queue ->
        (* Usuwamy krawędzie wychodzące z [a]. Dodajemy do kolejki wierzchołki,
           które po takim zmniejszeniu miałyby stopień 0. *)
        let queue', degrees' =
          List.fold_left
            (fun (queue, degrees) b ->
              ( (if degrees |> PMap.find b = 1 then b :: queue else queue),
                degrees |> update b (fun x -> x - 1) ))
            (queue, degrees)
            (graph |> PMap.find a)
        in
        walk degrees' queue' (a :: acc)
  in

  let result = walk degrees queue [] in
  if List.length result < List.length nodes then raise Cykliczne else result
