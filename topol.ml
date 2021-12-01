exception Cykliczne

(** Zamienia w [map] wartość dla [key] z jakiegoś [x] na [f x] jeżeli [key] jest
    w [map], w przeciwnym wypadku wstawia [f default]. *)
let update key f default map =
  map |> PMap.add key (f (try map |> PMap.find key with Not_found -> default))

let topol dependencies =
  (* Graf to mapa z nazwy wierzchołka na listę nazw wierzchołków, do których
     istnieje krawędź. "Stopniem" nazywamy liczbę krawędzi wchodzących do
     wierzchołka. Update'ujemy z identycznością żeby na pewno były klucze dla
     wszystkich wierzchołków z wejścia. *)
  let graph, degrees =
    List.fold_left
      (fun (graph, degrees) (b, aa) ->
        let graph = graph |> update b Fun.id []
        and degrees = degrees |> update b Fun.id 0 in
        List.fold_left
          (fun (graph, degrees) a ->
            ( graph |> update a (fun bb -> b :: bb) [],
              degrees |> update a Fun.id 0 |> update b (fun x -> x + 1) 0 ))
          (graph, degrees) aa)
      (PMap.empty, PMap.empty) dependencies
  in

  (* Wszystkie nazwy wierzchołków. *)
  let nodes = PMap.foldi (fun a _ t -> a :: t) graph [] in

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
                degrees |> update b (fun x -> x - 1) 0 ))
            (queue, degrees)
            (graph |> PMap.find a)
        in
        walk degrees' queue' (a :: acc)
  in

  (* Na kolejce na początku są wierzchołki, które na początku mają stopień 0. *)
  let queue =
    PMap.foldi (fun a degree t -> if degree = 0 then a :: t else t) degrees []
  in

  let result = walk degrees queue [] in
  if List.length result < List.length nodes then raise Cykliczne else result
