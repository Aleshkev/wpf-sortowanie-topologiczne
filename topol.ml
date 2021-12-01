exception Cykliczne

open List

let uniq = function
  | [] -> []
  | x :: t ->
      fold_left (fun (x', r) x -> x, if x = x' then r else x :: r) (x, [ x ]) t
      |> snd |> rev

let find x arr =
  let rec f lo hi =
    if lo >= hi
    then lo
    else
      let mid = (lo + hi) / 2 in
      if arr.(mid) < x then f (mid + 1) hi else f lo mid
  in
  f 0 (Array.length arr)

let fold_edges f acc v =
  List.fold_left
    (fun acc (y, row) -> fold_left (fun acc x -> f acc x y) acc row)
    acc v

let topol dependencies =
  let labels = dependencies |> fold_edges (fun acc a b -> a :: b :: acc) [] in
  let labels = dependencies |> fold_left (fun acc (b, _) -> b :: acc) labels in
  let labels = labels |> List.sort Stdlib.compare |> uniq in
  let labels = Array.of_list labels in
  let n_labels = Array.length labels in
  let index a = labels |> find a in

  let degrees = Array.make n_labels 0 in
  let graph = Array.make n_labels [] in
  fold_edges
    (fun () a b ->
      let a' = index a and b' = index b in
      degrees.(b') <- degrees.(b') + 1;
      graph.(a') <- b' :: graph.(a');
      ())
    () dependencies;

  let queue = ref [] in
  Array.iteri (fun i x -> if x = 0 then queue := i :: !queue) degrees;

  let result = ref [] in
  while !queue <> [] do
    let a = hd !queue in
    result := labels.(a) :: !result;
    queue := tl !queue;
    iter
      (fun b ->
        degrees.(b) <- degrees.(b) - 1;
        if degrees.(b) = 0 then queue := b :: !queue)
      graph.(a)
  done;
  if length !result < n_labels then raise Cykliczne else !result
