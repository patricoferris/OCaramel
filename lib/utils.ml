(* Tail-recursive mapping *)
let map f a = 
  let rec loop a acc = match a with 
    | []    -> acc 
    | x::xs -> loop xs ((f x)::acc)
  in
    List.rev (loop a [])

let map2 f a b =
  let rec loop a b acc = match a with 
    | [] -> acc
    | _  -> loop (List.tl a) (List.tl b) ( (f (List.hd a) (List.hd b)) :: acc)
  in
    List.rev (loop a b [])