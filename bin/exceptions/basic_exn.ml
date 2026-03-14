exception R of int;;

(* Why is this line important?*)

try 1 + raise (R 2) with R x -> 2 + x
(* What will this evaluate to? *)
