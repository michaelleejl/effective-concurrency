exception R of int;;

try 
  (try 1 + raise (R 2) with 
  | R(x) -> 2 + x) with 
| R(y) -> 3 + y 

(* What will this evaluate to? *)
(* How do you specify this behaviour in the semantics? *)