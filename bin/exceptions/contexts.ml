1 + (2 + 3*4);;
(* What is the evaluation context when I am evaluating 3 * 4?  *)
(* What is the evaluation context when I am evaluating 2 + 3*4? *)

exception R of int;;

try
  1 + raise (R 2)
with R x -> 2 + x;;

(* What is the evaluation context E that is
   discarded by the raise? *)

try
  (); 1 + raise (R 2) 
with R x -> 2 + x;;

(* What is the evaluation context E that is
   discarded by the raise? *)


try
  (1+2) + raise (R 2) 
with R x -> 2 + x;;

(* What is the evaluation context E that is
   discarded by the raise? *)

try
  1+(2 + raise (R 2))
with R x -> 2 + x;;

(* What is the evaluation context E that is
   discarded by the raise? *)

try
  1 + raise (R 2) ; 3 
with R x -> 2 + x;;

(* What is the evaluation context E that is
   discarded by the raise? *)

(* How do we see the evaluation contexts at the point of a raise? *)
(* Another way to see them is as a stack!*)

(* ---------------------------- *)
(* |         raise R 2        | *)
(* ---------------------------- *)
(* |          1 + [-]         | *)
(* ---------------------------- *)
(* |         [-] ; 3          | *)
(* ---------------------------- *)
(* | try [-] with R(x) -> 2+x | *)

(*Can we build up the evaluation contexts explicitly, and 
  apply the evaluation contexts once we hit the base case?*)
let rec sum xs = match xs with 
  | [] -> 0 
  | x::xs -> x + sum xs

