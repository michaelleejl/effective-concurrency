open Effective_concurrency.Bool

exception R of int;;

(* Starter program *)
try 1 + raise (R 2) with 
  | R(x) -> 2 + x;;

(* Why is this program well-typed? *)
(* What evaluation contexts E can raise R 2 appear in?*)
(* What does that tell you about the type of raise? *)
try "a" ^ raise (R 2) with 
  | R(_) -> "hello";;

(* Why is this program ill-typed? *)
try "a" ^ raise (R "bad") with 
  | R(_) -> "hello";;

(* Why is this program ill-typed in OCaml? *)
(* Does your type system allow this program? *)
(* What are the tradeoffs of allowing this program? *)
try 1 + raise (R 2) with 
  | R(_) -> "hello";;

(* Why is this program ill-typed in OCaml? *)
(* Does your type system allow this program? *)
(* Does this system have type safety? *)

try if b then 1 else raise (R 2) with 
  | R(x) -> x >= 0;;