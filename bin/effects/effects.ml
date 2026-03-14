open Effect
open Effect.Deep

type _ Effect.t += R : int -> int t;;

(*Don't worry about what t is in int t*)

try 1 + perform (R 2) with effect R x, k -> continue k (2 + x)
