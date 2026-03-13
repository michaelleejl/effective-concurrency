exception R of int;; 

try 1 + raise (R 2) with 
 | R(x) -> 2 + x

 (*Question: How do we handle the raised exception by 
             returning to the point it was raised, and
             "replaying" it with a value? For example, 
             replay it with 2 + x *)

(* What do we need to change? *)

(* First, we need some notion of "the point it was raised" *)
(* This is the evaluation context that was discarded E *)
(* In the previous example, 1 + [-] *)
(* we call this k, for continuation, and extend our handler to accept k *)
(* try 1 + raise (R 2) with 
 | R(x), k -> 2 + x
 *)

(* Second, now that we have k, we need the ability to use it: to *)
(* plug the hole that it exposes. The expression we use is *)
(* continue k v*)
(* Which will plug the hole in k with v.*)

(* try 1 + raise (R 2) with 
 | R(x), k -> continue k (2 + x)
 *)

(* Barring syntactic differences, that's basically all we need, operationally *)
(* What does the program above step to? *)

(* Note on types: we need R to have type int (payload) -> int (response/answer) *)


