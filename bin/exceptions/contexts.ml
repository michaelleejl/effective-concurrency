exception R of int;; 

try (); 1 + raise (R 2) with 
 | R(x) -> 2 + x

 (* How do we see the evaluation contexts at the point of a raise? *)
 (* One way to see them is via nesting: try [(); [1 + [-]]] with R(x) -> 2+x *)
 (* Another way to see them is as a stack!*)


    (* ---------------------------- *)
    (* |         raise R 2        | *)
    (* ---------------------------- *)
    (* |          1 + [-]         | *)
    (* ---------------------------- *)
    (* |         () ; [-]         | *)
    (* ---------------------------- *)
    (* | try [-] with R(x) -> 2+x | *)