open Effect
open Effect.Deep
open Printf

(*Uses of effects: to implement concurrency!*)

(* skip, numbers, loads, stores, sequencing *)
type expr = Skip | Num of int | Seq of expr * expr | Load | Store of int

let rec expr_to_str = function
  | Skip -> "skip"
  | Num n -> sprintf "%d" n
  | Seq (e1, e2) -> sprintf "%s ; %s" (expr_to_str e1) (expr_to_str e2)
  | Load -> "load"
  | Store v -> sprintf "store %d" v

type _ Effect.t += ThreadStore : int -> expr t
type _ Effect.t += ThreadLoad : expr t

let rec step = function
  | Skip -> Skip
  | Num n -> Num n
  | Load -> perform ThreadLoad
  | Store v -> perform (ThreadStore v)
  | Seq (Skip, e) -> e
  | Seq (e1, e2) -> Seq (step e1, e2)

let is_value = function Skip | Num _ -> true | _ -> false

(* round robin thread scheduling: each thread is allowed to take a step 
   in turn *)
let rec schedule eq =
  if Queue.is_empty eq then ()
  else
    let next_thread = Queue.pop eq in
    (if is_value next_thread then
       printf "Thread terminated with value %s\n" (expr_to_str next_thread)
     else
       let stepped = step next_thread in
       printf "Step: %s ↝ %s\n" (expr_to_str next_thread) (expr_to_str stepped);
       Queue.push stepped eq);
    schedule eq

let run_process es =
  let store = ref 0 in
  try schedule es with
  | effect ThreadLoad, k ->
      let n = !store in
      printf "Loading value %d\n" n;
      continue k (Num n)
  | effect ThreadStore v, k ->
      printf "Updating store to %d\n" v;
      store := v;
      continue k Skip

let thread_loader (es : expr list) =
  let eq = Queue.create () in
  List.iter (fun x -> Queue.push x eq) es;
  eq

let e1 = Seq (Store 3, Skip)
let e2 = Load
let process1 = thread_loader [ e1; e2 ]
