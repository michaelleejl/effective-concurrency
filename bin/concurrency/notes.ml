(* An implementation of a cut down version of the language in the notes, 
   with effect handlers.
   I got rid of booleans, >=, if, and while. The focus will be on 
   concurrency.   *)

open Effect
open Effect.Deep
open Printf 

type location = int 

type lock = int 

(* skip, numbers, loads, stores, sequencing *)
type expr = Skip | Seq of expr * expr 
           | Num of int 
           | Plus of expr * expr
           | Deref of location | Assign of location * expr
           | Lock of lock 
           | Unlock of lock 
let rec expr_to_str = function 
  | Skip -> "skip"
  | Seq (e1, e2) -> sprintf "%s ; %s" (expr_to_str e1) (expr_to_str e2)
  | Num n -> sprintf "%d" n 
  | Plus (e1, e2) -> sprintf "%s + %s" (expr_to_str e1) (expr_to_str e2)
  | Deref l -> sprintf "!l%d" l 
  | Assign (l, e) -> sprintf "l%d := %s" l (expr_to_str e) 
  | Lock m -> sprintf "lock %d" m
  | Unlock m -> sprintf "unlock %d" m 

type _ Effect.t += ThreadDeref: location -> expr t;;
type _ Effect.t += ThreadAssign: (location * int) -> expr t;;
type _ Effect.t += ThreadLock: lock -> expr t;;
type _ Effect.t += ThreadUnlock: lock -> expr t;;

let rec step = function 
  | Skip -> Skip
  | Seq(Skip, e) -> e 
  | Seq(e1, e2) -> Seq(step e1, e2)
  | Num(n) -> Num(n) 
  | Plus(Num(n1), Num(n2)) -> Num(n1+n2)
  | Plus(Num(n1), e2) -> Plus(Num(n1), step e2)
  | Plus(e1, e2) -> Plus(step e1, e2)
  | Deref(l) -> perform (ThreadDeref l)
  | Assign(l, Num(n)) -> perform (ThreadAssign (l, n))
  | Assign(l, e) -> Assign(l, step e)
  | Lock m -> perform (ThreadLock m)
  | Unlock m -> perform (ThreadUnlock m)
 

let is_value = function 
  | Skip 
  | Num(_) -> true
  | _ -> false 

let rec schedule (es: expr Queue.t) = 
  if Queue.is_empty es then () else 
  let next_thread = Queue.pop es in 
  (if is_value next_thread then 
    printf "Thread terminated with value %s\n\n" (expr_to_str next_thread)
  else 
    (let stepped = step next_thread in 
    printf "Step: %s ↝ %s\n\n" (expr_to_str next_thread) (expr_to_str stepped); 
    Queue.push stepped es));
  schedule es

let run_process es = 
  let s = ref (fun _ -> 0) in     (*all locations initialised to zero*)
  let m = ref (fun _ -> false) in (* all locks unlocked *)
  try schedule es with 
  | effect (ThreadDeref l), k -> 
      let n = (!s)(l) in
      printf "Loading value at location l%d: %d\n" l n; 
      continue k (Num (n))
  | effect (ThreadAssign (l, v)), k -> 
      printf "Updating location l%d to %d\n" l v; 
      s := (fun x -> if l = x then v else (!s)(x)); 
      continue k Skip
  | effect (ThreadLock l), k ->
    printf "Attempting to lock lock %d\n" l; 
    let locked = (!m)(l) in 
    if locked then 
      (printf "Lock %d is locked, retrying in future\n" l;
      continue k (Lock l))
    else
      (printf "Lock %d locked\n" l; 
      m := (fun x -> if l = x then true else (!m)(x)); 
      continue k Skip)
  | effect (ThreadUnlock l), k ->
      printf "Lock %d unlocked\n" l;
      m := (fun x -> if l = x then false else (!m)(x)); 
      continue k Skip


let thread_loader (es: expr list) = 
  let eq = Queue.create() in 
  List.iter (fun x -> Queue.push x eq) es;
  eq

let e1 = Seq(Assign(0,Num(3)), Skip)
let e2 = Deref(0)

let process1 = thread_loader [e1; e2];;
  
let e3 = Seq(Lock(0), 
             Seq(Assign(0, Plus(Num(1), Num(2))), 
                 Unlock(0)))
let e4 = Seq(Lock(0), 
             Seq(Unlock(0),
                 Deref(0)))
let process2 = thread_loader [e3; e4];;

let e5 = Seq(Lock(0), 
             Seq(Assign(0, Plus(Num(1), Num(2))), 
                 Unlock(0)))
let e6 = Seq(Unlock(0),
                 Deref(0))

let process3 = thread_loader [e5; e6];;

