(* Extending the language with thread ids *)

open Effect
open Effect.Deep
open Printf 

(*Uses of effects: to implement concurrency!*)

type location = int 

type lock = int 

type id = int 

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

type config = expr * id 

let config_to_str (c, id) = 
  sprintf "<%s, id%d>" (expr_to_str c) id 

type _ Effect.t += ThreadDeref: id * location -> expr t;;
type _ Effect.t += ThreadAssign: (id * location * int) -> expr t;;
type _ Effect.t += ThreadLock: id * lock -> expr t;;
type _ Effect.t += ThreadUnlock: id * lock -> expr t;;

let rec expr_step e id = match e with 
  | Skip -> Skip
  | Seq(Skip, e) -> e 
  | Seq(e1, e2) -> Seq(expr_step e1 id, e2)
  | Num(n) -> Num(n) 
  | Plus(Num(n1), Num(n2)) -> Num(n1+n2)
  | Plus(Num(n1), e2) -> Plus(Num(n1), expr_step e2 id)
  | Plus(e1, e2) -> Plus(expr_step e1 id, e2)
  | Deref(l) -> perform (ThreadDeref (id, l))
  | Assign(l, Num(n)) -> perform (ThreadAssign (id, l, n))
  | Assign(l, e) -> Assign(l, expr_step e id)
  | Lock m -> perform (ThreadLock (id, m))
  | Unlock m -> perform (ThreadUnlock (id, m))

let config_step (e, id) = (expr_step e id, id)

let is_value (e, _) = match e with  
  | Skip 
  | Num(_) -> true
  | _ -> false 

let rec schedule (es: config Queue.t) = 
  if Queue.is_empty es then () else 
  let next_thread = Queue.pop es in 
  (if is_value next_thread then 
    let (v, id) = next_thread in 
    printf "Thread %d terminated with value %s\n\n" id (expr_to_str v)
  else 
    (let stepped = config_step next_thread in 
    let (e, _) = next_thread in 
    let (e', id) = stepped in
    printf "Thread %d Step: %s ↝ %s\n\n" (id) (expr_to_str e) (expr_to_str e'); 
    Queue.push stepped es));
  schedule es

let run_process es = 
  let s = ref (fun _ -> 0) in     (*all locations initialised to zero*)
  let m = ref (fun _ -> false) in (* all locks unlocked *)
  try schedule es with 
  | effect (ThreadDeref (id, l)), k -> 
      let n = (!s)(l) in
      printf "Thread %d: Loading value at location l%d: %d\n" id l n; 
      continue k (Num (n))
  | effect (ThreadAssign (id, l, v)), k -> 
      printf "Thread %d: Updating location l%d to %d\n" id l v; 
      s := (fun x -> if l = x then v else (!s)(x)); 
      continue k Skip
  | effect (ThreadLock (id, l)), k ->
    printf "Thread %d: Attempting to lock lock %d\n" id l; 
    let locked = (!m)(l) in 
    if locked then 
      (printf "Global: Lock %d is locked, retrying in future\n" l;
      continue k (Lock l))
    else
      (printf "Global: Lock %d locked\n" l; 
      m := (fun x -> if l = x then true else (!m)(x)); 
      continue k Skip)
  | effect (ThreadUnlock (id, l)), k ->
      printf "Thread %d: Lock %d unlocked\n" id l;
      m := (fun x -> if l = x then false else (!m)(x)); 
      continue k Skip


let thread_loader (es: expr list) = 
  let eq = Queue.create() in 
  let i = ref 0 in
  List.iter (fun x -> Queue.push (x, !i) eq; i := !i + 1) es;
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