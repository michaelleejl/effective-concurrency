(* Extending the language with binary semaphores *)

open Effect
open Effect.Deep
open Printf 

type location = int 

type lock = int 

type id = int 

type sem = int 

(* skip, numbers, loads, stores, sequencing *)
type expr = Skip | Seq of expr * expr 
           | Num of int 
           | Plus of expr * expr
           | Deref of location | Assign of location * expr
           | Lock of lock 
           | Unlock of lock 
           | Fork of expr 
           | Wait of sem 
           | Signal of sem 
let rec expr_to_str = function 
  | Skip -> "skip"
  | Seq (e1, e2) -> sprintf "%s ; %s" (expr_to_str e1) (expr_to_str e2)
  | Num n -> sprintf "%d" n 
  | Plus (e1, e2) -> sprintf "%s + %s" (expr_to_str e1) (expr_to_str e2)
  | Deref l -> sprintf "!l%d" l 
  | Assign (l, e) -> sprintf "l%d := %s" l (expr_to_str e) 
  | Lock m -> sprintf "lock %d" m
  | Unlock m -> sprintf "unlock %d" m 
  | Fork e -> sprintf "fork(%s)" (expr_to_str e)
  | Wait z -> sprintf "wait(%d)" z
  | Signal z -> sprintf "signal(%d)" z


type config = expr * id 

type _ Effect.t += ThreadDeref: id * location -> expr t;;
type _ Effect.t += ThreadAssign: (id * location * int) -> expr t;;
type _ Effect.t += ThreadLock: id * lock -> expr t;;
type _ Effect.t += ThreadUnlock: id * lock -> expr t;;
type _ Effect.t += ThreadFork: expr -> expr t;;
type _ Effect.t += ThreadWait: id * sem -> expr t;;
type _ Effect.t += ThreadSignal: id * sem -> expr t;;
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
  | Fork e -> perform (ThreadFork e)
  | Wait s -> perform (ThreadWait (id, s))
  | Signal s -> perform (ThreadSignal (id, s))

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

let run_process (es, i) = 
  let s = ref (fun _ -> 0) in     (*all locations initialised to zero*)
  let m = ref (fun _ -> None) in (* all locks unlocked *)
  let z = ref (fun _ -> 0) in (* all semaphores set to zero *)
  let next_id = ref i in 
  try schedule es with 
  | effect (ThreadFork e), k -> 
      let id = !next_id in
      next_id := id + 1;
      printf "Forking new thread %d\n" id;
      Queue.push (e, id) es;
      continue k Skip
  | effect (ThreadDeref (id, l)), k -> 
      let n = (!s)(l) in
      printf "Thread %d: Loading value at location l%d: %d\n" id l n; 
      continue k (Num (n))
  | effect (ThreadAssign (id, l, v)), k -> 
      printf "Thread %d: Updating location l%d to %d\n" id l v; 
      s := (fun x -> if l = x then v else (!s)(x)); 
      continue k Skip
  | effect (ThreadLock (id, l)), k ->
    printf "Thread %d: Attempting lock of lock %d\n" id l;
    begin match (!m)(l) with
    | None -> 
       (printf "Global: Lock %d locked\n" l; 
        m := (fun x -> if l = x then Some(id) else (!m)(x)); 
        continue k Skip)
    | Some(id') -> 
       if id = id' then
        (printf "Global: Lock %d is already locked by the thread\n" l;
          continue k (Skip))
       else 
        (printf "Global: Lock %d is locked, retrying in future\n" l;
          continue k (Lock l)) end 
  | effect (ThreadUnlock (id, l)), k ->
      printf "Thread %d: Attempting unlock of lock %d\n" id l;
      begin match (!m)(l) with 
      | None ->
          printf "Global: Lock %d already unlocked\n" l;
          continue k Skip
      | Some(id')  -> 
        if id = id' then
          (printf "Global: Lock %d unlocked\n" l;
          m := (fun x -> if l = x then None else (!m)(x)); 
          continue k Skip)
        else 
          (printf "Global: Cannot unlock. Lock %d locked by thread %d\n" l id';
          continue k (Unlock l)) end 
   | effect (ThreadWait (id, sem)), k ->
      printf "Thread %d: Waiting on semaphore %d\n" id sem;
      begin match (!z)(sem) with
      | 0 -> 
        (printf "Global: Semaphore %d is 0. Still waiting \n" sem; 
          continue k (Wait sem))
      | 1 -> 
        (printf "Global: Semaphore %d is 1. Continuing \n" sem;
          z := (fun x -> if sem = x then 0 else (!z)(x)); 
          continue k (Skip))
      | 2 -> assert false end 
  | effect (ThreadSignal (id, sem)), k ->
      printf "Thread %d: Signalling semaphore %d\n" id sem;
      begin match (!z)(sem) with
      | 0 -> 
        (printf "Global: Semaphore %d is 0. Signalling \n" sem; 
          z := (fun x -> if sem = x then 1 else (!z)(x)); 
          continue k (Skip))
      | 1 -> 
        (printf "Global: Semaphore %d is already 1. Continuing execution \n" sem;
          continue k (Skip))
      | 2 -> assert false end 
         

let thread_loader (es: expr list) = 
  let eq = Queue.create() in 
  let i = ref 0 in 
  List.iter (fun x -> Queue.push (x, !i) eq; i:=!i+1) es;
  (eq, !i)

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

let e7 = Seq(Fork(Num(1)), Fork(Plus(Num(2), Num(3))))

let process4 = thread_loader [e7];;

let e8 = Seq(Wait(0), 
             Assign(0, Num(1)))
let e9 = Seq(Assign(1, Deref(0)), Signal(0))

let process5 = thread_loader [e8; e9];;
