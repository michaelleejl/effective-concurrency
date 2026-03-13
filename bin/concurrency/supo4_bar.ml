(* Extending the language with thread ids *)

open Effect
open Effect.Deep
open Printf 

(*Uses of effects: to implement concurrency!*)

type location = int 

type lock = int 

type id = int 

type sem = int 

type barrier = int 

let beta b = if b = 0 then 3 else 1

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
           | Barrier of barrier 
           | BarrierW of barrier 

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
  | Barrier b -> sprintf "barrier(%d)" b
  | BarrierW b -> sprintf "barrierW(%d)" b


type config = expr * id 

let config_to_str (c, id) = 
  sprintf "<%s, id%d>" (expr_to_str c) id 

type _ Effect.t += ThreadDeref: id * location -> expr t;;
type _ Effect.t += ThreadAssign: (id * location * int) -> expr t;;
type _ Effect.t += ThreadLock: id * lock -> expr t;;
type _ Effect.t += ThreadUnlock: id * lock -> expr t;;
type _ Effect.t += ThreadFork: expr -> expr t;;
type _ Effect.t += ThreadWait: id * sem -> expr t;;
type _ Effect.t += ThreadSignal: id * sem -> expr t;;
type _ Effect.t += ThreadBarrier: id * barrier -> expr t;;
type _ Effect.t += ThreadBarrierW: id * barrier -> expr t;;

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
  | Barrier b -> perform (ThreadBarrier (id, b))
  | BarrierW b -> perform (ThreadBarrierW (id, b))

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

type barrier_status = BarrierWait of int | BarrierRelease of int 

let run_process (es, i) = 
  let s = ref (fun _ -> 0) in     (*all locations initialised to zero*)
  let m = ref (fun _ -> None) in (* all locks unlocked *)
  let z = ref (fun _ -> 0) in (* all semaphores set to zero *)
  let b = ref (fun _ -> BarrierWait(0)) in 
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
  | effect (ThreadBarrier (id, barr)), k ->
      printf "Thread %d: Waiting on barrier %d\n" id barr;
      begin match (!b)(barr) with
        | BarrierWait(n) ->
            if n < beta(barr) - 1 then 
              (printf "Global: Increasing barrier count to %d\n" (n+1);
              b := (fun x -> if barr = x then BarrierWait(n+1) else (!b)(x)); 
              continue k (BarrierW barr))
            else
              (if n = beta(barr) - 1 then 
                (printf "Global: Switching barrier to release mode\n";
                b := (fun x -> if barr = x then BarrierRelease(n+1) else (!b)(x)); 
                continue k (BarrierW barr) )
              else 
                assert false )
        | BarrierRelease(_) -> 
              printf "Global: Barrier in release mode, blocking\n";
              continue k (Barrier barr) end 

  | effect (ThreadBarrierW (id, barr)), k ->
      printf "Thread %d: Waiting on barrier %d\n" id barr;
      (match (!b)(barr) with
      | BarrierRelease(n) ->
          if n > 1 then 
            (printf "Global: Releasing and decreasing barrier count to %d\n" (n-1);
            b := (fun x -> if barr = x then BarrierRelease(n-1) else (!b)(x)); 
            continue k (Skip))
          else
            (if n = 1 then 
              (printf "Global: Releasing and switching barrier to wait mode\n";
              b := (fun x -> if barr = x then BarrierWait(0) else (!b)(x)); 
              continue k (Skip)) 
            else 
              assert false )
      | BarrierWait(_) -> 
            assert false) 

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

let e10 = Seq(Barrier(0), 
              Num(0))
let e11 = Seq(Barrier(0), 
              Num(1))

let e12 = Seq(Barrier(0), 
              Num(2))
let process6 = thread_loader [e10; e11; e12]