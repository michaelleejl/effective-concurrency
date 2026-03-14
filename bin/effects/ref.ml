(* Here's an example of how we use effects to build a semantics for ref *)
(* Remember that in L3, we have a ref keyword, that gives us new locations *)
(* How do we know how to create new locations?*)

open Effect
open Effect.Deep
open Printf

type location = Location of int

let loc_to_string (Location n) = sprintf "l%d" n

(* Define a print effecct *)
type _ Effect.t += Ref : int -> location t

(* We can use it like so *)
let p () =
  let x = perform (Ref 0) in
  let y = perform (Ref 1) in
  printf "x: %s\n" (loc_to_string x);
  printf "y: %s\n" (loc_to_string y)

(* and we can figure out how to allocate new locations in the handler *)

(* this handler increments locations, one by one *)
let gen_loc_inc p =
  let s = Hashtbl.create 10 in
  let n = ref 0 in
  try p ()
  with effect Ref x, k ->
    let loc = !n in
    n := !n + 1;
    let _ = Hashtbl.add s loc x in
    continue k (Location loc)

(* this handler increments locations, two by two *)
let gen_loc_inc2 p =
  let s = Hashtbl.create 10 in
  let n = ref 0 in
  try p ()
  with effect Ref x, k ->
    let loc = !n in
    n := !n + 2;
    (* this is the change *)
    let _ = Hashtbl.add s loc x in
    continue k (Location loc)

(* Usage: run gen_loc_inc p and gen_loc_inc2 p*)
