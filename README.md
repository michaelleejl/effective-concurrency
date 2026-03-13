# Effective Concurrency

This repository contains a set of programs, designed to teach 
Part IB students at the University of Cambridge about the semantics of 
exceptions, effect, and concurrency. 

## Repository Structure (and Teaching Guide)
The `lib` directory is structured as follows

```
bin/
├─── exceptions/
│    └─── basic_exn.ml
│           A program that illustrates basic exceptions in OCaml 
│    └─── contexts.ml
│           A program that illustrates how context could be thought about
│    └─── deepest.ml
│           A program that illustrates the importance of deepest-handler semantics
│    └─── typing.ml
│           Programs that illustrate various issues with typing exceptions
├─── effects/
│    └─── basic_eff.ml
│    │      A commentary on how to go from exceptions to effects
│    └─── effects.ml
│    │      The end result of basic_eff. Worth showing side-by-side.
│    └─── ref.ml
│           An illustration of how effect handlers can be used to
│           generate new locations, implementing the semantics of ref
├─── concurrency/
│    └─── basic_con.ml
│    │      An illustration of how effect handlers can be used to
│    │      synchronise between two threads
│    └─── notes.ml     
│    │      An implementation of a cut-down version of the 
│    │      concurrent language in the notes using effect handlers
│    └─── supo0_id.ml 
│    │      Gives each thread an id
│    └─── supo1_fork.ml
│    │      Extends sup0_id with a fork primitive
│    └─── supo2_lock.ml
│    │      Extends sup1_fork with proper locking
│    └─── supo3_sem.ml 
│    │      Extends sup2_lock with binary semaphores
│    └─── supo4_barr.ml
│           Extends sup3_sem with thread barriers
```

I personally teach by going through the directories in the following order:
1. `exceptions`
2. `effects`
3. `concurrency`

### Thread Scheduling
Each of the modules in the `concurrency` directory uses the following helper 
functions:
1. `thread_loader` turns a list of threads into a queue of threads (and, later
    gives each an ID)
2. `scheduler` schedules each of the threads using a round robin policy, removing 
    threads once they terminate
3. `run_process` accepts a queue of threads, and handles the concurrent 
   operations by installing appropriate handlers. 

### lib

For completeness, `lib` contains a single file, 
```
bin/
└─── bool.ml 
```

Which exports a boolean constant `b = true`. I use this to illustrate why 
certain typing schemes for exceptions (allowing the 
type when an exception is raised to differ from the type 
of normal execution) do not have type soundness, since the value of `b`
is hidden from students.
