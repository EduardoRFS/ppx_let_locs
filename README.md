# ppx_let_locs

_ppx_let_locs_ is a typed ppx designed to improve stack traces when using `let monadic` over `lwt_ppx` and similars. It also improves stack traces of `Lwt.bind` and `>>=`.

## How to use

### Requirements

- ocaml@4.10.x || ocaml@4.12.x

### esy

```sh
esy add ppx_let_locs@EduardoRFS/ppx_let_locs#HASH_HERE
```

### opam

```sh
opam pin ppx_let_locs https://github.com/EduardoRFS/ppx_let_locs.git#HASH_HERE
```

### dune

Add it as `(preprocess (staged_pps ppx_let_locs))`, like:

```dune
(executable
 (name test)
 (preprocess
  (staged_pps ppx_let_locs)))
```

## How it works?

When a function `f` is called and there is also a `backtrace_f` or `f_backtrace` in the environment with a signature that matches `(exn -> exn) -> 'a`, it will apply the `backtrace_f` one and pass the additional argument with a `%reraise`, enhancing the exception stacktrace.

To achieve that it uses a copy of the OCaml typechecker with some additional hooks, so this PPX is an example on what is achieavable through a typed PPX.

Because of operators and let monadic cannot have a suffix of `_backtrace` in the OCaml syntax, this also provides an additional ppx `[@ppx_let_locs.use]` which may be used to conditionally adding the ppx.

### Examples

The following examples all works on OCaml and ReasonML.

**Lwt.bind**

```ocaml
(* you type *)
let from = Lwt.bind Lwt.return_unit (fun () -> Lwt.return_unit)

(* compiler see *)
let to_ = Lwt.backtrace_bind
  (fun exn ->
    let module Reraise = struct
      external reraise: exn -> 'a = "%reraise"
    end in
    try Reraise.reraise(exn) with exn -> exn)
  Lwt.return_unit
  (fun () -> Lwt.return_unit)
```

**let.await**

```reason
[@ppx_let_locs.use Lwt.backtrace_bind]
let (let.await) = Lwt.bind;

/* you type */
let from = {
  let.await () = Lwt.return_unit;
  Lwt.return_unit;
};

/* compiler see */
let to_ = {
  let.await_backtrace () = (
    exn => {
      module Reraise = {
        external reraise: exn => 'a = "%reraise";
      };
      try(Reraise.reraise(exn)) {
      | exn => exn
      };
    },
    Lwt.return_unit,
  );
  Lwt.return_unit;
};
```

**>>=**

```ocaml
[@ppx_let_locs.use Lwt.backtrace_bind]
let ( >>= ) = Lwt.bind

(* you type *)
let from = Lwt.return_unit >>= (fun () -> Lwt.return_unit)

(* compiler see *)
let to_ = (>>=_backtrace)
  (fun exn ->
    let module Reraise = struct
      external reraise: exn -> 'a = "%reraise"
    end in
    try Reraise.reraise(exn) with exn -> exn)
  Lwt.return_unit
  (fun () -> Lwt.return_unit)
```

**[@ppx_let_locs.use] on signatures**

```ocaml
(* you type *)
[@ppx_let_locs.use Lwt.backtrace_bind]
val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

(* compiler sees *)
val ( >>=_backtrace ) : (exn -> exn) -> 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
```
