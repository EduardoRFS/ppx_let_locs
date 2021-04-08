Printexc.record_backtrace(true);

try(
  ([@ppx_let_locs.ignore] Lwt.bind)(Lwt_unix.opendir("invalid_dir"), _ =>
    Lwt.return_unit
  )
  |> Lwt_main.run
) {
| _ =>
  print_endline("Lwt.bind ignore: ");
  Printexc.print_backtrace(stdout);
  print_newline();
};

try(
  Lwt.bind(Lwt_unix.opendir("invalid_dir"), _ => Lwt.return_unit)
  |> Lwt_main.run
) {
| _ =>
  print_endline("Lwt.bind backtraced: ");
  Printexc.print_backtrace(stdout);
  print_newline();
};

let (let.await) = Lwt.bind;
try(
  {
    let.await _ = Lwt_unix.opendir("invalid_dir");
    Lwt.return_unit;
  }
  |> Lwt_main.run
) {
| _ =>
  print_endline("letop ignore: ");
  Printexc.print_backtrace(stdout);
  print_newline();
};

[@ppx_let_locs.use Lwt.backtrace_bind]
let (let.await) = Lwt.bind;

open (
       {
         [@ppx_let_locs.use Lwt.backtrace_bind]
         let (let.await) = Lwt.bind;
       }: {
         [@ppx_let_locs.use]
         let (let.await): (Lwt.t('a), 'a => Lwt.t('b)) => Lwt.t('b);
       }
     );
try(
  {
    let.await _ = Lwt_unix.opendir("invalid_dir");
    Lwt.return_unit;
  }
  |> Lwt_main.run
) {
| _ =>
  print_endline("letop backtraced: ");
  Printexc.print_backtrace(stdout);
  print_newline();
};

let (>>=) = Lwt.bind;
try(
  {
    Lwt_unix.opendir("invalid_dir") >>= (_ => Lwt.return_unit);
  }
  |> Lwt_main.run
) {
| _ =>
  print_endline(">>= ignore: ");
  Printexc.print_backtrace(stdout);
  print_newline();
};

[@ppx_let_locs.use Lwt.backtrace_bind]
let (>>=) = Lwt.bind;
try(
  {
    Lwt_unix.opendir("invalid_dir") >>= (_ => Lwt.return_unit);
  }
  |> Lwt_main.run
) {
| _ =>
  print_endline(">>= backtraced: ");
  Printexc.print_backtrace(stdout);
  print_newline();
};

// TODO: this is a bug on OCaml Untypeast
module X = {
  type t = {name: string};
};

let f = (X.{name}) => name;
