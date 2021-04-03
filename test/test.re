Printexc.record_backtrace(true);

try(
  ([@ppx_locs.ignore] Lwt.bind)(Lwt_unix.opendir("invalid_dir"), _ =>
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

let (let.await) = ((exn, v), f) => Lwt.backtrace_bind(exn, v, f);
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
