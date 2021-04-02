Printexc.record_backtrace(true);

try(
  Lwt.bind(Lwt_unix.opendir("invalid_dir"), _ => Lwt.return_unit)
  |> Lwt_main.run
) {
| _ =>
  print_endline("backtraced: ");
  Printexc.print_backtrace(stdout);
  print_newline();
};

try(
  ([@ppx_locs.ignore] Lwt.bind)(Lwt_unix.opendir("invalid_dir"), _ =>
    Lwt.return_unit
  )
  |> Lwt_main.run
) {
| _ =>
  print_endline("ignore: ");
  Printexc.print_backtrace(stdout);
  print_newline();
};
