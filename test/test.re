Printexc.record_backtrace(true);

let fail = () => raise(Invalid_argument("just checking stack"));

try(Lwt.bind(Lwt_unix.sleep(0.01), fail) |> Lwt_main.run) {
| _ =>
  print_endline("backtraced: ");
  Printexc.print_backtrace(stdout);
  print_newline();
};

try(
  ([@ppx_locs.ignore] Lwt.bind)(Lwt_unix.sleep(0.01), fail) |> Lwt_main.run
) {
| _ =>
  print_endline("ignore: ");
  Printexc.print_backtrace(stdout);
  print_newline();
};
