let (let.some) = Option.bind;
module Codegen = {
  open Ppxlib.Selected_ast.Ast.Parsetree;
  open Ppxlib.Ast_builder.Default;
  let ppx_locs_ignore = loc => {
    attr_name: {
      loc,
      txt: "ppx_locs.ignore",
    },
    attr_payload: PStr([]),
    attr_loc: loc,
  };
  let merlin_focus = loc => {
    attr_name: {
      loc,
      txt: "merlin.focus",
    },
    attr_payload: PStr([]),
    attr_loc: loc,
  };
  let append_attributes = (attrs, exp) => {
    ...exp,
    pexp_attributes: exp.pexp_attributes @ attrs,
  };
  let make_reraise_exn = loc => [%expr
    exn => {
      module Reraise = {
        external reraise: exn => 'a = "%reraise";
      };
      try(Reraise.reraise(exn)) {
      | exn => exn
      };
    }
  ];
  let make_apply_with_reraise = fn => {
    let loc = fn.pexp_loc;
    let fn =
      append_attributes([ppx_locs_ignore(loc), merlin_focus(loc)], fn);
    Fun.id([%expr [%e fn]([%e make_reraise_exn(loc)])]);
  };
};

module Typer = {
  open Ppx_locs_typer;

  let exn_callback_typ =
    Ctype.newty(Tarrow(Nolabel, Predef.type_exn, Predef.type_exn, Cok));
  let backtraced_signature_typ =
    Ctype.newty(Tarrow(Nolabel, exn_callback_typ, Ctype.newvar(), Cok));
  let is_backtraced_signature = (env, expr) =>
    Ctype.matches(env, expr, backtraced_signature_typ);

  let hacked_pexp_apply = (env: Env.t, sfunct: Parsetree.expression) => {
    let.some lid =
      switch (sfunct.pexp_desc, sfunct.pexp_attributes) {
      // TODO: handle non empty attributes
      | (Parsetree.Pexp_ident(lid), []) => Some(lid)
      | _ => None
      };
    let prepend_backtrace = str => "backtrace_" ++ str;
    let lid = {
      ...lid,
      txt:
        switch (lid.txt) {
        | Lapply(_) as lid => lid
        | Lident(n) => Lident(prepend_backtrace(n))
        | Ldot(path, n) => Ldot(path, prepend_backtrace(n))
        },
    };
    let.some (_path, desc) =
      switch (Env.lookup_value(~loc=lid.loc, lid.txt, env)) {
      | value => Some(value)
      | exception _ => None
      };
    let.some () = is_backtraced_signature(env, desc.val_type) ? Some() : None;
    let sfunct =
      Codegen.make_apply_with_reraise({
        ...sfunct,
        pexp_desc: Pexp_ident(lid),
      });
    Some(Typecore.type_exp(env, sfunct));
  };
  Typecore.hacked_pexp_apply := hacked_pexp_apply;
};

open Ocaml_common;
let env =
  lazy(
    {
      Compmisc.init_path();
      Compmisc.initial_env();
    }
  );
let transform = str => {
  open Ppx_locs_typer;
  let env = Lazy.force(env);
  let (tstr, _, _, _) = Typemod.type_structure(env, str);
  let mapper = {
    ...Untypeast.default_mapper,
    expr: (super, expr) =>
      switch (expr.exp_attributes) {
      | [
          {
            attr_name: {txt: "untype.data", _},
            attr_payload: PStr([{pstr_desc: Pstr_eval(sexp, []), _}]),
            _,
          },
        ] => sexp
      | _ => Untypeast.default_mapper.expr(super, expr)
      },
  };
  let str = mapper.structure(mapper, tstr);
  // Format.printf("%a\n%!", Pprintast.structure, str);
  str;
};

let () =
  Ppxlib.Driver.(
    register_transformation(
      ~instrument=Instrument.make(transform, ~position=After),
      "ppx_locs",
    )
  );
