module Codegen = {
  open Ppxlib.Selected_ast.Ast.Parsetree;
  let make_with_reraise = (~loc, fn) => [%expr
    {
      module Reraise = {
        external reraise: exn => 'a = "%reraise";
      };

      [%e fn](exn =>
        try(Reraise.reraise(exn)) {
        | exn => exn
        }
      );
    }
  ];
  let make_with_reraise = (~loc, fn) =>
    make_with_reraise(
      ~loc,
      {
        ...fn,
        pexp_attributes: [
          {
            attr_name: {
              loc,
              txt: "ppx_locs.ignore",
            },
            attr_payload: PStr([]),
            attr_loc: loc,
          },
          ...fn.pexp_attributes,
        ],
      },
    );
};

module Typer = {
  open Ppx_locs_typer;

  let (let.some) = Option.bind;
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
    let.some () =
      switch (desc.val_type.desc) {
      | Tarrow(
          Nolabel,
          {
            desc:
              Tarrow(
                Nolabel,
                {desc: Tconstr(exn_1, [], _), _},
                {desc: Tconstr(exn_2, [], _), _},
                _,
              ),
            _,
          },
          _,
          _,
        )
          when Path.name(exn_1) == "exn" && Path.name(exn_2) == "exn" =>
        Some()
      | _ => None
      };
    let sfunct =
      Codegen.make_with_reraise(
        ~loc=lid.loc,
        {...sfunct, pexp_desc: Pexp_ident(lid)},
      );
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
  let str = Untypeast.untype_structure(tstr);
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
