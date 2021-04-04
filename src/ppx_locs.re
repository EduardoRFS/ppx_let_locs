let (let.some) = Option.bind;
let (let.none) = (v, f) =>
  switch (v) {
  | Some(v) => Some(v)
  | None => f()
  };
let (let.default) = (v, f) =>
  switch (f()) {
  | Some(v) => v
  | None => v
  };
module Codegen = {
  open Parsetree;
  open Compat;

  let ppx_let_locs_ignore = loc => {
    attr_name: {
      loc,
      txt: "ppx_let_locs.ignore",
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
  let ocaml_warning = (loc, str) => {
    attr_name: {
      loc,
      txt: "ocaml.warning",
    },
    attr_payload:
      PStr([
        pstr_eval(~loc, pexp_constant(~loc, pconst_string(~loc, str)), []),
      ]),
    attr_loc: loc,
  };
  let append_warning_description = (warning, descr) => {
    ...descr,
    pval_attributes: [
      ocaml_warning(descr.pval_loc, warning),
      ...descr.pval_attributes,
    ],
  };
  let append_warning_binding = (warning, binding) => {
    ...binding,
    pvb_attributes: [
      ocaml_warning(binding.pvb_loc, warning),
      ...binding.pvb_attributes,
    ],
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
      append_attributes([ppx_let_locs_ignore(loc), merlin_focus(loc)], fn);
    Fun.id([%expr [%e fn]([%e make_reraise_exn(loc)])]);
  };
  let make_let_with_reraise = (new_op, let_) => {
    let {pbop_exp, _} = let_;
    {
      ...let_,
      pbop_op: new_op,
      pbop_exp:
        pexp_tuple(
          ~loc=pbop_exp.pexp_loc,
          [
            make_reraise_exn(pbop_exp.pexp_loc),
            append_attributes([merlin_focus(pbop_exp.pexp_loc)], pbop_exp),
          ],
        ),
    };
  };

  let make_additional_binding = (is_letop, name, body) => {
    let loc = body.pexp_loc;
    let additional_binding = {
      pvb_pat: ppat_var(~loc, name),
      pvb_expr:
        is_letop ? [%expr ((exn, v), f) => [%e body](exn, v, f)] : body,
      pvb_attributes: [],
      pvb_loc: loc,
    };
    additional_binding;
  };

  let make_additional_value_typ = (~loc, is_letop, name, left, right) => {
    let type_ =
      is_letop
        ? [%type: ((exn => exn, [%t left])) => [%t right]]
        : [%type: (exn => exn, [%t left]) => [%t right]];
    {
      psig_loc: loc,
      psig_desc:
        Psig_value(
          value_description(~loc, ~name={txt: name, loc}, ~type_, ~prim=[]),
        ),
    };
  };
};

module Typer = {
  open Ppx_let_locs_typer;

  let exn_callback_typ =
    Ctype.newty(Tarrow(Nolabel, Predef.type_exn, Predef.type_exn, Cok));
  let backtraced_signature_typ =
    Ctype.newty(Tarrow(Nolabel, exn_callback_typ, Ctype.newvar(), Cok));
  let is_backtraced_signature = (env, expr) =>
    Ctype.matches(env, expr, backtraced_signature_typ);

  let backtraced_letop_typ =
    Ctype.newty(
      Tarrow(
        Nolabel,
        Ctype.newty(Ttuple([exn_callback_typ, Ctype.newvar()])),
        Ctype.newvar(),
        Cok,
      ),
    );
  let is_backtraced_letop = (env, expr) =>
    Ctype.matches(env, expr, backtraced_letop_typ);

  let prepend_backtrace = str => "backtrace_" ++ str;
  let append_backtrace = str => str ++ "_backtrace";

  let is_letop = {
    let chars = "$&*+-/<=>@^|.";
    str =>
      String.length(str) >= 4
      && String.sub(str, 0, 3) == "let"
      && String.index_opt(chars, str.[3]) != None;
  };
  let transform_lid = (f, lid: Location.loc(Longident.t)) => {
    ...lid,
    txt:
      switch (lid.txt) {
      | Lapply(_) as lid => lid
      | Lident(n) => Lident(f(n))
      | Ldot(path, n) => Ldot(path, f(n))
      },
  };

  // this transforms Lwt.bind into Lwt.backtrace_bind
  let hacked_pexp_apply = (env: Env.t, sfunct: Parsetree.expression) => {
    let.some lid =
      switch (sfunct.pexp_desc, sfunct.pexp_attributes) {
      // TODO: handle non empty attributes
      | (Parsetree.Pexp_ident(lid), []) => Some(lid)
      | _ => None
      };
    let lookup_opt_transform = f =>
      switch (
        {
          let lid = transform_lid(f, lid);
          (lid, Env.lookup_value(~loc=lid.loc, lid.txt, env) |> snd);
        }
      ) {
      | value => Some(value)
      | exception _ => None
      };
    let.some (lid, desc) = {
      let.none () = lookup_opt_transform(append_backtrace);
      lookup_opt_transform(prepend_backtrace);
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

  // this will handle letop with signature ((exn => exn, 'a), 'a => 'b) => 'b
  let hacked_pexp_letop = (env: Env.t, slet: Parsetree.binding_op) => {
    // TODO: is it okay to generate Pexp_apply?
    let backtraced_op = {
      ...slet.pbop_op,
      txt: append_backtrace(slet.pbop_op.txt),
    };
    let lid =
      Location.mkloc(Longident.Lident(backtraced_op.txt), backtraced_op.loc);
    let.some (_path, desc) =
      switch (Env.lookup_value(~loc=lid.loc, lid.txt, env)) {
      | value => Some(value)
      | exception _exn => None
      };
    let.some () = is_backtraced_letop(env, desc.val_type) ? Some() : None;
    Some(Codegen.make_let_with_reraise(backtraced_op, slet));
  };
  Typecore.hacked_pexp_letop := hacked_pexp_letop;

  // type recovery and ppx_let_locs.use
  let hacked_type_expect = (f, env, sexp, ty_expected) => {
    open Parsetree;
    open Typedtree;
    open Types;
    open Ctype;
    open Typecore;
    let saved = save_levels();
    try(f(env, sexp, ty_expected)) {
    | _ =>
      set_levels(saved);
      let loc = sexp.pexp_loc;
      {
        exp_desc:
          Texp_ident(
            Path.Pident(Ident.create_local("*type-error*")),
            Location.mkloc(Longident.Lident("*type-error*"), loc),
            switch () {
            | [@if ocaml_version >= (4, 11, 0)] () => {
                Types.val_type: ty_expected.ty,
                val_kind: Val_reg,
                val_loc: loc,
                val_attributes: [],
                val_uid: Uid.internal_not_actually_unique,
              }
            | [@if ocaml_version < (4, 11, 0)] () => {
                Types.val_type: ty_expected.ty,
                val_kind: Val_reg,
                val_loc: loc,
                val_attributes: [],
              }
            },
          ),
        exp_loc: loc,
        exp_extra: [],
        exp_type: ty_expected.ty,
        exp_env: env,
        exp_attributes: [
          {
            attr_name: {
              loc,
              txt: "untype.data",
            },
            attr_payload:
              PStr([
                {
                  pstr_loc: loc,
                  pstr_desc: [@implicit_arity] Pstr_eval(sexp, []),
                },
              ]),
            attr_loc: loc,
          },
        ],
      };
    };
  };
  Typecore.hacked_type_expect := hacked_type_expect;

  // pexp_let_locs.use
  let hacked_value_binding = (spat_list: list(Parsetree.value_binding)) =>
    spat_list
    |> List.concat_map(binding => {
         let.default () = [binding];
         let.some body =
           binding.Parsetree.pvb_attributes
           |> List.find_map(
                fun
                | {
                    Parsetree.attr_name: {txt: "ppx_let_locs.use", _},
                    attr_payload:
                      PStr([{pstr_desc: Pstr_eval(sexp, []), _}]),
                    _,
                  } =>
                  Some(sexp)
                | _ => None,
              );
         let.some name =
           switch (binding.pvb_pat.ppat_desc) {
           | Ppat_var(name) =>
             Some({...name, txt: append_backtrace(name.txt)})
           | _ => None
           };
         let is_letop = is_letop(name.txt);
         Some([
           Codegen.append_warning_binding("-32", binding),
           Codegen.make_additional_binding(is_letop, name, body),
         ]);
       });
  Typecore.hacked_value_binding := hacked_value_binding;

  // [@ppx_let_locs.use] on signature
  let additional_signature_item =
    fun
    | Parsetree.{
        psig_desc:
          Psig_value(
            {
              pval_attributes,
              pval_name,
              pval_type: {ptyp_desc: Ptyp_arrow(Nolabel, left, right), _},
              _,
            } as description,
          ),
        psig_loc,
      }
        when
          pval_attributes
          |> List.exists(
               fun
               | {
                   Parsetree.attr_name: {txt: "ppx_let_locs.use", _},
                   attr_payload: PStr([]),
                   _,
                 } =>
                 true
               | _ => false,
             ) => {
        // TODO: this clearly needs better locs
        Some((
          {
            Parsetree.psig_loc,
            psig_desc:
              Psig_value(
                Codegen.append_warning_description("-32", description),
              ),
          },
          Codegen.make_additional_value_typ(
            ~loc=psig_loc,
            is_letop(pval_name.txt),
            append_backtrace(pval_name.txt),
            left,
            right,
          ),
        ));
      }
    | _ => None;
  let hacked_transl_sig = (item, srem) =>
    switch (additional_signature_item(item)) {
    | Some((item, v)) => (item, [v, ...srem])
    | None => (item, srem)
    };
  Typemod.hacked_transl_sig := hacked_transl_sig;

  let transform_signature = signature => {
    let mapper = {
      ...Ast_mapper.default_mapper,
      signature: (super, signature) =>
        signature
        |> List.concat_map(sigi => {
             let sigi = Ast_mapper.default_mapper.signature_item(super, sigi);
             switch (additional_signature_item(sigi)) {
             | Some((sigi, v)) => [sigi, v]
             | None => [sigi]
             };
           }),
    };
    mapper.signature(mapper, signature);
  };

  let transform_signature = str =>
    Ppxlib.Selected_ast.To_ocaml.copy_signature(str)
    |> transform_signature
    |> Ppxlib.Selected_ast.Of_ocaml.copy_signature;
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
  open Ppx_let_locs_typer;
  let env = Lazy.force(env);
  let loc = {
    open Location;
    let {loc_start, loc_ghost: ghost_1, _} =
      List.nth_opt(str, 0)
      |> Option.map(v => v.Parsetree.pstr_loc)
      |> Option.value(~default=none);
    let {loc_end, loc_ghost: ghost_2, _} =
      List.rev(str)
      |> List.nth_opt(_, 0)
      |> Option.map(v => v.Parsetree.pstr_loc)
      |> Option.value(~default=none);
    {loc_start, loc_end, loc_ghost: ghost_1 && ghost_2};
  };
  let (tstr, _, _, _) =
    Compat.call_type_structure(~loc, env, str, Typemod.type_structure);
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

let transform = str =>
  Ppxlib.Selected_ast.To_ocaml.copy_structure(str)
  |> transform
  |> Ppxlib.Selected_ast.Of_ocaml.copy_structure;

Ppxlib.Driver.register_transformation_using_ocaml_current_ast;
let () =
  Ppxlib.Driver.(
    register_transformation(
      ~intf=Typer.transform_signature,
      ~impl=transform,
      "ppx_let_locs",
    )
  );
