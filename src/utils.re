open Ppxlib;
let pstr_eval = (~loc, x0, x1) => {
  pstr_loc: loc,
  pstr_desc: Pstr_eval(x0, x1),
};
let pexp_constant = (~loc, x0) => {
  pexp_loc_stack: [],
  pexp_attributes: [],
  pexp_loc: loc,
  pexp_desc: Pexp_constant(x0),
};

let pconst_string = (~loc: Location.t, str) => Pconst_string(str, loc, None);

let pexp_tuple = (~loc, l) => Ast_helper.Exp.tuple(~loc, l);

let ppat_var = (~loc, x0) => {
  ppat_loc_stack: [],
  ppat_attributes: [],
  ppat_loc: loc,
  ppat_desc: Ppat_var(x0),
};

let value_description = (~loc, ~name, ~type_, ~prim) => {
  pval_name: name,
  pval_type: type_,
  pval_prim: prim,
  pval_attributes: [],
  pval_loc: loc,
};
