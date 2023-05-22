module.exports = grammar({

  name: "wty_2",
  rules: {
    source_file: $ => many_semi_sep($.any_dec),

    /* See Haskell parser combinator grammar (or spec) for explanations of the 
     * purpose of many of these constructs */

    ident: $ => /[a-zA-Z]+/,
    op_ident: $ => /[\+<=>\-/\\\*\.\|&]+/,
    int_lit: $ => /\d+/,

    expr: $ => prec.left(choice(
      $.block,
      $.match,
      $.ident,
      $.parens_expr,
      $.fun_app,
      $.cps_bind,
      $.op_app,
      $.int_lit,
    )),
    block: $ => braces(choice(
      many_semi_sep_trail(choice($.dec, $.expr)),
      $.expr)),
    match: $ => seq('match', $.parens_expr, braces(repeat($.match_case))),
    match_case: $ => seq(choice(
      seq('case', $.parens_pat),
      seq('tycase', $.parens_expr)),
      '=>', $.expr),
    // TODO: Not sure why I had to make this left-associative to remove
    // conflicts. Maybe should be right-associative?
    fun_app: $ => prec.left(8,
      seq($.expr, optional('?'), $.parens_expr, optional($.block))),
    // TODO: More weird associativity stuff
    op_app: $ => prec.left(5, seq($.expr, $.op, some_sep($.expr, $.op))),
    cps_bind: $ => prec(7, seq('!', $.expr)),

    op: $ => choice($.op_ident, $.indexed_op),
    indexed_op: $ => prec(7, seq($.op_ident, $.brackets_expr)),

    parens_expr: $ => seq(
      optional($.brackets_expr),
      parens(optional($.inner_expr))),
    brackets_expr: $ => brackets($.inner_expr),
    inner_expr: $ => choice(some_comma_sep($.bind), some_comma_sep($.expr)),
    starts_with_parens_expr: $ => seq($.parens_expr, optional(seq($.op, $.expr))),

    bind: $ => seq(
      $.ident,
      optional($.starts_with_parens_expr),
      $.bind_op,
      $.expr),
    bind_op: $ => choice(':', ':=>', ':~'),
    relaxed_bind: $ => seq(
      $.ident,
      optional($.parens_expr),
      optional(seq($.bind_op, $.expr))),

    pat: $ => seq($.ident, optional($.parens_pat)),
    parens_pat: $ => parens(optional($.inner_pat)),
    inner_pat: $ => choice($.pat, some_comma_sep($.pat_elem)),
    pat_elem: $ => seq('.', $.ident, optional(seq(' = ', $.ident))),

    dec: $ => seq(choice(
      seq(choice($.bind, $.infallible_match), '='),
      seq($.ident, ':=')),
      $.expr),
    infallible_match: $ => seq('case', $.parens_pat),
    dec_block: $ => braces(many_semi_sep($.dec)),

    dat_dec: $ => seq('data', $.relaxed_bind),
    ty_dec: $ => seq('type', $.relaxed_bind,
      choice(
        $.closed_ty_dec_RHS,
        optional($.open_ty_dec_RHS))),
    open_ty_dec_RHS: $ => seq($.dec_block, '=>', $.expr),
    closed_ty_dec_RHS: $ => seq('=', $.expr),
    dat_ty_dec: $ => seq('datatype', $.ident, '=',
      many_bar_sep($.relaxed_bind)),

    inst_dec: $ => seq('inst', $.expr, 'for', $.expr, optional($.dec_block)),
    pat_dec: $ => seq('pattern', $.pat, '=', $.pat),

    any_dec: $ => choice($.dec, $.dat_dec, $.ty_dec, $.inst_dec, $.pat_dec),
  }
});

function between(l, r, p) {
  return seq(l, p, r)
}


function braces(p) {
  return between('{', '}', p)
}

function parens(p) {
  return between('(', ')', p)
}

function brackets(p) {
  return between('[', ']', p)
}

function many_sep(p, s) {
  return optional(some_sep(p, s))
}

function many_sep_trail(p, s) {
  return seq(many_sep(p, s), optional(s))
}

function some_sep(p, s) {
  return seq(p, repeat(seq(s, p)))
}

function some_sep_trail(p, s) {
  return seq(some_sep(p, s), optional(s))
}

function many_comma_sep(p) {
  return many_sep_trail(p, ',')
}

function some_comma_sep(p) {
  return some_sep_trail(p, ',')
}

function many_semi_sep(p) {
  return many_sep(p, ';')
}

function many_semi_sep_trail(p) {
  return many_sep_trail(p, ';')
}

function some_semi_sep(p) {
  return some_sep(p, ';')
}

function many_bar_sep(p) {
  return many_sep(p, '|')
}


