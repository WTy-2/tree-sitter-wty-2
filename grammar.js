const between = (l, r, p) => seq(l, p, r);

const braces = (p) => between("{", "}", p);

const parens = (p) => between("(", ")", p);

const brackets = (p) => between("[", "]", p);

// Note we define "many" versions in terms of "some" - this is important, the
// other way around creates ambiguities!!
// e.g try:
/*
```
const many_trail_with = (p, s) => repeat(seq((p, s));

const some_trail_with = (p, s) => seq(p, s, many_trail_with(p, s));
```
*/
const many_sep = (p, s) => optional(some_sep(p, s));

const many_sep_trail = (p, s) => seq(many_sep(p, s), optional(s));

const many_trail_with = (p, s) => optional(some_trail_with(p, s));

const some_sep = (p, s) => seq(p, repeat(seq(s, p)));

const some_sep_trail = (p, s) => seq(some_sep(p, s), optional(s));

const some_trail_with = (p, s) => seq(p, s, repeat(seq(p, s)));

const many_comma_sep = (p) => many_sep_trail(p, ",");

const some_comma_sep = (p) => some_sep_trail(p, ",");

const many_semi_sep = (p) => many_sep(p, ";");

const many_semi_sep_trail = (p) => many_sep_trail(p, ";");

const many_trail_with_semi = (p) => many_trail_with(p, ";");

const some_semi_sep_trail = (p) => some_sep_trail(p, ";");

const some_trail_with_semi = (p) => some_trail_with(p, ";");

const some_semi_sep = (p) => some_sep(p, ";");

const many_bar_sep = (p) => many_sep(p, "|");

module.exports = grammar({
  name: "wty_2",
  rules: {
    source_file: ($) => repeat(seq($.any_dec, ";")),

    /* See Haskell parser combinator grammar (or spec) for explanations of the
     * purpose of many of these constructs */

    low_ident: ($) => /[a-z][a-zA-Z0-9]*/,
    up_ident: ($) => /[A-Z][a-zA-Z0-9]*/,
    // Associativity correct here?
    any_ident: ($) => prec.left(choice($.low_ident, $.up_ident)),
    op_ident: ($) => /[\+<=>\-/\\\*\.\|&]+/,
    int_lit: ($) => /\d+/,

    expr: ($) =>
      prec(
        3,
        seq(
          choice(
            $.erased_pair,
            $.block,
            $.any_ident,
            $.no_brackets_parens_expr,
            $.fun_app,
            $.cps_bind,
            $.op_app,
            $.int_lit,
            $.promoted
          )
        )
      ),
    blockContent: ($) =>
      choice(
        some_trail_with_semi(choice($.var_dec, $.cps_bind)),
        seq(many_trail_with_semi(choice($.var_dec, $.cps_bind)), $.expr)
      ),
    block: ($) =>
      braces(
        choice(
          $.blockContent,
          seq("\\", $.pat, "->", $.blockContent),
          // Empty = empty case (i.e: value of closed type `Void`)
          many_comma_sep(seq("|", $.pat, "->", $.blockContent))
        )
      ),
    // TODO: Not sure why I had to make this left-associative to remove
    // conflicts. Maybe should be right-associative?
    // TODO: Allow for blocks without parens: `do { ... }` vs `do() { ... }`
    fun_app: ($) =>
      prec.left(
        8,
        choice(
          seq($.expr, optional("?"), $.parens_expr),
          seq($.expr, optional(seq(optional("?"), $.parens_expr)), $.block)
        )
      ),
    // TODO: More weird associativity stuff
    op_app: ($) => prec.left(5, seq($.expr, $.op, some_sep($.expr, $.op))),
    cps_bind: ($) => prec(7, seq("!", $.expr)),
    promoted: ($) => seq("'", $.expr),

    op: ($) => choice($.op_ident, $.indexed_op),
    indexed_op: ($) => prec(7, seq($.op_ident, $.brackets_expr)),

    no_brackets_parens_expr: ($) => parens(optional($.inner_expr)),
    // Higher precedence than `expr` because we want to ensure the info that
    // the expression can appear as a `parens_expr` to bubble out
    parens_expr: ($) =>
      prec(4, seq(optional($.brackets_expr), $.no_brackets_parens_expr)),
    erased_pair: ($) => seq($.brackets_expr, $.expr),
    brackets_expr: ($) => brackets($.inner_expr),
    inner_expr: ($) => choice(some_comma_sep($.bind), some_comma_sep($.expr)),
    starts_with_parens_expr: ($) =>
      seq($.parens_expr, optional(seq($.op, $.expr))),

    bind: ($) => seq($.any_ident, $.bind_op, $.expr),

    bind_op: ($) => choice(":", "<:", "<=:", "~:"),
    relaxed_bind: ($) => seq($.any_ident, optional(seq($.bind_op, $.expr))),
    // Like `pat` but lowercase constructors must be prefixed by the "match"
    // keyword to disambiguate
    disambig_pat: ($) =>
      seq(
        choice($.up_ident, seq("match", $.any_ident)),
        optional($.parens_pat)
      ),
    // TODO: Operator-constructor patterns
    pat: ($) => seq($.any_ident, optional($.parens_pat)),
    parens_pat: ($) => parens(optional($.inner_pat)),
    inner_pat: ($) => choice($.pat, some_comma_sep($.pat_elem)),
    // TODO: Is the prefix "." REALLY necessary here?
    // Does this support nested matching?
    pat_elem: ($) => seq(".", $.any_ident, optional(seq("=", $.any_ident))),

    // Same precedence as function application to avoid that being prioritised
    var_dec: ($) =>
      prec(
        1,
        seq(
          choice(
            seq(choice($.bind, $.irrefutable_match), "="),
            seq($.any_ident, ":="),
            // Function definition
            seq(
              choice($.low_ident, seq("fn", $.any_ident)),
              $.starts_with_parens_expr,
              choice(seq(":", $.expr, "="), ":=")
            )
          ),
          $.expr
        )
      ),
    irrefutable_match: ($) => $.disambig_pat,
    var_dec_block: ($) => braces(repeat(seq($.var_dec, ";"))),

    dat_dec: ($) => seq("data", $.relaxed_bind),
    ty_dec: ($) =>
      seq(
        "type",
        $.any_ident,
        choice($.closed_ty_dec_RHS, optional($.open_ty_dec_RHS))
      ),
    open_ty_dec_RHS: ($) => seq($.var_dec_block, optional(seq("<:", $.expr))),
    closed_ty_dec_RHS: ($) => seq("=", $.expr),
    dat_ty_dec: ($) =>
      seq("datatype", $.any_ident, "=", many_bar_sep($.relaxed_bind)),

    inst_dec: ($) =>
      seq(
        "instance",
        $.expr,
        "for",
        $.expr,
        optional(seq("where", $.var_dec_block))
      ),
    pat_dec: ($) => seq("pattern", $.pat, "=", $.pat),

    any_dec: ($) =>
      choice($.var_dec, $.dat_dec, $.ty_dec, $.inst_dec, $.pat_dec),
  },
});
