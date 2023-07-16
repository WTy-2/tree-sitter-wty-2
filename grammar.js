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
  name: "WTy2",

  // Ideally we wouldn't need these, but removing all ambiguities is
  // challenging
  // See how many conflicts are required for Haskell's grammar:
  // https://github.com/tree-sitter/tree-sitter-haskell/blob/master/grammar.js
  conflicts: ($) => [
    // Ambiguities between pattern matches and expressions in braces
    [$.any_ident, $.disambig_pat],
    [$.no_brackets_parens_expr, $.parens_pat],
    [$.any_ident, $.pat_elem],
    // Ambiguity between assignments and expressions in braces
    [$.any_ident, $.var_dec],
  ],

  inline: ($) => [$.match_ident, $.def_ident],

  // Currently allowing Haskell and C-style comments because I haven't made
  // my mind up on which I like more...
  // TODO: Support block comments
  extras: ($) => [choice(/\s+/, /\/\/[^\n\r]*[\n\r]/, /--[^\n\r]*[\n\r]/)],

  rules: {
    source_file: ($) => repeat(seq($.any_dec, ";")),

    low_ident: ($) => /[a-z][a-zA-Z0-9]*/,
    up_ident: ($) => /[A-Z][a-zA-Z0-9]*/,
    any_ident: ($) => choice($.low_ident, $.up_ident),
    match_ident: ($) => choice(seq("match", $.any_ident), $.up_ident),
    def_ident: ($) => choice(seq("def", $.any_ident), $.low_ident),
    op_ident: ($) => /[$\+<=>\-/\\\*\.\|&~]+/,
    int_lit: ($) => /\d+/,
    str_lit: ($) => /"[^"\n]*"/,

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
            $.str_lit,
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
          seq("\\", $.inner_pat, "->", $.blockContent),
          // Choice: Should empty block represent empty case - i.e: value of
          // closed type `Void`
          // Or, should it mean an empty block of statements returning unit
          // I think latter is neater, but then need to decide on syntax for
          // former, maybe `{ | }`
          many_comma_sep(seq("|", $.inner_pat, "->", $.blockContent))
        )
      ),

    // Left associative ensures `f(1)(2)` is parsed as `(f(1))(2)`, not
    // `f((1)(2))`
    fun_app: ($) =>
      prec.left(
        8,
        choice(
          seq($.expr, optional("?"), $.parens_expr),
          seq($.expr, optional(seq(optional("?"), $.parens_expr)), $.block)
        )
      ),

    // Operator expressions must be re-associated later (cannot do this during
    // parsing as WTy2 allows user-defined operators)
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

    bind: ($) => seq($.pat, $.bind_op, $.expr),
    bind_op: ($) => choice(":", "<:", "<=:", "~:"),

    // Like `pat` but lowercase constructors must be prefixed by the "match"
    // keyword to disambiguate
    // TODO: Is this _really_ the neatest way to organise pattern match rules?
    disambig_pat: ($) =>
      choice(
        "_",
        $.int_lit,
        $.str_lit,
        $.parens_pat,
        seq($.match_ident, optional($.parens_pat))
      ),

    pat: ($) =>
      choice(
        "_",
        $.int_lit,
        $.str_lit,
        $.parens_pat,
        seq($.any_ident, optional($.parens_pat))
      ),
    parens_pat: ($) => parens(optional($.inner_pat)),
    inner_pat: ($) => some_comma_sep($.pat_elem),
    pat_elem: ($) =>
      seq(choice($.def_ident, $.disambig_pat), optional(seq("=", $.any_ident))),

    // No idea why precedence of 1 vs 0 is significant here, but it is :/
    var_dec: ($) =>
      seq(
        choice(
          seq($.bind, "="),
          seq(choice($.any_ident, $.irrefutable_match), ":="),
          // Function definition
          seq(
            $.def_ident,
            $.starts_with_parens_expr,
            // TODO: Should other binding operators be allowed here?
            choice(seq(":", $.expr, "="), ":=")
          )
        ),
        $.expr
      ),
    irrefutable_match: ($) => $.disambig_pat,
    var_dec_block: ($) => braces(repeat(seq($.var_dec, ";"))),

    dat_dec_bind: ($) => seq($.any_ident, optional($.starts_with_parens_expr)),
    dat_dec: ($) => seq("data", $.dat_dec_bind),
    ty_dec: ($) =>
      seq(
        "type",
        $.any_ident,
        choice($.closed_ty_dec_RHS, optional($.open_ty_dec_RHS))
      ),
    // TODO: Allow just type signatures of variables to be declared in the body
    // (i.e: do not require default impl)
    open_ty_dec_RHS: ($) => seq($.var_dec_block, optional(seq("<:", $.expr))),
    closed_ty_dec_RHS: ($) => seq("=", $.expr),

    dat_ty_dec_bind: ($) =>
      seq(
        $.any_ident,
        optional($.starts_with_parens_expr),
        // GADT-style-syntax, expression must reduce down to a type constructed
        // with the type constructor defined at the start
        optional(seq(":", $.expr))
      ),
    dat_ty_dec: ($) =>
      seq(
        "datatype",
        $.any_ident,
        optional($.starts_with_parens_expr),
        "=",
        many_bar_sep($.dat_ty_dec_bind)
      ),

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
