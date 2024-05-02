#' @encoding UTF-8
#' @family utils
#' @title Glue \link[=av]{Atomized} `...` Arg Atomic Elements into a Character Scalar
#' @description Flattens `...` to a character vector and glues the resulting elements using `g` as the glue.
#' @param g Character scalar glue to use as delimiter.
#' @param ... Objects to be \link[=av]{atomized}.
#' @return A character scalar.
#' @export
g <- function(g, ...) {base::paste(dlg::av(...), collapse = g)}

#' @encoding UTF-8
#' @family utils
#' @title Thin Wrapper for \code{\link[base]{length}}
#' @return A positive integer scalar.
#' @export
N <- base::length

#' @encoding UTF-8
#' @family utils
#' @title Atomize
#' @description Flattens `...` to an atomic vector of atomic constituent values.
#' @param ... Objects to atomized.
#' @return An atomic vector
#' @export
av <- function(...) {
  x <- base::as.vector(base::unlist(base::list(...), T, F))
  base::attributes(x) <- NULL
  x
}

#' @encoding UTF-8
#' @family utils
#' @title Enhancements of \code{\link[base]{ifelse}}.
#' @description If `x` is scalar `TRUE`, returns `y`. If `x` is anything else, returns `n`.
#' @param x A logical scalar (if not,`x` it is replaced by `FALSE`).
#' @param y,n Any valid R object.
#' @param .d A character scalar delimiter for collapsing objects into scalar character objects. If `.d` is not a character scalar, it is replaced by `" "`.
#' @param .cond A character scalar in `c('all', 'any', 'none')`. If `.cond` is not of an allowed value, it is replaced by `'all'`.
#' @return An arbitrary object (either `y` or `n`).
#' @examples
#' f0(NA, data.frame(letters = letters), 0:26)
#' f0(TRUE, data.frame(letters = letters), 0:26)
#' f0(FALSE, data.frame(letters = letters), 0:26)
#' f0(list(1, "a"), data.frame(letters = letters), 0:26)
#' f0(c(.bad.var.name.), data.frame(letters = letters), 0:26)
#' @export
f0 <- function(x, y, n) {if (base::isTRUE(dlg::failsafe(x))) {dlg::failsafe(y)} else {dlg::failsafe(n)}}

#' @encoding UTF-8
#' @family utils
#' @title `paste0(av(...), collapse = "")`
#' @return A character scalar.
#' @export
g0 <- function(...) {base::paste0(dlg::av(...), collapse = "")}

#' @encoding UTF-8
#' @family utils
#' @title Thin Wrapper for \code{\link[base]{paste0}}
#' @return A positive integer scalar.
#' @export
p0 <- base::paste0

#' @encoding UTF-8
#' @family utils
#' @title Checks Whether Evaluation `x` Produces and Error
#' @param x An object to be evaluated.
#' @return A logical scalar
#' @export
is_err <- function(x) {rlang::is_error(tryCatch(base::identity(x), error = function(e) e, finally = NULL))}

#' @encoding UTF-8
#' @family utils
#' @title Failsafe Evaluation of Whether `x` Is Scalar `TRUE`
#' @param x An object to be evaluated for scalar `TRUE`-ness.
#' @return A logical scalar
#' @export
fs_t <- function(x) {if (dlg::is_err(x)) {FALSE} else {base::isTRUE(x)}}

#' @encoding UTF-8
#' @family environments
#' @family meta
#' @title Function names and counts in the call stack
#' @description This family of functions addresses the function call stack *from the perspective of a function that calls one of the functions in this family*. Consistent with that perspective, the basic terminology this family of functions uses is the following:
#' \tabular{ll}{  **Terminology**   \tab **Function identity** \cr
#'                "command line"    \tab The command line      \cr
#'                "caller"          \tab Parent function       \cr
#'                "callers"         \tab All parent functions    }
#' @param n A \link[cmp_psw_vec]{complete positive whole-number scalar} giving the number of generations back in the function call stack to go.
#' @param .err `TRUE` or `FALSE` indicating whether to throw an error if one is encountered.
#' @param .scl `TRUE` or `FALSE` indicating whether to collapse package and function into a character scalar rather than as a two element list with one element for packages and another for functions.
#' @param .vec `TRUE` or `FALSE` indicating whether to represent both package and function in a character vector rather than as a two element list with one element for packages and another for functions.
#' @param ... An arbitrary number of \link[=cmp_psw_vec]{complete positive whole-number vecs} giving the number(s) of generations back in the function call stack to go.
#' @return **A `list(pkg = <character vector>, fun = <character vector>)`** \cr\cr  When `vec = FALSE` or `scl = FALSE`.
#' \cr\cr  **A character vector** (when `vec = TRUE`)                       \cr\cr `callers`
#' \cr\cr  **A character scalar** (when `scl = TRUE`)                       \cr\cr `caller`
#' @examples
#' egD <- function() {list(caller    = caller()                 ,
#'                         callers   = callers()                )}
#' egC <- function() {list(caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         egD       = egD()                    )}
#' egB <- function() {list(caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         egC       = egC()                    )}
#' egA <- function() {list(caller    = caller()                 ,
#'                         callers   = callers()                ,
#'                         egB       = egB()                    )}
#' egA()
#' av(egA())
#' @export
callers <- function(.vec = TRUE) {
  if (!dlg::fs_t(.vec)) {.vec <- F}
  Stack <- base::c(base::rev(base::as.character(base::sys.calls())), "..r..::..command.line..()")
  if (base::length(Stack) == 2) {Stack <- base::c(Stack, "..r..::..command.line..()")}
  dlg::fmt_stack(Stack[3:base::length(Stack)])
}

#' @rdname callers
#' @export
caller <- function(.scl = TRUE) {
  if (!dlg::fs_t(.scl)) {.scl <- F}
  Stack <- base::sys.calls()
  Stack <- base::c(base::rev(base::as.character(Stack)), "..r..::..command.line..()")
  if (base::length(Stack) == 2) {Stack <- base::c(Stack, "..r..::..command.line..()")}
  dlg::fmt_stack(Stack[3])
}

#' @encoding UTF-8
#' @family extensions
#' @family logicals
#' @family errs
#' @title Failsafe Evaluation of an Object
#' @description Evaluate objects, test if an object has certain properties, and conduct binary logical operations safely. these functions never stop execution; they always produce a valid result, even if that result is an error object.
#' @param x Any object/expression to be evaluated, whether or not doing so produces an error.
#' @param .def A character scalar default error message if forcing evaluation produces an error. If not a character scalar, it is replaced with the default.
#' @return Either `x` or the object `.def` with the attribute `'stack'` composed of the function call stack as a character vector.
#' @examples
#' failsafe(non.existent.variable)
#' failsafe(pi)
#' @export
failsafe <- function(x, .def = "dlg.failsafe.err") {
  .def <- tryCatch(base::identity(.def), error = function(e) e, finally = NULL)
  if (rlang::is_error(.def)) {.def <- "dlg.failsafe.err"}
  x <- tryCatch(base::identity(x), error = function(e) e, finally = NULL)
  if (rlang::is_error(x)) {
    base::attr(.def, "stack") <- dlg::callers()
    .def
  } else {x}
}

#' @encoding UTF-8
#' @family utils
#' @title Conver the Call Stack to a Character Vector of Function Names
#' @param stack A character vector call stack
#' @return A character vector
#' @export
fmt_stack <- function(stack) {
  fun <- function(x) {
    def <- "..unknown.."
    x <- dlg::ssplit(x, d = "(")[1]
    x <- dlg::ssplit(x, d = ":::")
    x <- dlg::ssplit(x, d = "::")
    x <- base::trimws(x, which = "both")
    x <- x[x != ""]
    dlg::f0(dlg::N(x) == 0, def, x[1])
  }
  default <- "..??.."
  stack <- base::sapply(stack, fun)
  valid <- stack != default
  stack <- stack[valid]
  stack[stack == "..command.line.."] <- "[command.line]"
  stack
}

#' @encoding UTF-8
#' @family utils
#' @title Collapse and Glue `...` Arguments with Space as Default Delimiter
#' @inheritParams g
#' @inheritDotParams g
#' @return A character scalar.
#' @examples
#' glu(letters)
#' glu(letters, 0:9, d = "")
#' @export
glu <- function(..., d = " ") {dlg::g(d, ...)}

#' @encoding UTF-8
#' @family utils
#' @title Glue and Trim Whitespace from Both Sides
#' @inheritParams g
#' @inheritDotParams g
#' @return A character scalar.
#' @examples
#' .trm(" Just" ,  "a" ,  "little" ,  "thing ")
#' .trm(" Just ", " a ", " little ", " thing ")
#' .trm(" ", letters, LETTERS, 0:9, " ", d = "")
#' @export
trm <- function(..., d = " ") {base::trimws(dlg::glu(..., d = d), which = "both")}

#' @encoding UTF-8
#' @family utils
#' @title Split Strings along Pipes and Trim Whitespace from Both Sides of All Resulting Elements
#' @param x An object of mode character.
#' @param d Character scalar text delimiter.
#' @return A character vector
#' @examples
#' ssplit(" Just | a | little | thing ")
#' ssplit("  Just  |  a  |  little  |  thing  ")
#' @export
ssplit <- function(x, d = "|") {base::trimws(dlg::av(base::strsplit(x, d, fixed = T)), which = "both", whitespace = "[ ]")}

#' @encoding UTF-8
#' @family utils
#' @title Validate Formatting Argument Values
#' @param st Optional style spec.
#' @param bg Optional background color spec.
#' @param fg Optional foreground color spec.
#' @param d Character scalar text delimiter.
#' @param nullst Logical scalar indicating whether a `NULL` style spec is valid.
#' @param nullbg Logical scalar indicating whether a `NULL` background color spec is valid.
#' @param nullfg Logical scalar indicating whether a `NULL` foreground color spec is valid.
#' @examples
#' .fmt_errs()
#' .fmt_errs(d = 100)
#' .fmt_errs(st = "", bg = "", fg = "")
#' .fmt_errs(st = "q", bg = "q", fg = "q")
#' .fmt_errs(nullst = F, nullbg = F, nullfg = F)
#' @export
fmt_errs <- function(st = NULL, bg = NULL, fg = NULL, d = " ", nullst = TRUE, nullbg = TRUE, nullfg = TRUE) {
  Fun  <- dlg::caller()
  st   <- dlg::failsafe(st)
  bg   <- dlg::failsafe(bg)
  fg   <- dlg::failsafe(fg)
  d    <- dlg::failsafe(d)
  okST <- dlg::f0(base::is.null(st), nullst, ppp:::.unq_chr_vec(st, .valid = base::c(dlg::.sts(), base::toupper(dlg::.sts()))))
  okBG <- dlg::f0(base::is.null(bg), nullbg, ppp:::.cmp_chr_scl(bg, .valid = base::c(dlg::.bgs(), base::toupper(dlg::.bgs()))))
  okFG <- dlg::f0(base::is.null(fg), nullfg, ppp:::.cmp_chr_scl(fg, .valid = base::c(dlg::.fgs(), base::toupper(dlg::.fgs()))))
  okD  <- ppp::.cmp_chr_scl(d)
  Errs <- NULL
  if (!okBG) {Errs <- base::c(Errs, "[bg] must be a character scalar from bg_vals().")}
  if (!okFG) {Errs <- base::c(Errs, "[fg] must be a character scalar from fg_vals().")}
  if (!okST) {Errs <- base::c(Errs, "[st] must be a unique character vec from st_vals().")}
  if (!okD ) {Errs <- base::c(Errs, "[d] must be a non-NA character scalar.")}
  if (!base::is.null(Errs)) {ppp::stopperr(Errs, .fun = Fun, .pkg = "dlg")}
}

#' @describeIn match_alias Matches any valid alias for background color.
#' @export
match_bg <- function(x) {
  if      (x %in% dlg::.blu()) {"blu"}
  else if (x %in% dlg::.cyn()) {"cyn"}
  else if (x %in% dlg::.grn()) {"grn"}
  else if (x %in% dlg::.blk()) {"blk"}
  else if (x %in% dlg::.mag()) {"mag"}
  else if (x %in% dlg::.red()) {"red"}
  else if (x %in% dlg::.wht()) {"wht"}
  else if (x %in% dlg::.yel()) {"yel"}
  else                         {"def"}
}

#' @describeIn match_alias Matches any valid alias for foreground color.
#' @export
match_fg <- function(x) {
  if      (x %in% dlg::.blu()) {"blu"}
  else if (x %in% dlg::.cyn()) {"cyn"}
  else if (x %in% dlg::.grn()) {"grn"}
  else if (x %in% dlg::.blk()) {"blk"}
  else if (x %in% dlg::.mag()) {"mag"}
  else if (x %in% dlg::.red()) {"red"}
  else if (x %in% dlg::.sil()) {"sil"}
  else if (x %in% dlg::.wht()) {"wht"}
  else if (x %in% dlg::.yel()) {"yel"}
  else                         {"def"}
}

#' @describeIn match_alias Matches any valid alias for style.
#' @export
match_st <- function(x) {
  if      (x %in% dlg::.bld()) {"bld"}
  else if (x %in% dlg::.itl()) {"itl"}
  else if (x %in% dlg::.pln()) {"pln"}
  else if (x %in% dlg::.und()) {"und"}
  else                         {"def"}
}

#' @encoding UTF-8
#' @family utils
#' @title Validate a Formatting Spec (of form "[bg.color]|[fg.color]|[style]")
#' @param x A character scalar formatting spec.
#' @return A logical scalar
#' @examples
#' .ok_fmt("")
#' .ok_fmt(NULL)
#' .ok_fmt("q|r|b")
#' .ok_fmt("y|r|b")
#' .ok_fmt("  yellow | red | bold  ")
#' @export
ok_fmt <- function(x) {
  if (base::is.null(x)) {return(F)}
  else if (base::is.character(x) & base::length(x) == 1) {
    if (!base::is.na(x)) {if (x == "") {return(T)}}
  }
  x <- dlg::ssplit(x)
  if (base::length(x) != 3 | !base::is.character(x)) {F}
  else {x[1] %in% dlg::.bgs() & x[2] %in% dlg::.fgs() & base::all(x[3] %in% dlg::.sts())}
}

#' @encoding UTF-8
#' @family utils
#' @title Choose from a List of Options with a Given Message and a Variety of Options
#' @param opts An atomic vector of options to choose from.
#' @param msg   character scalar message to prompt the selection.
#' @param all  Logical scalar indicating whether it is valid to select all options.
#' @param none Logical scalar indicating whether it is valid to select none of the options.
#' @param min  Non-negative integer scalar minimum number of options to select.
#' @param max  Positive integer scalar maximum numver of options to select.
#' @param ft   Character scalar title formatting spec.
#' @param fs   Character scalar subtitle formatting spec.
#' @param fun  Character scalar name of calling function.
#' @param stk  Character vector call stack.
#' @param clr  Logical scalar indicating whether to clear the console before alerting the user to the selection.
#' @param can  Logical scalar indicating whether cancellation of selection is valid.
#' @return An atomic vector.
#' @examples
#' \dontrun{
#'   choose_from(letters, "What letter?", T, N, 0, 26, "b|s|u", "w|b|p", "console", "console", F)
#'   choose_from(0:9    , "What number?", F, F, 1, 1 , "r|y|b", "y|r|i", "console", "console", T, .cancel = F)
#' }
#'
#' @export
choose_from <- function(opts, msg, all, none, min, max, ft, fs, fun, stk, clr, can = T) {
  if (clr) {dlg::xcon()}
  if (ft != "") {
    tbg <- base::strsplit(ft, "|", fixed = T)[[1]][1]
    tfg <- base::strsplit(ft, "|", fixed = T)[[1]][2]
    tst <- base::strsplit(ft, "|", fixed = T)[[1]][3]
  } else {tbg <- tfg <- tst <- NULL}
  if (fs != "") {
    sbg <- base::strsplit(fs, "|", fixed = T)[[1]][1]
    sfg <- base::strsplit(fs, "|", fixed = T)[[1]][2]
    sst <- base::strsplit(fs, "|", fixed = T)[[1]][3]
  } else {sbg <- sfg <- NULL}
  dlg::alert(msg, title = "response required", ft = ft, fs = fs, clear = clr)
  if (can) {
    if (TRUE) {base::cat(dlg::txt("\nCODE   OPTION         ", bg = sbg, fg = sfg, st = sst))}
    if (TRUE) {base::cat("\n   X = { CANCEL }")}
    if (none) {base::cat("\n   N = { NONE }")}
    if (all ) {base::cat("\n   A = { ALL }")}
  } else {
    if (TRUE) {base::cat(dlg::txt("\nCODE   OPTION         ", bg = sbg, fg = sfg, st = sst))}
    if (none) {base::cat("\n   N = { NONE }")}
    if (all ) {base::cat("\n   A = { ALL }")}
  }
  for (i in 1:base::length(opts)) {
    code <- base::as.character(i)
    pref <- dlg::p0(base::rep.int(" ", 4 - base::nchar(code)), collapse = "")
    inf  <- " = "
    opt  <- base::gsub(" ", " ", opts[i], fixed = T)
    base::cat("\n", pref, code, inf, opt, sep = "")
  }
  base::cat("\n\n")
  if      (min == 1 & max == 1) {base::cat(dlg::txt("Enter the code for"                       , min,            "of the above options:", d = " ", bg = tbg, fg = tfg, st = tst))}
  else if (min ==     max     ) {base::cat(dlg::txt("Enter a comma separated list of codes for", min,            "of the above options:", d = " ", bg = tbg, fg = tfg, st = tst))}
  else                          {base::cat(dlg::txt("Enter a comma separated list of codes for", min, "to", max, "of the above options:", d = " ", bg = tbg, fg = tfg, st = tst))}
  ans <- base::toupper(base::trimws(base::strsplit(base::readline(), ",", fixed = TRUE)[[1]], which = "both"))
  if (base::length(ans) == 1) {
    if (can  & ans == "X") {ppp::stopperr("Canceled by user.", .fun = fun, .pkg = "dlg", stack = stk)}
    if (none & ans == "N") {return(NULL)}
    if (all  & ans == "A") {return(opts)}
    ans <- base::as.numeric(ans)
    if (!ppp::cmp_psw_scl(ans)) {ppp::stopperr("Invalid selection code"                                         , .fun = fun, .pkg = "dlg", .stack = stk)}
    if (1 < min               ) {ppp::stopperr("Too few options selected."                                      , .fun = fun, .pkg = "dlg", .stack = stk)}
    if (ans > dlg::N(opts)    ) {ppp::stopperr("Selection code is greater than the number of available options.", .fun = fun, .pkg = "dlg", .stack = stk)}
  } else {
    ans <- base::as.numeric(ans)
    if (!ppp::cmp_psw_vec(ans)       ) {ppp::stopperr("Unrecognized selection code(s)."                                  , .fun = fun, .pkg = "dlg", .stack = stk)}
    if (dlg::N(ans) < min            ) {ppp::stopperr("Too few options selected."                                        , .fun = fun, .pkg = "dlg", .stack = stk)}
    if (dlg::N(ans) > max            ) {ppp::stopperr("Too many options selected."                                       , .fun = fun, .pkg = "dlg", .stack = stk)}
    if (base::any(ans > dlg::N(opts))) {ppp::stopperr("A selection code is greater than the number of available options.", .fun = fun, .pkg = "dlg", .stack = stk)}
  }
  opts[ans]
}

#' @encoding UTF-8
#' @title Color Aliases
#' @description Each color is assigned a set of aliases, all of which will work in this package.
#' @return A character vector.
#' @examples
#' .blk()
#' .blu()
#' .cyn()
#' .grn()
#' .mag()
#' .red()
#' .sil()
#' .wht()
#' .yel()
#' .bgs()
#' .fgs()
#' @export
color_aliases <- function() {utils::help("color_aliases", package = "dlg")}

#' @encoding UTF-8
#' @title Style Aliases
#' @description Each style is assigned a set of aliases, all of which will work in this package.
#' @return A character vector.
#' @examples
#' .bld()
#' .itl()
#' .pln()
#' .def()
#' .und()
#' .res()
#' .sts()
#' @export
style_aliases <- function() {utils::help("style_aliases", package = "dlg")}

#' @encoding UTF-8
#' @family utils
#' @title Match Color or Style Aliases
#' @description Returns the Official Color or Style Designation from Any Alias
#' @param x A character scalar alias.
#' @return A 3-letter character scalar.
#' @examples
#' cat("\n\nBACKGROUND COLORS"); for (b in 1:bgs()) {cat("\n'", match_bg(b), "' << '", b, "'", sep = "")}
#' cat("\n\nFOREGROUND COLORS"); for (f in 1:bgs()) {cat("\n'", match_fg(f), "' << '", f, "'", sep = "")}
#' cat("\n\nTEXT STYLES"      ); for (s in 1:sts()) {cat("\n'", match_st(s), "' << '", s, "'", sep = "")}
#' @export
match_alias <- function() {utils::help("match_alias", package = "dlg")}

#' @describeIn color_aliases Aliases for "black".
#' @export
.blk <- function() {base::c("k", "blk", "black")}

#' @describeIn color_aliases Aliases for "blue".
#' @export
.blu <- function() {base::c("b", "blu", "blue")}

#' @describeIn color_aliases Aliases for "cyan".
#' @export
.cyn <- function() {base::c("c", "cyn", "cyan")}

#' @describeIn color_aliases Aliases for "green".
#' @export
.grn <- function() {base::c("g", "grn", "green")}

#' @describeIn color_aliases Aliases for "magenta".
#' @export
.mag <- function() {base::c("m", "mag", "magenta")}

#' @describeIn color_aliases Aliases for "red".
#' @export
.red <- function() {base::c("r", "red")}

#' @describeIn color_aliases Aliases for "silver".
#' @export
.sil <- function() {base::c("s", "sil", "slv", "silver", "gray", "grey", "gry")}

#' @describeIn color_aliases Aliases for "white".
#' @export
.wht <- function() {base::c("w", "wht", "white")}

#' @describeIn color_aliases Aliases for "yellow".
#' @export
.yel <- function() {base::c("y", "yel", "ylw", "yellow")}

#' @describeIn color_aliases All aliases for background colors.
#' @export
.bgs <- function() {base::sort(base::c(dlg::.blk(), dlg::.blu(), dlg::.cyn(), dlg::.grn(), dlg::.mag(), dlg::.red(), dlg::.wht(), dlg::.yel(), dlg::.def()))}

#' @describeIn color_aliases All aliases for foreground colors.
#' @export
.fgs <- function() {base::sort(base::c(dlg::.bgs(), dlg::.sil()))}

#' @describeIn style_aliases All aliases for bold style.
#' @export
.bld <- function() {base::c("b", "bo", "bld", "bold", "bolded", "s", "str", "strong", "strengthened")}

#' @describeIn style_aliases All aliases for italic style.
#' @export
.itl <- function() {base::c("i", "it", "itl", "italic", "italics", "italicized", "e", "em", "emph", "emphasis", "emphasized")}

#' @describeIn style_aliases All aliases for plain style
#' @export
.pln <- function() {base::c("p", "pl", "pln", "plain")}

#' @describeIn style_aliases All aliases for default style
#' @export
.def <- function() {base::c("d", "df", "def", "default")}

#' @describeIn style_aliases All aliases for underline style.
#' @export
.und <- function() {base::c("u", "un", "und", "underline", "underlined")}

#' @describeIn style_aliases All aliases for resetting styles and colors back to default values.
#' @export
.res <- function() {base::c("r", "res", "reset")}

#' @describeIn style_aliases All aliases for styles.
#' @export
.sts <- function() {base::sort(base::c(dlg::.bld(), dlg::.itl(), dlg::.pln(), dlg::.def(), dlg::.und(), dlg::.res()))}

