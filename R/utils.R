#' @export
.av <- uj::av

#' @export
.p0 <- base::paste0

#' @encoding UTF-8
#' @family utils
#' @title Flattens `...` Args to a Character Vector and Paste the Resulting Elements together Using Delimiter `.d`
#' @param .d Atomic scalar delimiter.
#' @param ... Objects to be flattened.
#' @return A character scalar.
#' @examples
#' .glue_args(" ", letters)
#' .glue_args("" , letters)
#' .glue_args("|", letters, LETTERS, 0:9)
#' @export
.glue_args <- function(.d, ...) {base::paste0(dlg::.av(...), collapse = .d)}

#' @encoding UTF-8
#' @family utils
#' @title Collapse and Glue `...` Arguments with Space as Default Delimiter
#' @inheritParams .glue_args
#' @inheritDotParams .glue_args
#' @return A character scalar.
#' @examples
#' .glu(letters)
#' .glu(letters, 0:9, .d = "")
#' @export
.glu <- function(..., .d = " ") {dlg::.glue_args(.d, ...)}

#' @encoding UTF-8
#' @family utils
#' @title Glue and Trim Whitespace from Both Sides
#' @inheritParams .glue_args
#' @inheritDotParams .glue_args
#' @return A character scalar.
#' @examples
#' .trm(" Just" ,  "a" ,  "little" ,  "thing ")
#' .trm(" Just ", " a ", " little ", " thing ")
#' .trm(" ", letters, LETTERS, 0:9, " ", .d = "")
#' @export
.trm <- function(..., .d = " ") {base::trimws(dlg::.glu(..., .d = .d), which = "both")}

#' @encoding UTF-8
#' @family utils
#' @title Split Strings along Pipes and Trim Whitespace from Both Sides of All Resulting Elements
#' @param x An object of mode character.
#' @return A character vector
#' @examples
#' .ssplit(" Just | a | little | thing ")
#' .ssplit("  Just  |  a  |  little  |  thing  ")
#' @export
.ssplit <- function(x) {base::trimws(uj::av(base::strsplit(x, "|", fixed = T)), which = "both", whitespace = "[ ]")}

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
.bgs <- function() {base::sort(base::c(dlg::.blk(), dlg::.blu(), dlg::.cyn(), dlg::.grn(), dlg::.mag(), dlg::.red(), dlg::.wht(), dlg::.yel(), dlg::.def(), dlg::.res()))}

#' @describeIn color_aliases All aliases for foreground colors.
#' @export
.fgs <- function() {base::sort(base::c(dlg::.bgs(), dlg::.sil()))}

#' @describeIn style_aliases All aliases for bold style.
#' @export
.bld <- function() {base::c("b", "bo", "bld", "bolded", "s", "str", "strong")}

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

#' @describeIn style_aliases All aliases for foreground colors.
#' @export
.res <- function() {base::c("r", "res", "reset")}

#' @describeIn style_aliases All aliases for styles.
#' @export
.sts <- function() {base::sort(base::c(dlg::.bld(), dlg::.itl(), dlg::.pln(), dlg::.def(), dlg::.und(), dlg::.res()))}

#' @encoding UTF-8
#' @family utils
#' @title Validate Formatting Argument Values
#' @param .st Optional style spec.
#' @param .bg Optional background color spec.
#' @param .fg Optional foreground color spec.
#' @param .d Character scalar text delimiter.
#' @param .null.st Logical scalar indicating whether a `NULL` style spec is valid.
#' @param .null.bg Logical scalar indicating whether a `NULL` background color spec is valid.
#' @param .null.fg Logical scalar indicating whether a `NULL` foreground color spec is valid.
#' @examples
#' .fmt_errs()
#' .fmt_errs(.d = 100)
#' .fmt_errs(.st = "", .bg = "", .fg = "")
#' .fmt_errs(.st = "q", .bg = "q", .fg = "q")
#' .fmt_errs(.null.st = F, .null.bg = F, .null.fg = F)
#' @export
.fmt_errs <- function(.st = NULL, .bg = NULL, .fg = NULL, .d = " ", .null.st = TRUE, .null.bg = TRUE, .null.fg = TRUE) {
  Fun  <- uj::caller()
  .st  <- uj::failsafe(.st)
  .bg  <- uj::failsafe(.bg)
  .fg  <- uj::failsafe(.fg)
  .d   <- uj::failsafe(.d)
  OkST <- uj::f0(base::is.null(.st), .null.st, ppp:::.unq_chr_vec(.st, .valid = base::c(dlg::sts(), base::toupper(dlg::sts()))))
  OkBg <- uj::f0(base::is.null(.bg), .null.bg, ppp:::.cmp_chr_scl(.bg, .valid = base::c(dlg::bgs(), base::toupper(dlg::bgs()))))
  OkFg <- uj::f0(base::is.null(.fg), .null.fg, ppp:::.cmp_chr_scl(.fg, .valid = base::c(dlg::fgs(), base::toupper(dlg::fgs()))))
  OkD  <- ppp::.cmp_chr_scl(.d)
  Errs <- NULL
  if (!OkBg) {Errs <- base::c(Errs, "[.bg] must be a character scalar from bg_vals().")}
  if (!OkFg) {Errs <- base::c(Errs, "[.fg] must be a character scalar from fg_vals().")}
  if (!OkST) {Errs <- base::c(Errs, "[.st] must be a unique character vec from st_vals().")}
  if (!OkD ) {Errs <- base::c(Errs, "[.d] must be a non-NA character scalar.")}
  if (!base::is.null(Errs)) {ppp::stopperr(Errs, .fun = Fun, .pkg = "dlg")}
}

#' @describeIn match_alias Matches any valid alias for background color.
#' @export
.match_bg <- function(x) {
  if      (x %in% dlg::.blu()) {"blu"}
  else if (x %in% dlg::.cyn()) {"cyn"}
  else if (x %in% dlg::.grn()) {"grn"}
  else if (x %in% dlg::.blk()) {"blk"}
  else if (x %in% dlg::.mag()) {"mag"}
  else if (x %in% dlg::.red()) {"red"}
  else if (x %in% dlg::.wht()) {"wht"}
  else if (x %in% dlg::.ylw()) {"ylw"}
  else                         {"def"}
}

#' @describeIn match_alias Matches any valid alias for foreground color.
#' @export
.match_fg <- function(x) {
  if      (x %in% dlg::.blu()) {"blu"}
  else if (x %in% dlg::.cyn()) {"cyn"}
  else if (x %in% dlg::.grn()) {"grn"}
  else if (x %in% dlg::.blk()) {"blk"}
  else if (x %in% dlg::.mag()) {"mag"}
  else if (x %in% dlg::.red()) {"red"}
  else if (x %in% dlg::.sil()) {"sil"}
  else if (x %in% dlg::.wht()) {"wht"}
  else if (x %in% dlg::.ylw()) {"ylw"}
  else                         {"def"}
}

#' @describeIn match_alias Matches any valid alias for style.
#' @export
.match_st <- function(x) {
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
.ok_fmt <- function(x) {
  if (base::is.null(x)) {return(F)} else if (base::is.character(x) & base::length(x) == 1) {if (!base::is.na(x)) {if (x == "") {return(TRUE)}}}
  x <- dlg::ssplit(x)
  uj::f0(base::length(x) != 3, F, uj::f0(!(x[1] %in% dlg::bgs()), F, uj::f0(!(x[2] %in% dlg::fgs()), F, base::all(x[3] %in% dlg::sts()))))
}

#' @encoding UTF-8
#' @family utils
#' @title Choose from a List of Options with a Given Message and a Variety of Options
#' @param options An atomic vector of options to choose from.
#' @param message A character scalar message to prompt the selection.
#' @param .all Logical scalar indicating whether it is valid to select all options.
#' @param .none Logical scalar indicating whether it is valid to select none of the options.
#' @param .min Non-negative integer scalar minimum number of options to select.
#' @param .max Positive integer scalar maximum numver of options to select.
#' @param .ft Character scalar title formatting spec.
#' @param .fs Character scalar subtitle formatting spec.
#' @param .fun Character scalar name of calling function.
#' @param .stack Character vector call stack.
#' @param .clear Logical scalar indicating whether to clear the console before alerting the user to the selection.
#' @param .cancel Logical scalar indicating whether cancellation of selection is valid.
#' @return An atomic vector.
#' @examples
#' \dontrun{
#'   cat_choose_list(letters, "What letter?", T, N, 0, 26, "b|s|u", "w|b|p", "console", "console", F)
#'   cat_choose_list(0:9    , "What number?", F, F, 1, 1 , "r|y|b", "y|r|i", "console", "console", T, .cancel = F)
#' }
#'
#' @export
.cat_choose_list <- function(options, message, .all, .none, .min, .max, .ft, .fs, .fun, .stack, .clear, .cancel = T) {
  if (.clear) {uj::xconsole()}
  if (.ft != "") {
    tBG <- base::strsplit(.ft, "|", fixed = T)[[1]][1]
    tFG <- base::strsplit(.ft, "|", fixed = T)[[1]][2]
    tST <- base::strsplit(.ft, "|", fixed = T)[[1]][3]
  } else {tBG <- tFG <- tST <- NULL}
  if (.fs != "") {
    sBG <- base::strsplit(.fs, "|", fixed = T)[[1]][1]
    sFG <- base::strsplit(.fs, "|", fixed = T)[[1]][2]
  } else {sBG <- sFG <- NULL}
  dlg::alert(message, Title = "response required", .ft = .ft, .fs = .fs, .clear = .clear)
  base::cat("\n")
  if (.cancel) {
    if (TRUE ) {base::cat(dlg::txt("CODE   OPTION         ", .bg = sBG, .fg = sFG, .st = "bold"))}
    if (TRUE ) {base::cat(      "\n   X   { CANCEL }")}
    if (.none) {base::cat(      "\n   N   { NONE }")}
    if (.all ) {base::cat(      "\n   A   { ALL }")}
  } else {
    if (TRUE ) {base::cat(dlg::txt("CODE   OPTION         ", .bg = sBG, .fg = sFG, .st = "bold"))}
    if (.none) {base::cat(      "\n   N   { NONE }")}
    if (.all ) {base::cat(      "\n   A   { ALL }")}
  }
  for (i in 1:base::length(options)) {
    Code   <- base::as.character(i)
    Prefix <- base::paste0(base::rep.int(" ", 4 - base::nchar(Code)), collapse = "")
    Infix  <- "   "
    Option <- base::gsub(" ", " ", options[i], fixed = T)
    base::cat("\n", Prefix, Code, Infix, Option, sep = "")
  }
  base::cat("\n\n")
  if      (.min == 1 & .max == 1) {base::cat(dlg::txt("Enter the code for"                       , .min,             "of the above options:", .d = " ", .bg = tBG, .fg = tFG, .st = tST))}
  else if (.min ==     .max     ) {base::cat(dlg::txt("Enter a comma separated list of codes for", .min,             "of the above options:", .d = " ", .bg = tBG, .fg = tFG, .st = tST))}
  else                            {base::cat(dlg::txt("Enter a comma separated list of codes for", .min, "to", .max, "of the above options:", .d = " ", .bg = tBG, .fg = tFG, .st = tST))}
  Answer <- base::toupper(base::trimws(base::strsplit(base::readline(), ",", fixed = TRUE)[[1]], which = "both"))
  if (base::length(Answer) == 1) {
    if (.cancel & Answer == "X") {ppp::stopperr("Canceled by user.", .fun = .fun, .pkg = "dlg", .stack = .stack)}
    if (.none   & Answer == "N") {return(NULL)}
    if (.all    & Answer == "A") {return(options)}
    Answer <- base::as.numeric(Answer)
    if (!ppp::cmp_psw_scl(Answer)) {ppp::stopperr("Invalid selection code"                                         , .fun = .fun, .pkg = "dlg", .stack = .stack)}
    if (1 < .min                 ) {ppp::stopperr("Too few options selected."                                      , .fun = .fun, .pkg = "dlg", .stack = .stack)}
    if (Answer > uj::N(options)  ) {ppp::stopperr("Selection code is greater than the number of available options.", .fun = .fun, .pkg = "dlg", .stack = .stack)}
  } else {
    Answer <- base::as.numeric(Answer)
    if (!ppp::cmp_psw_vec(Answer)         ) {ppp::stopperr("Unrecognized selection code(s)."                                  , .fun = .fun, .pkg = "dlg", .stack = .stack)}
    if (uj::N(Answer) < .min              ) {ppp::stopperr("Too few options selected."                                        , .fun = .fun, .pkg = "dlg", .stack = .stack)}
    if (uj::N(Answer) > .max              ) {ppp::stopperr("Too many options selected."                                       , .fun = .fun, .pkg = "dlg", .stack = .stack)}
    if (base::any(Answer > uj::N(options))) {ppp::stopperr("A selection code is greater than the number of available options.", .fun = .fun, .pkg = "dlg", .stack = .stack)}
  }
  options[Answer]
}
