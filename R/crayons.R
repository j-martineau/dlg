#' @title Wrappers for Package \code{\link[crayon]{crayon}} Functions to Style and Color Console Text
#' @description Functions in this family apply styles and colors to text for display on the console, where the text to display is identified by \link[=glue_dots]{collapsing} `...` args into a character scalar.
#' @section Background color functions:
#' \tabular{ll}{  `bg_vals`   \tab Get all valid values of arg `.bg`.                                   \cr
#'                `bg_red`    \tab Set to red (\code{\link[crayon:bgRed]{crayon::bgRed}}).              \cr
#'                `bg_blu`    \tab Set to blue (\code{\link[crayon:bgBlue]{crayon::bgBlue}}).           \cr
#'                `bg_cyn`    \tab Set to cyan (\code{\link[crayon:bgCyan]{crayon::bgCyan}}).           \cr
#'                `bg_blk`    \tab Set to black (\code{\link[crayon:bgBlack]{crayon::bgBlack}}).        \cr
#'                `bg_grn`    \tab Set to green (\code{\link[crayon:bgGreen]{crayon::bgGreen}}).        \cr
#'                `bg_wht`    \tab Set to white (\code{\link[crayon:bgWhite]{crayon::bgWhite}}).        \cr
#'                `bg_ylw`    \tab Set to yellow (\code{\link[crayon:bgYellow]{crayon::bgYellow}}).     \cr
#'                `bg_mag`    \tab Set to magenta (\code{\link[crayon:bgMagenta]{crayon::bgMagenta}}).  \cr
#'                `bg_def`    \tab Set to system default.                                               \cr
#'                `bg`        \tab Set to value of arg `.bg`.                                              }
#' \cr\cr Valid values of `.bg` are the following:
#' \tabular{ll}{  magenta   \tab `'m'`, `'mag'`, `'magenta'` \cr
#'                default   \tab `'d'`, `'def'`, `'default'` \cr
#'                yellow    \tab `'y'`, `'ylw'`, `'yellow'`  \cr
#'                black     \tab `'k'`, `'blk'`, `'black'`   \cr
#'                white     \tab `'w'`, `'wht'`, `'white'`   \cr
#'                green     \tab `'g'`, `'grn'`, `'green'`   \cr
#'                blue      \tab `'b'`, `'blu'`, `'blue'`    \cr
#'                cyan      \tab `'c'`, `'cyn'`, `'cyan'`    \cr
#'                red       \tab `'r'`, `'red'`                }
#' NOTE: Silver (grey) is *not* valid for *background*.
#' @section Foreground color functions:
#' \tabular{ll}{  `fg_vals`   \tab Get all valid values of `.fg` (foreground text color).              \cr
#'                `fg_red`    \tab Set to red (\code{\link[crayon:red]{crayon::red}}).                 \cr
#'                `fg_blu`    \tab Set to red (\code{\link[crayon:blue]{crayon::blue}}).               \cr
#'                `fg_cyn`    \tab Set to red (\code{\link[crayon:cyan]{crayon::cyan}}).               \cr
#'                `fg_blk`    \tab Set to red (\code{\link[crayon:black]{crayon::black}}).             \cr
#'                `fg_grn`    \tab Set to red (\code{\link[crayon:green]{crayon::green}}).             \cr
#'                `fg_wht`    \tab Set to red (\code{\link[crayon:white]{crayon::white}}).             \cr
#'                `fg_ylw`    \tab Set to red (\code{\link[crayon:yellow]{crayon::yellow}}).           \cr
#'                `fg_mag`    \tab Set to red (\code{\link[crayon:magenta]{crayon::magenta}}).         \cr
#'                `fg_gry`    \tab Set to silver (grey) (\code{\link[crayon:silver]{crayon::silver}}). \cr
#'                `fg_sil`    \tab Set to silver (grey).                                               \cr
#'                `fg_slv`    \tab Set to silver (grey).                                               \cr
#'                `fg_def`    \tab Set to system default.                                              \cr
#'                `fg`        \tab Set to value of arg `.fg`.                                            }
#' \cr\cr Valid values of `.fg` are :
#' \tabular{ll}{  silver (grey)   \tab `'s'`, `'sil'`, `'slv'`, `'silver'`, `'gry'`, `'gray'`, `'grey'` \cr
#'                magenta         \tab `'m'`, `'mag'`, `'magenta'`                                      \cr
#'                default         \tab `'.d'`, `'def'`, `'default'`                                     \cr
#'                yellow          \tab `'y'`, `'ylw'`, `'yellow'`                                       \cr
#'                black           \tab `'k'`, `'blk'`, `'black'`                                        \cr
#'                white           \tab `'w'`, `'wht'`, `'white'`                                        \cr
#'                green           \tab `'g'`, `'grn'`, `'green'`                                        \cr
#'                blue            \tab `'b'`, `'blu'`, `'blue'`                                         \cr
#'                cyan            \tab `'c'`, `'cyn'`, `'cyan'`                                         \cr
#'                red             \tab `'r'`, `'red'`                                                     }
#' NOTE: Silver (grey) is *only* valid for *foreground*.
#' @section Style functions:
#' \tabular{ll}{  `st_vals`   \tab Get all valid values of arg `.st`.                              \cr
#'                `st_bld`    \tab Set to bold (\code{\link[crayon:bgRed]{crayon::bold}}).         \cr
#'                `st_pln`    \tab Set to plain (\code{\link[crayon:bgBlue]{crayon::reset}}).      \cr
#'                `st_itl`    \tab Set to italic (\code{\link[crayon:bgBlue]{crayon::italic}}).    \cr
#'                `st_und`    \tab Set to underline (\code{\link[crayon:bgBlue]{crayon::italic}}). \cr
#'                `st_def`    \tab Set to system default.                                          \cr
#'                `st`        \tab Set to value of arg `.st`.                                        }
#' \cr\cr Valid values of `st` are:
#' \tabular{ll}{  underline   \tab `'u'`, `'un'`, `'und'`, `'under'`, `'underline'`, `'underlined'`                                                                         \cr   \tab   \cr
#'                default     \tab `'.d'`, `'def'`, `'default'`                                                                                                             \cr   \tab   \cr
#'                italic      \tab `'i'`, `'it'`, `'itl'`, `'ital'`, `'italic'`, `'italics'`, `'italicized'`,`'e'`, `'em'`, `'emp'`, `'emph'`, `'emphasis'`, `'emphasized'` \cr   \tab   \cr
#'                plain       \tab `'p'`,` 'pl'`, `'pln'`, `'plain'`, `'r'`, `'re'`, `'res`', `'reset'`                                                                     \cr   \tab   \cr
#'                bold        \tab `'b'`, `'bo'`, `'bld'`, `'bold'`, `'bolded'`, `'s'`, `'st'`, `'str'`, `'strong'`                                                                        }
#' @section Generic function:
#' \tabular{ll}{  `txt`   \tab Styles text using args `.st`, `.bg`, `.fg`.}
#' @param ... An arbitrary number of objects to be \link[=glue_dots]{collapsed} to a character scalar to be styled.
#' @param .st A quoted or unquoted, case-insensitive, character scalar text style from `st_vals()`. May also be `NULL` for function `st_vals`.
#' @param .bg A quoted or unquoted, case-insensitive, character scalar text background color from `bg_vals()`. May also be `NULL` for function `bg_vals`.
#' @param .fg A quoted or unquoted, case-insensitive, character scalar text foreground color from `fg_vals()`. May also be `NULL` for function `fg_vals`.
#' @param .d A non-`NA` character scalar delimiter for \link[=glue_dots]{collapsing} `...` args into a character scalar.
#' @return **A character vector** \cr\cr `st_vals, fg_vals, bg_vals`
#' \cr\cr  **A character scalar** \cr\cr All others
#' @examples
#'
#' bg_vals()
#' bg_vals("k")
#' bg_vals("mag")
#' bg_vals("yellow")
#'
#' fg_vals()
#' fg_vals("s")
#' fg_vals("blu")
#' fg_vals("white")
#'
#' st_vals()
#' st_vals("b")
#' st_vals("itl")
#' st_vals("plain")
#' st_vals("underline")
#'
#' \dontrun{
#'
#'   ## because formatting doesn't show up in help viewer examples output
#'
#'   egArg1 <- "Some "
#'   egArg2 <- c("text", " to display", "\nP.S. ")
#'
#'   cat(st_bld(egArg1, egArg2, "note"))
#'   cat(st_def(egArg1, egArg2, "note"))
#'   cat(st_itl(egArg1, egArg2, "note"))
#'   cat(st_pln(egArg1, egArg2, "note"))
#'   cat(st_und(egArg1, egArg2, "note"))
#'
#'   cat(st("u"        , egArg1, egArg2, "note"))
#'   cat(st("itl"      , egArg1, egArg2, "note"))
#'   cat(st("under"    , egArg1, egArg2, "note"))
#'
#'   cat(st_itl(st_und(egArg1, egArg2, "note")))
#'   cat(st("underline", st("itl", egArg1, egArg2, "note")))
#'
#'   cat(fg_blk(egArg1, egArg2))
#'   cat(fg_blu(egArg1, egArg2))
#'   cat(fg_cyn(egArg1, egArg2))
#'   cat(fg_def(egArg1, egArg2))
#'   cat(fg_grn(egArg1, egArg2))
#'   cat(fg_gry(egArg1, egArg2))
#'   cat(fg_sil(egArg1, egArg2))
#'   cat(fg_slv(egArg1, egArg2))
#'   cat(fg_mag(egArg1, egArg2))
#'   cat(fg_red(egArg1, egArg2))
#'   cat(fg_wht(egArg1, egArg2))
#'   cat(fg_ylw(egArg1, egArg2))
#'   cat(fg_def(egArg1, egArg2))
#'
#'   cat(fg("s", egArg1, egArg2, "note"))
#'   cat(fg("sil", egArg1, egArg2, "note"))
#'   cat(fg("grey", egArg1, egArg2, "note"))
#'
#'   cat(bg_blk(egArg1, egArg2, "note"))
#'   cat(bg_blu(egArg1, egArg2, "note"))
#'   cat(bg_cyn(egArg1, egArg2, "note"))
#'   cat(bg_def(egArg1, egArg2, "note"))
#'   cat(bg_grn(egArg1, egArg2, "note"))
#'   cat(bg_mag(egArg1, egArg2, "note"))
#'   cat(bg_red(egArg1, egArg2, "note"))
#'   cat(bg_wht(egArg1, egArg2, "note"))
#'   cat(bg_ylw(egArg1, egArg2, "note"))
#'   cat(bg_def(egArg1, egArg2, "note"))
#'
#'   cat(bg("y"     , egArg1, egArg2, "note"))
#'   cat(bg("blu"   , egArg1, egArg2, "note"))
#'   cat(bg("yellow", egArg1, egArg2, "note"))
#'
#'   cat(bg("y", fg("r", st("u", st("i", egArg1, egArg2, "note")))))
#'
#'   cat(txt(egArg1, egArg2, "note", .bg = "blue", .fg = "w", .st = c("i", "under")))
#' }
#' @export
crayons <- function() {utils::help("crayons", package = "dlg")}

#' @rdname crayons
#' @export
st_vals <- function(.st = NULL) {
  dlg::.fmt_errs(.st = .st)
  if (!base::is.null(.st)) {
    .st <- dlg::.match_st(.st)
    if      (.st == "bld") {dlg::.bld()}
    else if (.st == "itl") {dlg::.itl()}
    else if (.st == "def") {dlg::.def()}
    else if (.st == "und") {dlg::.und()}
    else                   {dlg::.pln()}
  } else                   {dlg::.sts()}
}

#' @rdname crayons
#' @export
bg_vals <- function(.bg = NULL) {
  dlg::.fmt_errs(.bg = .bg)
  if (!base::is.null(.bg)) {
    .bg <- dlg::.match_bg(.bg)
    if      (.bg == "red") {dlg::.red()}
    else if (.bg == "blu") {dlg::.blu()}
    else if (.bg == "cyn") {dlg::.cyn()}
    else if (.bg == "blk") {dlg::.blk()}
    else if (.bg == "grn") {dlg::.grn()}
    else if (.bg == "wht") {dlg::.wht()}
    else if (.bg == "mag") {dlg::.mag()}
    else if (.bg == "ylw") {dlg::.ylw()}
    else                   {dlg::.def()}
  } else                   {dlg::.bgs()}
}

#' @rdname crayons
#' @export
fg_vals <- function(.fg = NULL) {
  dlg::.fmt_errs(.fg = .fg)
  if (!base::is.null(.fg)) {
    .fg <- dlg::.match_fg(.fg)
    if      (.fg == "red") {dlg::.red()}
    else if (.fg == "blu") {dlg::.blu()}
    else if (.fg == "cyn") {dlg::.cyn()}
    else if (.fg == "blk") {dlg::.blk()}
    else if (.fg == "grn") {dlg::.grn()}
    else if (.fg == "wht") {dlg::.wht()}
    else if (.fg == "sil") {dlg::.sil()}
    else if (.fg == "mag") {dlg::.mag()}
    else if (.fg == "ylw") {dlg::.ylw()}
    else                   {dlg::.def()}
  } else                   {dlg::.fgs()}
}

# style ####

#' @rdname crayons
#' @export
st_bld <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::bold(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
st_def <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); dlg::.glue_args(.d, ...)}

#' @rdname crayons
#' @export
st_itl <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::italic(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
st_pln <- st_def

#' @rdname crayons
#' @export
st_und <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::underline(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
st_res <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::reset(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
st <- function(.st, ..., .d = " ") {
  dlg::.fmt_errs(.st = .st, .d = .d, .null.st = F)
  .st <- base::tolower(.st)
  x   <- dlg::.glue_args(.d, ...)
  if      (.st %in% dlg::.bld()) {x <- uj::st_bld(x)}
  else if (.st %in% dlg::.pln()) {x <- uj::st_pln(x)}
  else if (.st %in% dlg::.itl()) {x <- uj::st_itl(x)}
  else if (.st %in% dlg::.und()) {x <- uj::st_und(x)}
  else                           {x <- uj::st_def(x)}
  x
}

# fg color ####

#' @rdname crayons
#' @export
fg_blk <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::black(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg_blu <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::blue(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg_cyn <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::cyan(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg_def <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); dlg::.glue_args(.d, ...)}

#' @rdname crayons
#' @export
fg_grn <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::green(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg_gry <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::silver(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg_mag <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::magenta(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg_red <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::red(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg_sil <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::silver(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg_slv <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::silver(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg_wht <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::white(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg_ylw <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::yellow(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
fg <- function(.fg, ..., .d = " ") {
  dlg::.fmt_errs(.fg = .fg, .d = .d, .null.fg = F)
  .fg <- base::tolower(.fg)
  x   <- dlg::.glue_args(.d, ...)
  if      (.fg %in% dlg::.red()) {x <- uj::fg_red(x)}
  else if (.fg %in% dlg::.blu()) {x <- uj::fg_blu(x)}
  else if (.fg %in% dlg::.cyn()) {x <- uj::fg_cyn(x)}
  else if (.fg %in% dlg::.blk()) {x <- uj::fg_blk(x)}
  else if (.fg %in% dlg::.grn()) {x <- uj::fg_grn(x)}
  else if (.fg %in% dlg::.wht()) {x <- uj::fg_wht(x)}
  else if (.fg %in% dlg::.sil()) {x <- uj::fg_sil(x)}
  else if (.fg %in% dlg::.ylw()) {x <- uj::fg_ylw(x)}
  else if (.fg %in% dlg::.mag()) {x <- uj::fg_mag(x)}
  else                           {x <- uj::fg_def(x)}
  x
}

# bg color ####

#' @rdname crayons
#' @export
bg_blk <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::bgBlack(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
bg_blu <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::bgBlue(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
bg_cyn <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::bgCyan(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
bg_def <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); dlg::.glue_args(.d, ...)}

#' @rdname crayons
#' @export
bg_grn <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::bgGreen(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
bg_mag <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::bgMagenta(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
bg_red <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::bgRed(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
bg_wht <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::bgWhite(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
bg_ylw <- function(..., .d = " ") {dlg::.fmt_errs(.d = .d); crayon::bgYellow(dlg::.glue_args(.d, ...))}

#' @rdname crayons
#' @export
bg <- function(.bg, ..., .d = " ") {
  dlg::.fmt_errs(.bg = .bg, .d = .d, .null.bg = F)
  .bg <- base::tolower(.bg)
  x   <- dlg::.glue_args(.d, ...)
  if      (.bg %in% dlg::.red()) {x <- uj::bg_red(x)}
  else if (.bg %in% dlg::.blu()) {x <- uj::bg_blu(x)}
  else if (.bg %in% dlg::.cyn()) {x <- uj::bg_cyn(x)}
  else if (.bg %in% dlg::.blk()) {x <- uj::bg_blk(x)}
  else if (.bg %in% dlg::.grn()) {x <- uj::bg_grn(x)}
  else if (.bg %in% dlg::.wht()) {x <- uj::bg_wht(x)}
  else if (.bg %in% dlg::.ylw()) {x <- uj::bg_ylw(x)}
  else if (.bg %in% dlg::.mag()) {x <- uj::bg_mag(x)}
  else                           {x <- uj::bg_def(x)}
  x
}

# generic ####

#' @rdname crayons
#' @export
txt <- function(..., .bg = NULL, .fg = NULL, .st = NULL, .d = " ") {
  dlg::.fmt_errs(.bg = .bg, .fg = .fg, .st = .st, .d = .d, .null.bg = T, .null.fg = T, .null.st = T)
  x <- dlg::.glue_args(.d, ...)
  if (!base::is.null(.st)) {x <- uj::st(base::tolower(.st), x)}
  if (!base::is.null(.bg)) {x <- uj::bg(base::tolower(.bg), x)}
  if (!base::is.null(.fg)) {x <- uj::fg(base::tolower(.fg), x)}
  x
}
