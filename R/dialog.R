#' @name dialog
#' @encoding UTF-8
#' @family dialogs
#' @family user
#' @title Console-Based User Dialog (may also use package `svDialogs` functions)
#' @description All functions \link[=collapse_dots]{collapses} `...` args into a prompt. Each posts an alert to the console, posts the prompt (if any), followed by a specific action.
#' @details
#' \tabular{ll}{  `acknowledge`   \tab Waits for user to acknowledge.                        \cr
#'                                \tab                                                       \cr
#'                `OK, CANCEL`    \tab Offers OK and CANCEL options.\eqn{^{(3)}}             \cr
#'                                \tab                                                       \cr
#'                `choose_doc`    \tab Asks user to choose a document.\eqn{^{(1)}}           \cr
#'                `choose_dir`    \tab Asks user to choose a directory.\eqn{^{(1)}}          \cr
#'                                \tab                                                       \cr
#'                `choose1`       \tab Asks user to choose `1` option.\eqn{^{(1)}}           \cr
#'                `chooseN`       \tab Asks user to choose `1+` options.\eqn{^{(1,2)}}       \cr
#'                                \tab                                                       \cr
#'                `YES, NO`       \tab Offers YES, NO, and CANCEL options.\eqn{^{(1,3)}}     \cr
#'                                \tab                                                       \cr
#'                `ask_new`       \tab Asks for a list of new values.\eqn{^{(1)}}            \cr
#'                                \tab                                                       \cr
#'                `alert`         \tab No subsequent action.                                 \cr
#'                                \tab                                                       \cr
#'                `ask`           \tab Asks for typed input.\eqn{^{(1)}}                     \cr
#'                                \tab                                                         }
#'  \tabular{l}{  \eqn{^{(1)}} Stops execution if user chooses `CANCEL`.                     \cr
#'                \eqn{^{(2)}} Stops execution if `stop | (type = "error" & is.null(stop))`. \cr
#'                \eqn{^{(3)}} Returns `TRUE` if user choice matches the function name.        }
#' The following give templates for what the user sees, where any value derived from arguments will be absent if it is or resolves to a blank string.
#' \cr\cr
#' \strong{\code{alert}}
#' \tabular{ll}{
#'     **CONDITION**           \tab **VALUE**                       \cr
#'     `g(.d, .title) != ""`   \tab formatted title from `.title`.  \cr
#'     `g(.d, .sub) != ""`     \tab formatted subtitle from `.sub`. \cr
#'     `g(.d, ...) != ""`      \tab formatted message from `...`.   \cr
#'                             \tab `< blank line >`                \cr
#'     `g(.d, .ps) != ""`      \tab formatted postscript from `.ps`.  }
#' .all other templates incorporate a call to `alert(.)` with components as shown below.
#' \cr\cr\strong{\code{acknowledge}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                 \cr
#'     .title          \tab `'ACKNOWLEDGMENT REQUIRED'`               \cr
#'     Subtitle        \tab `'< g(.d, .sub) >'`                       \cr
#'     Message         \tab `'< g(.d, ...) >'`                        \cr
#'                     \tab `< blank line >`                          \cr
#'     Postscript      \tab `'press [return] or [enter] to continue:'`  }
#' \strong{\code{choose1}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                         \cr
#'     .title          \tab `'RESPONSE REQUIRED'`             \cr
#'     Subtitle        \tab `'< g(.d, .sub) >'`               \cr
#'     Message         \tab `'< g(.d, ...) >'`                \cr
#'                     \tab `< blank line >`                  \cr
#'     Postscript      \tab `'choose an option - Select one'` \cr
#'                     \tab `< blank line >`                  \cr
#'     Cancel.Option   \tab `'1: { CANCEL }`'                 \cr
#'     Option.1        \tab `'2: < options[1] >'`             \cr
#'     Option.2        \tab `'3: < options[2] >'`             \cr
#'     ...             \tab ...                               \cr
#'     Option.n        \tab `'< n + 1 >: < options[n] >'`     \cr
#'     Prompt          \tab `'Selection:'`                      }
#' \strong{\code{chooseN}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                                                      \cr
#'     .title           \tab `'RESPONSE REQUIRED'`                                                         \cr
#'     Subtitle        \tab `'< g(.d, .sub) >'`                                                            \cr
#'     Message         \tab `'< g(.d, ...) >'`                                                             \cr
#'                     \tab `< blank line >`                                                               \cr
#'     Postscript      \tab `'choose < n | between n1 and n2 > options - Select one or more'`              \cr
#'                     \tab `< blank line >`                                                               \cr
#'     Cancel.Option   \tab `'1: { CANCEL }'`                                                              \cr
#'     Option.1        \tab `'2: < options[1] >'`                                                          \cr
#'     Option.2        \tab `'3: < options[2] >'`                                                          \cr
#'     ...             \tab ...                                                                            \cr
#'     Option.n        \tab `'< n + 1 >: < options[n] >'`                                                  \cr
#'     ALL.option      \tab `'< n + 2 >: { ALL }'`                                                         \cr
#'     NONE.option     \tab `'< n + 3 >: { NONE }'`                                                        \cr
#'     Prompt          \tab `'Enter one or more numbers separated by spaces and then ENTER, or 0 to cancel'` }
#' \strong{\code{NO}} and \strong{\code{YES}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                         \cr
#'     Title           \tab `'RESPONSE REQUIRED'`             \cr
#'     Subtitle        \tab '`< g(.d, .sub) >'`               \cr
#'     Message         \tab `'< g(.d, ...) >'`                \cr
#'                     \tab `< blank line >`                  \cr
#'     Postscript      \tab `'choose an option - Select one'` \cr
#'                     \tab `< blank line >`                  \cr
#'     Cancel.option   \tab `'1: { CANCEL }'`                 \cr
#'     Yes.option      \tab `'2: { YES }'`                    \cr
#'     No.option       \tab `'3: { NO }'`                     \cr
#'     Prompt          \tab `'Selection:'`                      }
#' \strong{\code{OK}} and \strong{\code{CANCEL}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                         \cr
#'     Title           \tab `'RESPONSE REQUIRED'`             \cr
#'     Subtitle        \tab `'< g(.d, .sub) >'`               \cr
#'     Message         \tab `'< g(.d, ...) >'`                \cr
#'                     \tab `< blank line >`                  \cr
#'     Postscript      \tab `'choose an option - Select one'` \cr
#'                     \tab `< blank line >`                  \cr
#'     Cancel.option   \tab `'1: { CANCEL }'`                 \cr
#'     OK.option       \tab `'2: { OK }'`                     \cr
#'     Prompt          \tab `'Selection: '`                     }
#' \strong{\code{ask}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                 \cr
#'     Title           \tab `'RESPONSE REQUIRED'`     \cr
#'     Subtitle        \tab `'< g(.d, .sub) >'`       \cr
#'     Message         \tab `'< g(.d, ...) >'`        \cr
#'                     \tab `< blank line >`          \cr
#'     Postscript      \tab `'enter your response: '`   }
#' \strong{\code{ask_new}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                                                                                                \cr
#'     .title           \tab `'RESPONSE REQUIRED'`                                                                                                   \cr
#'     Subtitle        \tab `'< g(.d, .sub) >'`                                                                                                      \cr
#'     Message         \tab `'Enter a pipe-separated list of < N(options) > replacement values for the following pipe-separated original values: '`  \cr
#'                     \tab `< blank line >`                                                                                                         \cr
#'     old.values      \tab `'< paste0(old, collapse = " | ") >'`                                                                                    \cr
#'                     \tab `< blank line >`                                                                                                         \cr
#'     Postscript      \tab `'enter your response: '`                                                                                                  }
#' \strong{\code{choose_dir}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                          \cr
#'     Title           \tab `'ACKNOWLEDGMENT REQUIRED'`                        \cr
#'     Subtitle        \tab `'< g(.d, .sub) >'`                                \cr
#'     Message         \tab `'In the next dialog box, select a < dir.type >.'`   }
#' \strong{\code{choose_doc}}
#' \tabular{ll}{
#'     **COMPONENT**   \tab **VALUE**                                          \cr
#'     Title           \tab `'ACKNOWLEDGMENT REQUIRED'`                        \cr
#'     Subtitle        \tab `'< g(.d, .sub) >'`                                \cr
#'     Message         \tab `'In the next dialog box, select a < doc.type >.'`   }
#' @section Specifying formats: When formatting arguments (`.ft`, `.fs`, `.fm`, and `.fp`) take the special value `""`, the corresponding alert elements (`.title`, `.sub`, `...`, and `.ps`, respectively) are posted to the console without special formatting.
#' \cr\cr Otherwise, formatting arguments must be \link[=cmp_str_vec]{complete string vecs} that when \link[=av]{atomized} and \link[=ssplit]{split along pipes} results in a three-element character vector, the first element of which is used to specify \link[=bg]{text background color} and must be a value from \code{\link{bg_vals}()}, the second element of which is used to specify \link[=fg]{text foreground color} and must be a value from \code{\link{fg_vals}()}, and the last of which specifies \link[=st]{text style} and must be a value from \code{\link{st_vals}()}.
#' @param ... An arbitrary, optional number of arguments which are \link[=av]{atomized} into a character scalar message to be posted to the console.
#' @param old A \link[=chr_vec]{character vec} of unique values to be replaced.
#' @param type A character scalar describing the type of replacement values to be entered.
#' @param options An atomic vector listing options to choose from.
#' @param .d A character scalar delimiter for collapsing `...` args into a character scalar.
#' @param .n An optional \link[ppp:cmp_psw_scl]{complete positive numeric whole-number scalar} (?cmp_psw_scl) indicating the number of options that must be selected. Must be contained in `1:length(options)`.
#' @param .all Scalar `TRUE` or `FALSE` indicating whether to add an `{ ALL }` value to `options`.
#' @param .title A .title for the alert (`type = 'error'` results in stopping execution after posting the alert). Should be short to avoid formatting problems.
#' @param .sub A character scalar alert subtitle to post to the console on the line following the title Should be short to avoid formatting problems.
#' @param .ps A character scalar suffix to post to the console on the line following the alert message contained in `...`.
#' @param .u Scalar `TRUE` or `FALSE` indicating whether replacement values must be unique.
#' @param .none Scalar `TRUE` or `FALSE` indicating whether to add a `{ NONE }` value to `options` (implying that it is valid to select none).
#' @param .ft,.fs,.fm,.fp Formatting values consistent with the description in the *specifying formats* section giving formatting instructions for, respectively, the title (in `.title`), subtitle (in `.sub`), message (in `...`), and postscript (in `.ps`).
#' @param .min An optional \link[ppp:CMP]{complete} \link[ppp:PSW]{positive numeric whole-number} \link[ppp:SCL]{scalar} indicating the minimum number of options that may be selected. Must be `NULL` when `.n` is non-`NULL`.
#' @param .max An optional complete positive numeric whole-number scalar indicating the maximum number of options that may be selected. Must be `NULL` when `.n` is non-`NULL`.
#' @param .def A character scalar containing a default message if \link[=av]{atomizing} and collapsing `...` to a character scalar results in a blank string (`""`).
#' @param .clear A non-`NA` logical scalar indicating whether to clear the console before each interaction with the user.
#' @return **The** `NULL` **object** \cr\cr `acknowledge, alert`
#' \cr\cr  **A character scalar**    \cr\cr `choose_dir` (a directory path) \cr `choose_doc` (a document path) \cr `ask`
#' \cr\cr  **A character vector**    \cr\cr `ask_new`
#' \cr\cr  **An atomic vector**      \cr\cr `chooseN`
#' \cr\cr  **An atomic scalar**      \cr\cr `choose1`
#' \cr\cr  **A logical scalar**      \cr\cr `CANCEL, YES, NO, OK`
#' @examples
#' \dontrun{
#'
#'   ## because formatting doesn't show up in help viewer examples output
#'
#'   egA    <- "two-part"
#'   egB    <- "message"
#'
#'   egFT   <- c("yellow", "red", "plain")
#'   egFS   <- c("blk|wht", "und")
#'   egFM   <- "b|y|i"
#'
#'   egT    <- ".title"
#'   egS    <- "Subtitle"
#'
#'   egOpts <- paste("option", letters[1:10])
#'
#'   egM1   <- "Do you want to continue?"
#'   egM2   <- "Why do you want to continue?"
#'
#'   alert(egA, egB, .d = " ")
#'   alert(egA, egB, .title = egTitle, .d = " ")
#'   alert(egA, egB, .sub = l, .d = " ")
#'   alert(egA, egB, .title = egTitle, .sub = egSub, .d = " ")
#'   alert(egA, egB, .title = egTitle, .sub = egSub, .d = " ", .fm = egFM)
#'   alert(egA, egB, .title = egTitle, .sub = egSub, .d = " ", .fs = egFS)
#'   alert(egA, egB, .title = egTitle, .sub = egSub, .d = " ", .ft = egFT)
#'   alert(egA, egB, .title = egTitle, .sub = egSub, .d = " ", .ft = egFT, .fs = egFS, .fm = egFM)
#'   alert(.title = egT, .sub = egSub, .ft = egFT, .fs = egFS)
#'   alert(.title = egT, .sub = egS)
#'   alert(.sub = egS, .fs = egFS)
#'
#'   acknowledge(egA, egB)
#'
#'   choose1(egOpts)
#'   chooseN(egOpts)
#'   chooseN(egOpts, .all = T, .none = F, Min = 6, Max = 10)
#'
#'   NO(egMsg1)
#'   OK(egMsg2)
#'   YES(egMsg1)
#'   CANCEL(egMsg1)
#'
#'   ask(Msg2)
#'
#'   ask_new(egOpts)
#'
#'   choose_dir(dir.type = "directory for R scripts")
#'
#'   choose_doc(doc.type = "document to read")
#'
#' }
#' @export
alert <- function(..., .title = "alert", .sub = "", .ps = "", .def = "", .ft = "r|w|b", .fs = "k|y|p", .fm = "", .fp = "k|y|i", .d = " ", .clear = FALSE) {
  fmt <- function(x, f) {
    if (f == "") {return(x)}
    f <- uj::av(base::strsplit(f, "|", fixed = TRUE))
    dlg::txt(base::paste0(" ", x, " "), .BG = f[1], .FG = f[2], .ST = f[3])
  }
  Message <- base::paste0(base::as.character(uj::av(...)), collapse = .d)
  .title  <- base::paste0(base::toupper(base::as.character(uj::av(.title))), collapse = .d)
  .sub    <- base::paste0(base::as.character(uj::av(.sub)), collapse = .d)
  .ps     <- base::paste0(base::as.character(uj::av(.ps)), collapse = .d)
  .def    <- base::paste0(base::as.character(uj::av(.def)), collapse = .d)
  .ft     <- uj::f0(!ppp::.cmp_chr(.ft), "r|w|b", base::paste0(.ft, collapse = "|"))
  .fs     <- uj::f0(!ppp::.cmp_chr(.fs), "k|y|i", base::paste0(.fs, collapse = "|"))
  .fp     <- uj::f0(!ppp::.cmp_chr(.fp), "k|y|i", base::paste0(.fp, collapse = "|"))
  .fm     <- uj::f0(!ppp::.cmp_chr(.fm), "k|w|p", base::paste0(.fm, collapse = "|"))
  if (Message == "") {Message <- .def}
  Errors <- NULL
  if (!dlg::.ok_fmt(.ft)     ) {Errors <- base::c(Errors, "[.ft] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?dlg::alert).")}
  if (!dlg::.ok_fmt(.fs)     ) {Errors <- base::c(Errors, "[.fs] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?dlg::alert).")}
  if (!dlg::.ok_fmt(.fm)     ) {Errors <- base::c(Errors, "[.fm] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?dlg::alert).")}
  if (!dlg::.ok_fmt(.fp)     ) {Errors <- base::c(Errors, "[.fp] must be consistent with the description the [specifying formats] section of the [alert] topic of package [uj] (?dlg::alert).")}
  if (!ppp::.cmp_chr_scl(.d)) {Errors <- base::c(Errors, "[.d] must be a complete character scalar (?cmp_chr_scl).")}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "dlg")}
  Error <- .title == "ERROR"
  if (.clear) {
    base::gc(verbose = FALSE)
    base::cat("\014")
  }
  if (.title  != "") {base::cat("\n", fmt(.title , .ft), sep = "")}
  if (.sub    != "") {base::cat("\n", fmt(.sub   , .fs), sep = "")}
  if (Message != "") {base::cat("\n", fmt(Message, .fm), sep = "")}
  if (.ps     != "") {base::cat("\n", fmt(.ps    , .fp), sep = "")} else {cat("\n")}
  if (Error        ) {ppp::stopperr("", .pkg = "dlg")}
}

#' @rdname dialog
#' @export
acknowledge <- function(..., .sub = "", .ps = "", .ft = "r|w|b", .fs = "k|y|p", .fp = "k|y|i", .d = " ", .clear = FALSE) {
  dlg::alert(..., .title = "acknowledgment required", .sub = .sub, .ps = "press [return] or [enter] to continue", .ft = .ft, .fs = .fs, .fp = .fp, .d = .d, .clear = .clear)
  base::readline()
  NULL
}

#' @rdname dialog
#' @export
choose1 <- function(options, ..., .ft = "r|w|b", .fs = "k|y|p", .d = " ", .clear = FALSE) {
  if (!ppp::.unq_atm_vec(options)) {ppp::stopperr("[options] must be a unique atomic vec (?unq_atm_vec).", .pkg = "dlg")}
  Message <- base::paste0(uj::av(...), collapse = .d)
  dlg::cat_choose_list(options, Message, F, F, 1, 1, .ft, .fs, "choose1", uj::caller(), .clear)
}

#' @rdname dialog
#' @export
chooseN <- function(options, ..., .all = TRUE, .none = FALSE, .n = NULL, .min = NULL, .max = NULL, .ft = "r|w|b", .fs = "k|y|p", .d = " ", .clear = FALSE) {
  Errors <- NULL
  if (!ppp::.unq_atm_mvc(options)                              ) {Errors <- base::c(Errors, "[options] must be a unique atomic multivec (?unq_atm_mvc).")}
  if (!ppp::.cmp_lgl_scl(.all)                                 ) {Errors <- base::c(Errors, "[.all] must be TRUE or FALSE.")}
  if (!ppp::.cmp_lgl_scl(.none)                                ) {Errors <- base::c(Errors, "[.none] must be TRUE or FALSE.")}
  if (!uj::f0(base::is.null(.n   ), T, ppp::.cmp_psw_scl(.n  ))) {Errors <- base::c(Errors, "[.n] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(.min ), T, ppp::.cmp_psw_scl(.min))) {Errors <- base::c(Errors, "[.min] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!uj::f0(base::is.null(.max ), T, ppp::.cmp_psw_scl(.max))) {Errors <- base::c(Errors, "[.max] must be NULL or a complete positive whole-number scalar (?cmp_psw_scl).")}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "dlg")}
  DefMin   <- !base::is.null(.min)
  DefMax   <- !base::is.null(.max)
  DefN     <- !base::is.null(.n)
  Message  <- base::paste0(uj::av(...), collapse = .d)
  nOptions <- base::length(options)
  OkLo     <- uj::f0(!DefN, T, .n >= 1)
  OkHi     <- uj::f0(!DefN, T, .n <= nOptions)
  OkMin    <- uj::f0(!DefMin, T, .min <= nOptions)
  OkMax    <- uj::f0(!DefMax, T, .max <= nOptions)
  OkCombo  <- (!DefMin & !DefMax) | !DefN
  OkMinMax <- uj::f0(!DefMin | !DefMax, T, .min <= .max)
  if (!OkLo    ) {Errors <- base::c(Errors, "[.n] is out of bounds [min(.n) < 1].")}
  if (!OkHi    ) {Errors <- base::c(Errors, "[.n] is out of bounds [max(.n) > length(options)].")}
  if (!OkMin   ) {Errors <- base::c(Errors, "[.min] is out of bounds [.min > length(options)].")}
  if (!OkMax   ) {Errors <- base::c(Errors, "[.max] is out of bounds [.max > length(options)].")}
  if (!OkCombo ) {Errors <- base::c(Errors, "When [.n] is supplied, [.min] and [.max] must be NULL.")}
  if (!OkMinMax) {Errors <- base::c(Errors, "[.min] and [.max] are inconsistent [.min > .max].")}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "dlg")}
  .min <- uj::f0(DefN, .n, uj::f0(DefMin, .min, uj::f0(.none, 0, 1)))
  .max <- uj::f0(DefN, .n, uj::f0(DefMax, .max, nOptions))
  dlg::cat_choose_list(options, Message, .all, .none, .min, .max, .ft, .fs, "chooseN", uj::callers(), .clear)
}

#' @rdname dialog
#' @export
NO <- function(..., .ft = "r|w|b", .fs = "k|y|p", .d = " ", .clear = FALSE) {
  Message <- base::paste0(uj::av(...), collapse = .d)
  dlg::cat_choose_list(base::c("yes", "no"), Message, F, F, 1, 1, .ft, .fs, "NO", uj::callers(), .clear) == "no"
}

#' @rdname dialog
#' @export
YES <- function(..., .ft = "r|w|b", .fs = "k|y|p", .d = " ", .clear = FALSE) {
  Message <- base::paste0(uj::av(...), collapse = .d)
  dlg::cat_choose_list(base::c("yes", "no"), Message, F, F, 1, 1, .ft, .fs, "YES", uj::callers(), .clear) == "yes"
}

#' @rdname dialog
#' @export
OK <- function(..., .ft = "r|w|b", .fs = "k|y|p", .d = " ", .clear = FALSE) {
  Message <- base::paste0(uj::av(...), collapse = .d)
  dlg::cat_choose_list(base::c("ok", "cancel"), Message, F, F, 1, 1, .ft, .fs, "OK", uj::callers(), .clear, .CANCEL = F) == "ok"
}

#' @rdname dialog
#' @export
CANCEL <- function(..., .ft = "r|w|b", .fs = "k|y|p", .d = " ", .clear = FALSE) {
  Message <- base::paste0(uj::av(...), collapse = .d)
  dlg::cat_choose_list(base::c("ok", "cancel"), Message, F, F, 1, 1, .ft, .fs, "CANCEL", uj::callers(), .clear, .CANCEL = F) == "cancel"
}

#' @rdname dialog
#' @export
ask <- function(..., Default = "", .sub = "", .ft = "r|w|b", .fs = "k|y|p", .fm = "", .fp = "k|y|i", .d = " ", .clear = FALSE) {
  Message <- dlg::trm(..., .d = "")
  Message <- base::paste0(Message, "\n(enter '{cancel}' to cancel)")
  dlg::alert(Message, .title = "response required", .sub = .sub, .ps = "enter your response:", .ft = .ft, .fs = .fs, .fm = .fm, .fp = .fp, .d = .d, .clear = .clear)
  Answer <- base::readline()
  Cancel <- uj::f0(base::length == 0, T, Answer == "{cancel}")
  uj::f0(Cancel, ppp::stopperr("Action canceled by user.", .pkg = "dlg"), Answer)
}

#' @rdname dialog
#' @export
ask_new <- function(old, type = "replacement values", .u = TRUE, .sub = "", .ft = "r|w|b", .fs = "k|y|p", .fm = "", .fp = "k|y|i", .d = "|", .clear = FALSE) {
  Errors <- NULL
  if (!ppp::.unq_atm_vec(old )) {Errors <- base::c(Errors, "[old] must be a unique atomic vec (?unq_atm_vec).")}
  if (!ppp::.cmp_str_scl(type)) {Errors <- base::c(Errors, "[type] must be a complete string scalar (?cmp_str_scl).")}
  if (!ppp::.cmp_lgl_scl(.u  )) {Errors <- base::c(Errors, "[.u] must be TRUE or FALSE.")}
  if (!ppp::.cmp_ch1_scl(.d  )) {Errors <- base::c(Errors, "[.sep] must be a single character.")}
  if (!base::is.null(Errors  )) {ppp::stopperr(Errors, .pkg = "dlg")}
  .d <- uj::f0(.d == "|", "pipe (|)",
        uj::f0(.d == ".", "dot (.)",
        uj::f0(.d == ":", "colon (:)",
        uj::f0(.d == ";", "semicolon (;)",
        uj::f0(.d == "~", "tilde (~)",
        uj::f0(.d == "^", "caret (^)",
        uj::f0(.d == " ", "space",
        uj::f0(.d == "`", "backtick (`)", base::paste0("'", .d, "'")))))))))
  List     <- base::paste0(old, collapse = base::paste0(" ", .d, " "))
  Question <- base::paste0("Enter a %s separated list of %d %s for the following %s separated original values:\n\n  %s\n\n(enter '{cancel}' to cancel)\n")
  Question <- base::sprintf(Question, .d, base::length(old), type, .d, List)
  dlg::alert(Question, .title = "response required", .ps = "ENTER YOUR RESPONSE", .sub = "", .ft = .ft, .fs = .fs, .fm = .fm, .fp = .fp, .d = "", .clear = .clear)
  Answer   <- base::readline()
  Answer   <- uj::av(base::strsplit(Answer, .d, fixed = T))
  Answer   <- base::trimws(Answer, which = "both")
  Answer   <- Answer[Answer != ""]
  nAnswers <- uj::N(Answer)
  Cancel   <- uj::f0(nAnswers == 0, T, uj::f0(nAnswers > 1, F, Answer == "{cancel}"))
  if (Cancel                   ) {ppp::stopperr("Action cancelled by user.", .pkg = "dlg")}
  if (nAnswers != uj::N(old)   ) {ppp::stopperr("Numbers of old and new values do not match.", .pkg = "dlg")}
  if (.u & !uj::unq_vec(Answer)) {ppp::stopperr("Replacement values are not unique.", .pkg = "dlg")}
  Answer
}

#' @rdname dialog
#' @export
choose_dir <- function(dir.type = "directory", .sub = "", .ft = "r|w|b", .fs = "k|y|p", .fm = "", .fp = "k|y|i", .d = " ", .clear = FALSE) {
  if (!ppp::.cmp_chr_scl(dir.type)) {ppp::stopperr("[dir.type] must be a complete character scalar (?cmp_chr_scl)", .pkg = "dlg")}
  dlg::acknowledge(base::paste0("In the next dialog box, select a ", dir.type, "."), .sub = .sub, .ft = .ft, .fs = .fs, .fm = .fm, .fp = .fp, .d = .d, .clear = .clear)
  Path <- svDialogs::dlg_dir(.title = base::paste0("Select a ", dir.type, ":"))$res
  if (base::length(Path) == 0) {ppp::stopperr("Action canceled by user.", .pkg = "dlg")}
  Path
}

#' @rdname dialog
#' @export
choose_doc <- function(doc.type = "document", .sub = "", .ft = "r|w|b", .fs = "", .fm = "", .fp = "k|y|i", .d = " ", .clear = FALSE) {
  if (!ppp::.cmp_chr_scl(doc.type)) {ppp::stopperr("[doc.type] must be a complete character scalar (?cmp_chr_scl)", .pkg = "dlg", .clear = FALSE)}
  dlg::acknowledge(base::paste0("In the next dialog box, select a ", doc.type, "."), .sub = .sub, .ft = .ft, .fs = .fs, .fm = .fm, .fp = .fp, .d = .d, .clear = .clear)
  Path <- svDialogs::dlg_open(.title = base::paste0("Select a ", doc.type, ":"))$res
  if (base::length(Path) == 0) {ppp::stopperr("Action canceled by user.", .pkg = "dlg")}
  Path
}
