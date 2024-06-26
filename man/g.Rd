% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\encoding{UTF-8}
\name{g}
\alias{g}
\title{Glue \link[=av]{Atomized} \code{...} Arg Atomic Elements into a Character Scalar}
\usage{
g(g, ...)
}
\arguments{
\item{g}{Character scalar glue to use as delimiter.}

\item{...}{Objects to be \link[=av]{atomized}.}
}
\value{
A character scalar.
}
\description{
Flattens \code{...} to a character vector and glues the resulting elements using \code{g} as the glue.
}
\seealso{
Other utils: 
\code{\link{N}()},
\code{\link{av}()},
\code{\link{choose_from}()},
\code{\link{f0}()},
\code{\link{fmt_errs}()},
\code{\link{fmt_stack}()},
\code{\link{fs_t}()},
\code{\link{g0}()},
\code{\link{glu}()},
\code{\link{is_err}()},
\code{\link{match_bg}()},
\code{\link{ok_fmt}()},
\code{\link{p0}()},
\code{\link{ssplit}()},
\code{\link{trm}()}
}
\concept{utils}
