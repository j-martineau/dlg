% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\encoding{UTF-8}
\name{ssplit}
\alias{ssplit}
\title{Split Strings along Pipes and Trim Whitespace from Both Sides of All Resulting Elements}
\usage{
ssplit(x, d = "|")
}
\arguments{
\item{x}{An object of mode character.}

\item{d}{Character scalar text delimiter.}
}
\value{
A character vector
}
\description{
Split Strings along Pipes and Trim Whitespace from Both Sides of All Resulting Elements
}
\examples{
ssplit(" Just | a | little | thing ")
ssplit("  Just  |  a  |  little  |  thing  ")
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
\code{\link{g}()},
\code{\link{is_err}()},
\code{\link{match_bg}()},
\code{\link{ok_fmt}()},
\code{\link{p0}()},
\code{\link{trm}()}
}
\concept{utils}
