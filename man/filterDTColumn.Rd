% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filterDTColumn.R
\name{filterDTColumn}
\alias{filterDTColumn}
\alias{filterDT}
\title{Filter \pkg{DT} columns}
\usage{
filterDTColumn(x, search)

filterDT(df, column, global)
}
\arguments{
\item{x}{A numeric or character vector, usually representing a column of a data.frame.}

\item{search}{A string specifying the search filter to apply to \code{x}.}

\item{df}{A data.frame that was used in the \code{\link{datatable}} widget.}

\item{column}{A character vector of per-column search strings to apply to \code{df}.
If any entry is an empty string, the corresponding column is not used for any filtering.}

\item{global}{String containing a regular expression to search for across all columns in \code{df} (and row names, if present).
If an empty string, no filtering is performed.}
}
\value{
A logical vector indicating which entries of \code{x} or rows of \code{df} are to be retained.
}
\description{
Filter a data.frame based on the \pkg{DT} \code{\link{datatable}} widget column search string.
}
\details{
For character \code{x}, \code{search} is treated as a regular expression.

For numeric \code{x}, \code{search} should have the form \code{LOWER ... UPPER}
where all elements in [LOWER, UPPER] are retained.

For factor \code{x}, \code{search} should have the form \code{["choice_1", "choice_2", etc.]}.
This is also the case for logical \code{x}, albeit with the only choices being \code{"true"} or \code{"false"}.

\code{filterDT} will retain all rows where (i) any value in any column (after coercion to a string) matches \code{global},
and (ii) the value in each column satisfies the filter specified in the corresponding entry of \code{column}.
Setting \code{global} to an empty string will skip requirement (i) while 
setting any entry of \code{column} to an empty string will skip requirement (ii) for the affected column.

Ideally, \code{ncol(df)} and \code{length(searches)} would be the same, but if not,
\code{\link{filterDT}} will simply filter on the first N entries where N is the smaller of the two.

Any \code{NA} element in \code{x} will be treated as a no-match.
The same applies for each column of \code{df} that has non-empty \code{column}. 
Note that a no-match in one column does not preclude a successful match in another column by \code{global}.
}
\examples{
# Regular expression:
filterDTColumn(LETTERS, "A|B|C")

# Range query:
filterDTColumn(runif(20), "0.1 ... 0.5")

# Factor query:
filterDTColumn(factor(letters), "['a', 'b', 'c']")

# Works on DataFrames:
X <- data.frame(row.names=LETTERS, thing=runif(26), 
    stuff=sample(letters[1:3], 26, replace=TRUE),
    stringsAsFactors=FALSE)

filterDT(X, c("0 ... 0.5", "a|b"), global="")
filterDT(X, "", global="A")

}
\seealso{
\code{\link{datatable}} and associated documentation for more details about column searches.
}
\author{
Aaron Lun
}
