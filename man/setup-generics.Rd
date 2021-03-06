% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R
\name{setup-generics}
\alias{setup-generics}
\alias{.refineParameters}
\alias{.cacheCommonInfo}
\title{Generics for setting up parameters}
\description{
These generics are related to the initial setup of the \pkg{iSEE} application.
}
\section{Caching common information}{

\code{.cacheCommonInfo(x, se)} computes common values that can be re-used for all panels with the same class as \code{x}.
The following arguments are required:
\itemize{
\item \code{x}, an instance of a \linkS4class{Panel} class.
\item \code{se}, the \linkS4class{SummarizedExperiment} object containing the current dataset.
}

It is expected to return \code{se} with (optionally) extra fields added to \code{\link{int_metadata}(se)$iSEE}.
Each field should be named according to the class name and contain some common information that is constant for all instances of the class of \code{x} - see \code{\link{.setCachedCommonInfo}} for an appropriate setter utility.
The goal is to avoid repeated recomputation of required values when creating user interface elements or observers that respond to those elements.

Methods for this generic should start by checking whether the metadata already contains the class name, and returning \code{se} without modification if this is the case.
Otherwise, it should \code{\link{callNextMethod}} to fill in the cache values from the parent classes, before adding cached values under the class name for \code{x}.
This means that any modification to \code{se} will only be performed once per class, so any cached values should be constant for all instances of the same class.

Values from the cache can also be \code{\link{deparse}}d and used to assemble rendering commands in \code{\link{.generateOutput}}. 
However, those same commands should not make any use of the cache itself, i.e., they should not call \code{\link{.getCachedCommonInfo}}.
This is because the code tracker does not capture the code used to construct the cache, so the commands that are shown to the user will make use of a cache that is not present in the original \code{se} object.
}

\section{Refining parameters}{

\code{.refineParameters(x, se)} enforces appropriate settings for each parameter in \code{x}.
The following arguments are required:
\itemize{
\item \code{x}, an instance of a \linkS4class{Panel} class.
\item \code{se}, the \linkS4class{SummarizedExperiment} object containing the current dataset.
}

Methods for this generic should return a copy of \code{x} where slots with invalid values are replaced with appropriate entries from \code{se}.
This is necessary because the constructor and validity methods for \code{x} do not know about \code{se};
thus, certain slots (e.g., for the row/column names) cannot be set to a reasonable default or checked by the validity method.
By comparison, \code{\link{.refineParameters}} can catch and correct invalid values as it has access to \code{se}.

We recommend specializing \code{\link{initialize}} to fill any yet-to-be-determined slots with \code{NA} defaults.
\code{\link{.refineParameters}} can then be used to sweep across these slots and replace them with appropriate entries,
typically by using \code{\link{.getCachedCommonInfo}} to extract the cached set of potential valid values.
Of course, any slots that are not \code{se}-dependent should just be set at construction and checked by the validity method.

It is also possible for this generic to return \code{NULL}, which is used as an indicator that \code{se} does not contain information to meaningfully show any instance of the class of \code{x} in the \pkg{iSEE} app.
For example, the method for \linkS4class{ReducedDimensionPlot} will return \code{NULL} if \code{se} is not a \linkS4class{SingleCellExperiment} containing some dimensionality reduction results.
}

\author{
Aaron Lun
}
