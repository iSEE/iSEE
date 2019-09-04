
#' Number of levels for any data type
#'
#' @param x A \code{vector} of any R internal type.
#'
#' @return The length of \code{levels(x)}, or the count of unique values in
#' \code{x}.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_nlevels
#' @seealso
#' \code{\link{nlevels}},
#' \code{\link{unique}}.
.nlevels <- function(x){
  # numeric covariates are defined to have infinite levels
  if (is.numeric(x)){
    return(Inf)
  }
  # default answer for factors
  if (is.factor(x)){
    return(nlevels(x))
  }
  # default answer for character would be NULL
  return(length(unique(x)))
}

#' Determine whether a vector is discrete
#'
#' This function requires a threshold on the count of unique values
#' beyond which the vector is declared as continuous and coerced to numeric
#' values.
#'
#' @param x A \code{vector} of any R internal type.
#' @param max_levels Maximum number of levels or unique values beyond which \code{x} is declared to be continuous (i.e., not groupable).
#'
#' @return A \code{logical} that indicates whether \code{x} has fewer than \code{max_levels} levels or unique values.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_is_groupable
#' @seealso
#' \code{\link{.nlevels}}.
.is_groupable <- function(x, max_levels = getOption("iSEE.maxlevels", 24)){
  return(.nlevels(x) <= max_levels)
}

#' Identify groupable covariates in a DataFrame
#'
#' @param x A DataFrame (or equivalent).
#'
#' @return An integer vector containing the indices of the groupable covariates.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_groupable
#' @seealso
#' \code{\link{.add_general_parameters}}
.which_groupable <- function(x) {
    if (identical(ncol(x), 0L)) {
        return(integer(0L))
    }
    which(vapply(x, FUN=.is_groupable, FUN.VALUE=FALSE))
}

#' Identify numeric covariates in a DataFrame
#'
#' @param x A DataFrame (or equivalent).
#'
#' @return An integer vector containing the indices of the numeric covariates.
#'
#' @author Charlotte Soneson
#'
#' @rdname INTERNAL_numeric
#' @seealso
#' \code{\link{.add_general_parameters}}
.which_numeric <- function(x) {
    if (identical(ncol(x), 0L)) {
        return(integer(0L))
    }
    which(vapply(x, FUN=is.numeric, FUN.VALUE=FALSE))
}
