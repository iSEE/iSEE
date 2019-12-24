#' @importFrom S4Vectors metadata metadata<-
.set_common_info <- function(se, cls, ...) {
    if (is.null(metadata(se)$iSEE)) {
        metadata(se)$iSEE <- list()
    }
    metadata(se)$iSEE[[cls]] <- list(...)
    se
}

#' @importFrom S4Vectors metadata
.get_common_info <- function(se, cls) {
    metadata(se)$iSEE[[cls]]
}

.empty_default <- function(x, field, default) {
    if (is.null(x[[field]])) {
        x[[field]] <- default 
    }
    x
}

.find_atomic_fields <- function(df) {
    covariates <- colnames(df)
    for (i in seq_along(covariates)) {
        current <- df[,i]
        if (!is.atomic(current) || !is.null(dim(current))) {
            covariates[i] <- NA_character_
        }
    }
    covariates[!is.na(covariates)]
}

.single_string_error <- function(msg, x, fields) {
    for (field in fields) {
        if (length(x[[field]]) != 1L) {
            msg <- c(msg, sprintf("'%s' should be a single string for '%s'", field, class(x)[1]))
        }
    }
    msg
}

.valid_logical_error <- function(msg, x, fields) {
    for (field in fields) {
        if (length(val <- x[[field]])!=1 || is.na(val)) {
            msg <- c(msg, sprintf("'%s' should be a non-NA logical scalar for '%s'", field, class(x)[1]))
        }
    }
    msg
}

.valid_string_error <- function(msg, x, fields) {
    for (field in fields) {
        if (length(val <- x[[field]])!=1 || is.na(val)) {
            msg <- c(msg, sprintf("'%s' should be a non-NA string for '%s'", field, class(x)[1]))
        }
    }
    msg
}

.allowable_choice_error <- function(msg, x, field, allowable) {
    if (!x[[field]] %in% allowable) {
        msg <- c(msg, sprintf("'%s' for '%s' should be one of %s", field, class(x)[1],
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }
    msg
}

.multiple_choice_error <- function(msg, x, field, allowable) {
    if (any(!x[[field]] %in% allowable)) {
        msg <- c(msg, sprintf("values of '%s' for '%s' should be in %s", field, class(x)[1],
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }
    msg
}

.valid_number_error <- function(msg, x, field, lower, upper) {
    if (length(val <- x[[field]])!=1 || is.na(val) || val < lower || val > upper) {
        msg <- c(msg, sprintf("'%s' for '%s' should be a numeric scalar in [%s, %s]", 
            field, class(x)[1], lower, upper))
    }
    msg
}

.replace_na_with_first <- function(x, field, choices) {
    if (is.na(chosen <- x[[field]]) || !chosen %in% choices) {
        x[[field]] <- choices[1]
    }
    x
}

#' Number of levels for any data type
#'
#' @param x An atomic vector.
#'
#' @return Numeric scalar specifying the number of unique levels in \code{x}.
#' This is \code{Inf} for numeric \code{x}
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_nlevels
#' @seealso
#' \code{\link{nlevels}},
#' \code{\link{unique}}.
.nlevels <- function(x){
    if (is.numeric(x)){
        Inf
    } else if (is.factor(x)) {
        nlevels(x)
    } else {
        length(unique(x))
    }
}

#' Determine whether a vector is categorical
#'
#' This function is used to eliminate non-numeric variables with very large numbers of levels.
#' Otherwise, plotting functions that attempt to show too many variables (e.g., in the legend, or in the facets) will freeze.
#'
#' @param x An atomic vector.
#' @param max_levels Integer scalar specifying the maximum number unique values for \code{x} to be categorical.
#'
#' @return A logical scalar that indicates whether \code{x} has no more than than \code{max_levels} unique values.
#'
#' @author Kevin Rue-Albrecht
#' @rdname INTERNAL_is_groupable
#' @seealso
#' \code{\link{.nlevels}}.
.is_groupable <- function(x, max_levels = getOption("iSEE.maxlevels", 24)){
    .nlevels(x) <= max_levels
}

#' Identify categorical columns
#'
#' Identify categorical columns that can be used as options in various interface elements, e.g., for faceting or shaping.
#' This is typically called in \code{\link{.cacheCommonInfo}} for later use by methods of \code{\link{.defineInterface}}.
#' 
#' @param x A DataFrame (or equivalent).
#'
#' @return An integer vector containing the indices of the categorical columns.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_groupable
.which_groupable <- function(x) {
    which(vapply(x, FUN=.is_groupable, FUN.VALUE=FALSE))
}

#' Identify numeric columns
#'
#' Identify continuous columns that can be used as options in various interface elements, e.g., for sizing.
#' This is typically called in \code{\link{.cacheCommonInfo}} for later use by methods of \code{\link{.defineInterface}}.
#'
#' @param x A \linkS4class{DataFrame} or data.frame.
#'
#' @return An integer vector containing the indices of the numeric columns.
#'
#' @author Charlotte Soneson
#'
#' @rdname INTERNAL_numeric
.which_numeric <- function(x) {
    which(vapply(x, FUN=is.numeric, FUN.VALUE=FALSE))
}
