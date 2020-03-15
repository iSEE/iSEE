#' Set and get cached commons
#'
#' Get and set common cached information for each class.
#' The setter is usually called in \code{\link{.cacheCommonInfo}}
#' while the getter is usually called in \code{\link{.defineInterface}}.
#'
#' @param se A \linkS4class{SummarizedExperiment} object containing the current dataset.
#' @param cls String containing the name of the class for which this information is cached.
#' @param ... Any number of named R objects to cache.
#'
#' @return
#' \code{.setCachedCommonInfo} returns \code{se} with \code{...} added to its \code{\link{metadata}}.
#'
#' \code{.getCachedCommonInfo} retrieves the cached common information for class \code{cls}.
#'
#' @details
#' This function is intended for use by developers of \linkS4class{Panel} classes.
#' If you're an end-user and you're reading this, you probably took a wrong turn somewhere.
#'
#' @author Aaron Lun
#'
#' @examples
#' se <- SummarizedExperiment()
#' se <- .setCachedCommonInfo(se, "SomePanelClass",
#'     something=1, more_things=TRUE, something_else="A")
#' .getCachedCommonInfo(se, "SomePanelClass")
#'
#' @export
#' @rdname setCachedCommonInfo
#' @importFrom S4Vectors metadata metadata<-
.setCachedCommonInfo <- function(se, cls, ...) {
    if (is.null(metadata(se)$iSEE)) {
        metadata(se)$iSEE <- list()
    }
    metadata(se)$iSEE[[cls]] <- list(...)
    se
}

#' @export
#' @rdname setCachedCommonInfo
#' @importFrom S4Vectors metadata
.getCachedCommonInfo <- function(se, cls) {
    metadata(se)$iSEE[[cls]]
}

#' Set default slot values
#'
#' A utility function to set slots to default values if their values are not provided to \code{\link{initialize}} methods.
#'
#' @param args A named list of arguments to pass to the \code{initialize} method for a given class.
#' @param field String specifying the field to set.
#' @param default The default value of the slot in \code{field}.
#'
#' @details
#' A more natural approach would be to have the default values in the arguments of the \code{initialize} method.
#' However, this would require us to hard-code the slot names in the function signature,
#' which would break our current DRY model of only specifying the slot names once.
#'
#' @return
#' \code{args} is returned with the named \code{field} set to \code{default} if it was previously absent.
#'
#' @author Aaron Lun, Kevin Rue-Albrecht
#'
#' @name class-utils
#'
#' @export
#' @examples
#' showMethods("initialize", classes = "ReducedDimensionPlot", includeDefs = TRUE)
.emptyDefault <- function(args, field, default) {
    if (is.null(args[[field]])) {
        args[[field]] <- default
    }
    args
}

#' Find atomic fields
#'
#' A utility function to find column sin a data.frame or \linkS4class{DataFrame}
#' that are atomic R types, as most of the app does not know how to handle  more complex types being stored as columns.
#' An obvious example is in data.frames expected by \code{\link{ggplot}} or \code{\link{datatable}}.
#'
#' @param df A data.frame or \linkS4class{DataFrame}.
#'
#' @return A character vector of names of atomic fields in \code{df}.
#'
#' @author Aaron Lun, Kevin Rue-Albrecht
#'
#' @name dataframe-utils
#'
#' @export
#' @examples
#' x <- DataFrame(
#'     A = rnorm(10),
#'     B = sample(letters, 10),
#'     DataFrame = I(DataFrame(
#'         C = rnorm(10),
#'         D = sample(letters, 10)
#'     ))
#' )
#'
#' .findAtomicFields(x)
.findAtomicFields <- function(df) {
    covariates <- colnames(df)
    for (i in seq_along(covariates)) {
        current <- df[,i]
        if (!is.atomic(current) || !is.null(dim(current))) {
            covariates[i] <- NA_character_
        }
    }
    covariates[!is.na(covariates)]
}

#' Validation error utilites
#'
#' Helper functions to implement \code{\link{setValidity}} methods for \linkS4class{Panel} subclasses.
#'
#' @param msg Character vector containing the current error messages.
#' @param x An instance of a \linkS4class{Panel} subclass.
#' @param field String containing the name of the relevant slot under investigation.
#' @param fields Character vector containing the names of the relevant slots.
#' @param allowable Character vector of allowable choices for a multiple-choice selection.
#' @param lower Numeric scalar specifying the lower bound of possible values.
#' @param upper Numeric scalar specifying the upper bound of possible values.
#'
#' @return
#' All functions return \code{msg}, possibly appended with additional error messages.
#'
#' @details
#' \code{.single_string_error} adds an error message if any of the slots named in \code{fields} does not contain a single string.
#'
#' \code{.valid_string_error} adds an error message if any of the slots named in \code{fields} does not contain a single non-\code{NA} string.
#'
#' \code{.valid_logical_error} adds an error message if any of the slots named in \code{fields} does not contain a non-\code{NA} logical scalar.
#'
#' \code{.allowable_choice_error} adds an error message if the slot named \code{field} does not have a value in \code{allowable}, assuming it contains a single string.
#'
#' \code{.multiple_choice_error} adds an error message if the slot named \code{field} does not have all of its values in \code{allowable}, assuming it contains a character vector of any length.
#'
#' \code{.valid_number_error} adds an error message if the slot named \code{field} is not a non-\code{NA} number within [\code{lower}, \code{upper}].
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_validation_errors
.single_string_error <- function(msg, x, fields) {
    for (field in fields) {
        if (length(x[[field]]) != 1L) {
            msg <- c(msg, sprintf("'%s' should be a single string for '%s'", field, class(x)[1]))
        }
    }
    msg
}

#' @rdname INTERNAL_validation_errors
.valid_logical_error <- function(msg, x, fields) {
    for (field in fields) {
        if (length(val <- x[[field]])!=1 || is.na(val)) {
            msg <- c(msg, sprintf("'%s' should be a non-NA logical scalar for '%s'", field, class(x)[1]))
        }
    }
    msg
}

#' @rdname INTERNAL_validation_errors
.valid_string_error <- function(msg, x, fields) {
    for (field in fields) {
        if (length(val <- x[[field]])!=1 || is.na(val)) {
            msg <- c(msg, sprintf("'%s' should be a non-NA string for '%s'", field, class(x)[1]))
        }
    }
    msg
}

#' @rdname INTERNAL_validation_errors
.allowable_choice_error <- function(msg, x, field, allowable) {
    if (!x[[field]] %in% allowable) {
        msg <- c(msg, sprintf("'%s' for '%s' should be one of %s", field, class(x)[1],
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }
    msg
}

#' @rdname INTERNAL_validation_errors
.multiple_choice_error <- function(msg, x, field, allowable) {
    if (any(!x[[field]] %in% allowable)) {
        msg <- c(msg, sprintf("values of '%s' for '%s' should be in %s", field, class(x)[1],
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }
    msg
}

#' @rdname INTERNAL_validation_errors
.valid_number_error <- function(msg, x, field, lower, upper) {
    if (length(val <- x[[field]])!=1 || is.na(val) || val < lower || val > upper) {
        msg <- c(msg, sprintf("'%s' for '%s' should be a numeric scalar in [%s, %s]",
            field, class(x)[1], lower, upper))
    }
    msg
}

#' Replace with first choice
#'
#' Replace a \code{NA} value in a slot with the first valid choice.
#' This is usually called in \code{\link{.refineParameters}}.
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' @param field String containing the name of the relevant slot.
#' @param choices Character vector of permissible values for this slot.
#'
#' @return
#' \code{x} where the slot named \code{field} is replaced with \code{choices[1]} if it was previously \code{NA}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_replace_na_with_first
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
