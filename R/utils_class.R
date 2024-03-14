#' Set and get cached commons
#'
#' Get and set common cached information for each class.
#' The setter should only ever be called in \code{\link{.cacheCommonInfo}}.
#' The getter can be called anywhere but most usually in \code{\link{.defineInterface}}.
#'
#' @param se A \linkS4class{SummarizedExperiment} object containing the current dataset.
#' @param cls String containing the name of the class for which this information is cached.
#' @param ... Any number of named R objects to cache.
#'
#' @return
#' \code{.setCachedCommonInfo} returns \code{se} with \code{...} added to its \code{\link{int_metadata}}.
#'
#' \code{.getCachedCommonInfo} retrieves the cached common information for class \code{cls}.
#'
#' @author Aaron Lun
#'
#' @examples
#' se <- SummarizedExperiment()
#' se <- .setCachedCommonInfo(se, "SomePanelClass",
#'     something=1, more_things=TRUE, something_else="A")
#' .getCachedCommonInfo(se, "SomePanelClass")
#'
#' @seealso
#' \code{?"\link{cache-utils}"}, for utilities to define some cached variables.
#'
#' @export
#' @rdname setCachedCommonInfo
#' @importFrom S4Vectors metadata metadata<-
.setCachedCommonInfo <- function(se, cls, ...) {
    metadata(se) <- .set_nested_list(metadata(se), c("iSEE", "cached", cls), list(...))
    se
}

#' @export
#' @rdname setCachedCommonInfo
#' @importFrom S4Vectors metadata
.getCachedCommonInfo <- function(se, cls) {
    metadata(se)[["iSEE"]][["cached"]][[cls]]
}

.set_nested_list <- function(x, i, value) {
    if (!is.list(x)) {
        x <- list()
    }
    if (length(i)==1L) {
        x[[i]] <- value
    } else {
        i1 <- i[1]
        x[[i1]] <- .set_nested_list(x[[i1]], i[-1], value)
    }
    x 
}

#' Dedicated colormap getters/setters
#'
#' Store/retrieve the \linkS4class{ExperimentColorMap} in the \linkS4class{SummarizedExperiment} using the caching machinery.
#'
#' @param se A \linkS4class{SummarizedExperiment} object containing the current dataset.
#' @param colormap A \linkS4class{ExperimentColorMap} object.
#'
#' @return 
#' \code{.set_colormap} returns \code{se} with \code{colormap} cached in its metadata.
#'
#' \code{.get_colormap} returns \code{colormap} from its metadata cache in \code{se}.
#'
#' @author Aaron Lun
#' 
#' @rdname INTERNAL_set_colormap
.set_colormap <- function(se, colormap) {
    .setCachedCommonInfo(se, ".internal", colormap=colormap)
}

#' @rdname INTERNAL_set_colormap
.get_colormap <- function(se) {
    .getCachedCommonInfo(se, ".internal")$colormap
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

#' Caching utilities
#'
#' Utility functions to be used in a \code{\link{.cacheCommonInfo}} method, usually to identify names of elements of the \linkS4class{SummarizedExperiment} for later use in \code{\link{.defineInterface}} to populate the user interface.
#'
#' @param x A data.frame or \linkS4class{DataFrame}, most typically the \code{\link{rowData}} or \code{\link{colData}}.
#' @param se The \linkS4class{SummarizedExperiment} object.
#' @param i An integer scalar or string specifying the assay of interest in \code{se}.
#' @param max_levels Integer scalar specifying the maximum number unique values for \code{x} to be categorical.
#'
#' @details
#' \code{.findAtomicFields} is necessary as many of the widgets used by \code{\link{iSEE}} (e.g., \code{\link{ggplot}}, \code{\link{datatable}}) do not know how to handle more complex types being stored as columns.
#' Similarly, \code{.whichNumeric} and \code{.whichGroupable} can be used to specify options for visualization modes that only make sense for continuous or discrete variables respectively (e.g., sizing, faceting).
#'
#' @return 
#' For \code{.findAtomicFields}, a character vector of names of columns in \code{x} containing atomic R types.
#'
#' For \code{.whichNumeric}, an integer vector containing the indices of the numeric columns.
#'
#' For \code{.whichGroupable}, an integer vector containing the indices of the categorical columns.
#'
#' For \code{.isAssayNumeric}, a logical scalar indicating whether the specified assay as numeric.
#'
#' @author Aaron Lun, Kevin Rue-Albrecht, Charlotte Soneson
#'
#' @name cache-utils
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
#' .whichGroupable(x)
#' .whichNumeric(x)
.findAtomicFields <- function(x) {
    covariates <- colnames(x)
    for (i in seq_along(covariates)) {
        current <- x[,i]
        if (!is.atomic(current) || !is.null(dim(current))) {
            covariates[i] <- NA_character_
        }
    }
    covariates[!is.na(covariates)]
}

#' Validation error utilities
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
#' All functions return a character vector containing \code{msg}, possibly appended with additional error messages.
#'
#' @details
#' \code{.singleStringError} adds an error message if any of the slots named in \code{fields} does not contain a single string.
#'
#' \code{.validStringError} adds an error message if any of the slots named in \code{fields} does not contain a single non-\code{NA} string.
#'
#' \code{.validLogicalError} adds an error message if any of the slots named in \code{fields} does not contain a non-\code{NA} logical scalar.
#'
#' \code{.allowableChoiceError} adds an error message if the slot named \code{field} does not have a value in \code{allowable}, assuming it contains a single string.
#'
#' \code{.multipleChoiceError} adds an error message if the slot named \code{field} does not have all of its values in \code{allowable}, assuming it contains a character vector of any length.
#'
#' \code{.validNumberError} adds an error message if the slot named \code{field} is not a non-\code{NA} number within [\code{lower}, \code{upper}].
#'
#' @author Aaron Lun
#'
#' @export
#' @name validate-utils
.singleStringError <- function(msg, x, fields) {
    for (field in fields) {
        if (length(slot(x, field)) != 1L) {
            msg <- c(msg, sprintf("'%s' should be a single string for '%s'", field, class(x)[1]))
        }
    }
    msg
}

#' @export
#' @rdname validate-utils
.validLogicalError <- function(msg, x, fields) {
    for (field in fields) {
        val <- slot(x, field)
        if (length(val)!=1 || is.na(val)) {
            msg <- c(msg, sprintf("'%s' should be a non-NA logical scalar for '%s'", field, class(x)[1]))
        }
    }
    msg
}

#' @export
#' @rdname validate-utils
.validStringError <- function(msg, x, fields) {
    for (field in fields) {
        val <- slot(x, field)
        if (length(val)!=1 || is.na(val)) {
            msg <- c(msg, sprintf("'%s' should be a non-NA string for '%s'", field, class(x)[1]))
        }
    }
    msg
}

#' @export
#' @rdname validate-utils
.allowableChoiceError <- function(msg, x, field, allowable) {
    if (!slot(x, field) %in% allowable) {
        msg <- c(msg, sprintf("'%s' for '%s' should be one of %s", field, class(x)[1],
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }
    msg
}

#' @export
#' @rdname validate-utils
.multipleChoiceError <- function(msg, x, field, allowable) {
    if (any(!slot(x, field) %in% allowable)) {
        msg <- c(msg, sprintf("values of '%s' for '%s' should be in %s", field, class(x)[1],
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }
    msg
}

#' @export
#' @rdname validate-utils
.validNumberError <- function(msg, x, field, lower, upper) {
    val <- slot(x, field)
    if (length(val)!=1 || is.na(val) || val < lower || val > upper) {
        msg <- c(msg, sprintf("'%s' for '%s' should be a numeric scalar in [%s, %s]",
            field, class(x)[1], lower, upper))
    }
    msg
}

#' Replace with first choice
#'
#' Replace an \code{NA} or invalid value in a slot of a \linkS4class{Panel} object with the first valid choice.
#' This is usually called in \code{\link{.refineParameters}}.
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' @param field String containing the name of the relevant slot.
#' @param choices Character vector of permissible values for this slot.
#'
#' @return
#' \code{x} where the slot named \code{field} is replaced with \code{choices[1]} if its value was previously \code{NA} or did not exist in \code{choices}.
#'
#' @author Aaron Lun
#' @export
#' @rdname replaceMissingWithFirst
.replaceMissingWithFirst <- function(x, field, choices) {
    chosen <- slot(x, field)
    if (is.na(chosen) || !chosen %in% choices) {
        if (is.null(choices)) {
            choices <- vector(typeof(chosen), 0) 
        }
        slot(x, field) <- choices[1]
    }
    x
}

#' Remove invalid values in multiple choices
#'
#' Removes invalid values in a slot of a \linkS4class{Panel} object.
#' This is usually called in \code{\link{.refineParameters}}.
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' @param field String containing the name of the relevant slot.
#' @param choices Character vector of permissible values for this slot.
#'
#' @return
#' \code{x} where the slot named \code{field} is replaced only with the values that exist in \code{choices}.
#'
#' @author Kevin Rue-Albrecht
#' @export
#' @rdname removeInvalidChoices
.removeInvalidChoices <- function(x, field, choices) {
    chosen <- slot(x, field)
    if (any(!chosen %in% choices)) {
        removed <- setdiff(chosen, choices)
        warning(sprintf("Removing invalid values of '%s' for '%s': %s", field, class(x)[1],
            paste(sprintf("'%s'", removed), collapse=", ")))
        slot(x, field) <- intersect(chosen, choices)
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
.is_groupable <- function(x, max_levels = Inf) {
    out <- .nlevels(x)
    is.finite(out) && out <= max_levels
}

#' @rdname cache-utils
#' @export
.whichGroupable <- function(x, max_levels = Inf) {
    which(vapply(x, FUN=.is_groupable, max_levels=max_levels, FUN.VALUE=FALSE))
}

#' @rdname cache-utils
#' @export
.whichNumeric <- function(x) {
    which(vapply(x, FUN=is.numeric, FUN.VALUE=FALSE))
}

#' @export
#' @rdname cache-utils
.isAssayNumeric <- function(se, i) {
    # The as.matrix is necessary to account for non-ordinary matrices.
    is.numeric(as.matrix(assay(se, i)[0, 0]))
}

#' Check the Panel version
#'
#' Is the \linkS4class{Panel} generated by the latest version of \pkg{iSEE}?
#' 
#' @param x A \linkS4class{Panel} instance.
#'
#' @return Logical scalar answering the question above.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_is_latest_version
.is_latest_version <- function(x) {
    v <- try(slot(x, .packageVersion), silent=TRUE)
    !is(v, "try-error") && identical(v$iSEE, .latest_version$iSEE)
}
