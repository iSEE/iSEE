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

.empty_default <- function(x, field, newdef=NA) {
    if (length(x[[field]])==0L) {
        x[[field]] <- as(newdef, typeof(x[[field]]))
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

#' @importFrom S4Vectors isSingleString
.single_string_error <- function(msg, x, fields) {
    for (field in fields) {
        if (!isSingleString(x[[field]])) {
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

.transparency_error <- function(msg, x, field) {
    if (length(val <- x[[field]])!=1 || is.na(val) || val < 0 || val > 1) {
        msg <- c(msg, sprintf("'%s' for '%s' should be a numeric scalar in [0, 1]", field, class(x)[1]))
    }
    msg
}

.replace_na_with_first <- function(x, field, choices) {
    if (is.na(chosen <- x[[field]]) || !chosen %in% choices) {
        x[[field]] <- choices[1]
    }
    x
}
