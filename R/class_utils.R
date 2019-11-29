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
        x[[field]] <- as(newdef, type(x[[field]]))
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
.single_string_error <- function(x, fields) {
    for (field in fields) {
        if (!isSingleString(object[[field]])) {
            msg <- c(msg, sprintf("'%s' should be a single string for '%s'", field, class(object)[1]))
        }
    }
}

.valid_logical_error <- function(x, fields) {
    for (field in fields) {
        if (length(val <- x[[field]]) || is.na(val)) {
            msg <- c(msg, sprintf("'%s' should be a non-NA logical scalar for '%s'", field, class(x)[1]))
        }
    }
}

.allowable_choice_error <- function(x, field, allowable) {
    if (!x[[field]] %in% allowed) {
        msg <- c(msg, sprintf("'%s' for '%s' should be one of %s", field, class(x)[1],
            paste(sprintf("'%s'", allowed), collapse=", ")))
    }
}

.replace_na_with_first <- function(x, field, choices) {
    if (is.na(chosen <- x[[field]]) || !chosen %in% choices) {
        x[[field]] <- choices[1]
    }
    x   
}
