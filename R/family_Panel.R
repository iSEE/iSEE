#' @export
setMethod("initialize", "Panel", function(.Object, ...) {
    .Object <- .empty_default(.Object, .organizationId)
    .Object <- .empty_default(.Object, .organizationHeight, 500L)
    .Object <- .empty_default(.Object, .organizationWidth, 4L)

    .Object <- .empty_default(.Object, .selectParamBoxOpen, FALSE)
    .Object <- .empty_default(.Object, .selectByPlot, .noSelection)

    .Object <- .empty_default(.Object, .selectMultiType, .selectMultiActiveTitle)
    .Object <- .empty_default(.Object, .selectMultiSaved, 0L)

    .Object
})

setValidity2("Panel", function(object) {
    msg <- character(0)
    msg <- .valid_logical_error(msg, object, .selectParamBoxOpen)
    msg <- .single_string_error(msg, object, .selectByPlot)

    msg <- .valid_numeric_error(msg, object, .organizationHeight, lower=400L, upper=1000L)
    msg <- .valid_numeric_error(msg, object, .organizationWidth, lower=1L, upper=12L)

    if (length(val <- object[[.organizationId]])!=1 || (!is.na(val) && val <= 0L)) {
        msg <- c(msg, sprintf("'%s' must be a positive integer or NA for '%s'", .organizationId, class(object)[1]))
    }

    msg <- .allowable_choice_error(msg, object, .selectMultiType,
        c(.selectMultiActiveTitle, .selectMultiUnionTitle, .selectMultiSavedTitle))

    if (length(saved <- object[[.selectMultiSaved]]) > 1L || saved < 0L) {
        msg <- c(msg, sprintf("'%s' must be a non-negative integer in '%s'", .selectMultiSaved, class(object)[1]))
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod("[[", "Panel", function(x, i, j, ...) {
    slot(x, i)        
})

#' @export
setReplaceMethod("[[", "Panel", function(x, i, j, ..., value) {
    slot(x, i) <- value
    x
})

#' @export
setMethod("[", "Panel", function(x, i, j, ..., drop=FALSE) {
    x[[j]]
})

#' @export
setReplaceMethod("[", "Panel", function(x, i, j, ..., value) {
    x[[j]] <- value
    x
})

#' @export
setMethod(".refineParameters", "Panel", function(x, se) {
    x
})

#' @export
setMethod(".cacheCommonInfo", "Panel", function(x, se) {
    se
})

setMethod(".createParamObservers", "Panel", function(x, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)
    input_FUN <- function(field) paste0(panel_name, "_", field)

    .safe_reactive_bump(rObjects, .input_FUN(.panelLinkInfo))

    width_name <- input_FUN(.organizationWidth)
    observeEvent(input[[width_name]], {
        copy <- pObjects$memory_copy[[panel_name]]
        cur.width <- copy[[.organizationWidth]]
        new.width <- as.integer(input[[width_name]])
        if (!isTRUE(all.equal(new.width, cur.width))) {
            pObjects$memory_copy[[panel_name]][[.organizationWidth]] <- new.width
        }
    })

    height_name <- input_FUN(.organizationHeight)
    observeEvent(input[[height_name]], {
        copy <- pObjects$memory_copy[[panel_name]]
        cur.height <- copy[[.organizationWidth]]
        new.height <- as.integer(input[[height_name]])
        if (!isTRUE(all.equal(new.height, cur.height))) {
            pObjects$memory_copy[[panel_name]][[.organizationHeight]] <- new.height
        }
    })

    .define_child_propagation_observers(panel_name, session=session, pObjects=pObjects, rObjects=rObjects)
})
