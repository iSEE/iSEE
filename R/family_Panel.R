#' @export
setMethod("initialize", "Panel", function(.Object, ...) {
    .Object <- .empty_default(.Object, .organizationId)
    .Object <- .empty_default(.Object, .organizationHeight, 500L)
    .Object <- .empty_default(.Object, .organizationWidth, 4L)

    .Object <- .empty_default(.Object, .selectParamBoxOpen, FALSE)
    .Object <- .empty_default(.Object, .selectRowSource, .noSelection)
    .Object <- .empty_default(.Object, .selectColSource, .noSelection)

    .Object <- .empty_default(.Object, .selectRowType, .selectMultiActiveTitle)
    .Object <- .empty_default(.Object, .selectRowSaved, 0L)
    .Object <- .empty_default(.Object, .selectColType, .selectMultiActiveTitle)
    .Object <- .empty_default(.Object, .selectColSaved, 0L)

    .Object
})

setValidity2("Panel", function(object) {
    msg <- character(0)
    msg <- .valid_logical_error(msg, object, .selectParamBoxOpen)
    msg <- .single_string_error(msg, object, c(.selectRowSource, .selectColSource))

    msg <- .valid_numeric_error(msg, object, .organizationHeight, lower=400L, upper=1000L)
    msg <- .valid_numeric_error(msg, object, .organizationWidth, lower=1L, upper=12L)

    if (length(val <- object[[.organizationId]])!=1 || (!is.na(val) && val <= 0L)) {
        msg <- c(msg, sprintf("'%s' must be a positive integer or NA for '%s'", .organizationId, class(object)[1]))
    }

    for (field in c(.selectRowType, .selectColType)) {
        msg <- .allowable_choice_error(msg, object, field,
            c(.selectMultiActiveTitle, .selectMultiUnionTitle, .selectMultiSavedTitle))
    }

    for (field in c(.selectRowSaved, .selectColSaved)) {
        if (length(saved <- object[[field]]) > 1L || saved < 0L) {
            msg <- c(msg, sprintf("'%s' must be a non-negative integer in '%s'", field, class(object)[1]))
        }
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

#' @export
setMethod(".createParamObservers", "Panel", function(x, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) paste0(panel_name, "_", field)

    .safe_reactive_init(rObjects, panel_name)
    .safe_reactive_init(rObjects, .input_FUN(.panelLinkInfo))

    pObjects$selection_links <- .add_panel_vertex(pObjects$selection_links, panel_name) 
    pObjects$aesthetics_links <- .add_panel_vertex(pObjects$aesthetics_links, panel_name) 

    .define_child_propagation_observers(panel_name, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_selection_choice_observer(panel_name, by_field=.selectRowSource, 
        type_field=.selectRowType, saved_field=.selectRowSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_selection_choice_observer(panel_name, by_field=.selectColSource, 
        type_field=.selectColType, saved_field=.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_saved_selection_choice_observers(panel_name, by_field=.selectRowSource,
        type_field=.selectRowType, saved_field=.selectRowSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_saved_selection_choice_observers(panel_name, by_field=.selectColSource,
        type_field=.selectColType, saved_field=.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".hideInterfaceElement", "Panel", function(x, field) FALSE)

#' @export
setMethod(".restrictsSelection", "Panel", function(x) TRUE)

#' @export
setMethod(".transmittedDimension", "Panel", function(x) "none")
