#' The Panel virtual class
#'
#' The Panel is a virtual base class for all \pkg{iSEE} panels.
#' It provides slots and methods to control the height and width of each panel,
#' as well as to control the choice of transmitting panels from which to receive a multiple selection
#' (i.e., a selection of multiple rows or columns from a \linkS4class{SingleCellExperiment}).
#'
#' @section Slot overview:
#' The following slots are relevant to panel organization:
#' \itemize{
#' \item \code{PanelId}, an integer scalar specifying the identifier for the panel.
#' This should be unique across panels of the same concrete class.
#' \item \code{PanelWidth}, an integer scalar specifying the width of the panel.
#' Bootstrap coordinates are used so this value should lie between 2 and 12; defaults to 4.
#' \item \code{PanelHeight}, an integer scalar specifying the height of the panel in pixels.
#' This is expected to lie between 400 and 1000; defaults to 500.
#' }
#'
#' The following slots are relevant to \emph{receiving} a multiple selection on the rows:
#' \itemize{
#' \item \code{RowSelectionSource}, a string specifying the name of the transmitting panel from which to receive a multiple row selection (e.g., \code{"RowDataPlot1"}).
#' Defaults to \code{"---"}.
#' \item \code{RowSelectionType}, a string specifying which of the multiple row selections from the transmitting panel should be used.
#' Takes one of \code{"Active"}, only the active selection;
#' \code{"Union"}, the union of active and saved selections;
#' and \code{"Saved"}, one of the saved selections.
#' Defaults to \code{"Active"}.
#' \item \code{RowSelectionSaved}, an integer scalar specifying the index of the saved multiple row selection to use when \code{RowSelectionType="Saved"}.
#' Defaults to 0.
#' \item \code{RowSelectionDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source for multiple row selections.
#' Defaults to \code{FALSE}.
#' }
#'
#' The following slots are relevant to \emph{receiving} a multiple selection on the columns:
#' \itemize{
#' \item \code{ColumnSelectionSource}, a string specifying the name of the transmitting panel from which to receive a multiple column selection (e.g., \code{"ColumnDataPlot1"}).
#' Defaults to \code{"---"}.
#' \item \code{ColumnSelectionType}, a string specifying which of the column-based selections from the transmitting panel should be used.
#' Takes one of \code{"Active"}, only the active selection;
#' \code{"Union"}, the union of active and saved selections;
#' and \code{"Saved"}, one of the saved selections.
#' Defaults to \code{"Active"}.
#' \item \code{ColumnSelectionSaved}, an integer scalar specifying the index of the saved multiple column selection to use when \code{ColumnSelectionType="Saved"}.
#' Defaults to 0.
#' \item \code{ColumnSelectionDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source for multiple column selections.
#' Defaults to \code{FALSE}.
#' }
#'
#' There are also the following miscellaneous slots:
#' \itemize{
#' \item \code{SelectionBoxOpen}, a logical scalar indicating whether the selection parameter box should be open at initialization.
#' Defaults to \code{FALSE}.
#' \item \code{SelectionHistory}, a list of arbitrary elements that contain parameters for saved multiple selections.
#' Each element of this list corresponds to one saved selection in the current panel.
#' Defaults to an empty list.
#' }
#'
#' @section Getting and setting slots:
#' In all of the following code chunks, \code{x} is an instance of a Panel,
#' and \code{i} is a string containing the slot name:
#' \itemize{
#' \item \code{x[[i]]} returns the value of a slot named \code{i}.
#' \item \code{x[[i]] <- value} modifies \code{x} so that the value in slot \code{i} is replaced with \code{value}.
#' }
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{ColumnDotPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} is a no-op, returning \code{x} without modification.
#' \item \code{\link{.cacheCommonInfo}(x, se)} is a no-op, returning \code{se} without modification.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineInterface}(x, se, select_info)} will return a list of collapsible boxes for changing data and selection parameters.
#' The data parameter box will be populated based on \code{\link{.defineDataInterface}}.
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} will return an empty list.
#' \item \code{\link{.hideInterface}(x, field)} will always return \code{FALSE}.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} will add observers to respond to changes in multiple selection options.
#' It will also call \code{\link{.singleSelectionSlots}(x)} to set up observers for responding to transmitted single selections.
#' \item \code{\link{.renderOutput}(x, se, output, pObjects, rObjects)} will add elements to \code{output} for rendering the information textboxes at the bottom of each panel.
#' Each panel should specialize this method to add rendering expressions for the actual output (e.g., plots, tables),
#' followed by a \code{callNextMethod} to create the textboxes.
#' }
#'
#' For generating output:
#' \itemize{
#' \item \code{\link{.exportOutput}(x, se, all_memory, all_contents)} is a no-op,
#' i.e., it will return an empty character vector and create no files.
#' }
#'
#' For controlling selections:
#' \itemize{
#' \item \code{\link{.multiSelectionRestricted}(x)} will always return \code{TRUE}.
#' \item \code{\link{.multiSelectionDimension}(x)} will always return \code{"none"}.
#' \item \code{\link{.multiSelectionActive}(x)} will always return \code{NULL}.
#' \item \code{\link{.multiSelectionClear}(x)} will always return \code{x}.
#' \item \code{\link{.multiSelectionInvalidated}(x)} will always return \code{FALSE}.
#' \item \code{\link{.multiSelectionAvailable}(x, contents)} will return \code{nrow(contents)}.
#' \item \code{\link{.singleSelectionDimension}(x)} will always return \code{"none"}.
#' \item \code{\link{.singleSelectionValue}(x)} will always return \code{NULL}.
#' \item \code{\link{.singleSelectionSlots}(x)} will always return an empty list.
#' }
#'
#' @section Subclass expectations:
#' Subclasses are required to implement methods for:
#' \itemize{
#' \item \code{\link{.defineOutput}}
#' \item \code{\link{.generateOutput}}
#' \item \code{\link{.renderOutput}}
#' \item \code{\link{.fullName}}
#' \item \code{\link{.panelColor}}
#' }
#' Subclasses that transmit selections should also implement specialized methods for selection-related parameters listed above.
#'
#' @author Aaron Lun
#'
#' @seealso
#' \linkS4class{DotPlot} and \linkS4class{Table}, for examples of direct subclasses.
#'
#' @name Panel-class
#' @aliases
#' initialize,Panel-method
#' [[,Panel-method
#' [[<-,Panel-method
#' [[,Panel,ANY,ANY-method
#' [[<-,Panel,ANY,ANY-method
#' .defineInterface,Panel-method
#' .refineParameters,Panel-method
#' .cacheCommonInfo,Panel-method
#' .createObservers,Panel-method
#' .hideInterface,Panel-method
#' .defineDataInterface,Panel-method
#' .renderOutput,Panel-method
#' .exportOutput,Panel-method
#' .multiSelectionRestricted,Panel-method
#' .multiSelectionDimension,Panel-method
#' .multiSelectionClear,Panel-method
#' .multiSelectionActive,Panel-method
#' .multiSelectionInvalidated,Panel-method
#' .multiSelectionAvailable,Panel-method
#' .singleSelectionDimension,Panel-method
#' .singleSelectionValue,Panel-method
#' .singleSelectionSlots,Panel-method
NULL

#' @export
setMethod("initialize", "Panel", function(.Object, ...) {
    args <- list(...)

    args <- .emptyDefault(args, .organizationId, NA_integer_)
    args <- .emptyDefault(args, .organizationHeight, iSEEOptions$get("panel.height"))
    args <- .emptyDefault(args, .organizationWidth, iSEEOptions$get("panel.width"))

    args <- .emptyDefault(args, .selectParamBoxOpen, FALSE)
    args <- .emptyDefault(args, .selectRowSource, .noSelection)
    args <- .emptyDefault(args, .selectColSource, .noSelection)

    args <- .emptyDefault(args, .selectRowType, .selectMultiActiveTitle)
    args <- .emptyDefault(args, .selectRowSaved, 0L)
    args <- .emptyDefault(args, .selectColType, .selectMultiActiveTitle)
    args <- .emptyDefault(args, .selectColSaved, 0L)

    args <- .emptyDefault(args, .selectRowDynamic, iSEEOptions$get("selection.dynamic.multiple"))
    args <- .emptyDefault(args, .selectColDynamic, iSEEOptions$get("selection.dynamic.multiple"))

    args <- .emptyDefault(args, .dataParamBoxOpen, FALSE)

    do.call(callNextMethod, c(list(.Object), args))
})

setValidity2("Panel", function(object) {
    msg <- character(0)

    msg <- .valid_logical_error(msg, object, c(.selectParamBoxOpen, .dataParamBoxOpen,
        .selectRowDynamic, .selectColDynamic))

    msg <- .single_string_error(msg, object, c(.selectRowSource, .selectColSource))

    msg <- .valid_number_error(msg, object, .organizationHeight, lower=height_limits[1], upper=height_limits[2])
    msg <- .valid_number_error(msg, object, .organizationWidth, lower=width_limits[1], upper=width_limits[2])

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
setMethod(".refineParameters", "Panel", function(x, se) {
    x
})

#' @export
setMethod(".cacheCommonInfo", "Panel", function(x, se) {
    se
})

#' @export
setMethod(".defineInterface", "Panel", function(x, se, select_info) {
    list(
        .create_data_param_box(x, se, select_info),
        .create_selection_param_box(x, select_info$multi$row, select_info$multi$column)
    )
})

#' @export
setMethod(".defineDataInterface", "Panel", function(x, se, select_info) list())

#' @export
setMethod(".createObservers", "Panel", function(x, se, input, session, pObjects, rObjects) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) paste0(panel_name, "_", field)

    .safe_reactive_init(rObjects, panel_name)
    .safe_reactive_init(rObjects, .input_FUN(.flagSingleSelect))
    .safe_reactive_init(rObjects, .input_FUN(.flagMultiSelect))
    .safe_reactive_init(rObjects, .input_FUN(.flagRelinkedSelect))

    .create_box_observers(panel_name, c(.dataParamBoxOpen, .selectParamBoxOpen), pObjects, rObjects)

    .create_multi_selection_choice_observer(panel_name, by_field=.selectRowSource,
        type_field=.selectRowType, saved_field=.selectRowSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_choice_observer(panel_name, by_field=.selectColSource,
        type_field=.selectColType, saved_field=.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_type_observers(panel_name, by_field=.selectRowSource,
        type_field=.selectRowType, saved_field=.selectRowSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_type_observers(panel_name, by_field=.selectColSource,
        type_field=.selectColType, saved_field=.selectColSaved,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_history_observers(panel_name,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_dynamic_multi_selection_source_observer(panel_name,
        dyn_field=.selectRowDynamic, by_field=.selectRowSource, source_type="row",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_dynamic_multi_selection_source_observer(panel_name,
        dyn_field=.selectColDynamic, by_field=.selectColSource, source_type="column",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    for (f in .singleSelectionSlots(x)) {
        if ("dimension" %in% names(f)) {
            .create_dimname_observers(panel_name,
                name_field=f$parameter,
                choices=if (f$dimension=="sample") colnames(se) else rownames(se),
                use_mode_field=f$use_mode,
                use_value=f$use_value,
                tab_field=f$source,
                protected=f$protected,
                input=input, session=session, pObjects=pObjects, rObjects=rObjects)

            if (!is.null(f$dynamic)) {
                .create_dynamic_single_selection_source_observer(panel_name, dyn_field=f$dynamic,
                    by_field=f$source, source_type=f$dimension,
                    input=input, session=session, pObjects=pObjects, rObjects=rObjects)
            }
        }
    }
})

#' @export
setMethod(".renderOutput", "Panel", function(x, se, ..., output, pObjects, rObjects) {
    plot_name <- .getEncodedName(x)

    .create_selection_info_output(plot_name, se=se,
        output=output, pObjects=pObjects, rObjects=rObjects)

    .create_link_info_output(plot_name,
        output=output, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".exportOutput", "Panel", function(x, se, all_memory, all_contents) {
    character(0)
})

#' @export
setMethod(".hideInterface", "Panel", function(x, field) FALSE)

#' @export
setMethod(".multiSelectionRestricted", "Panel", function(x) TRUE)

#' @export
setMethod(".multiSelectionDimension", "Panel", function(x) "none")

#' @export
setMethod(".multiSelectionActive", "Panel", function(x) NULL)

#' @export
setMethod(".multiSelectionClear", "Panel", function(x) x)

#' @export
setMethod(".multiSelectionInvalidated", "Panel", function(x) FALSE)

#' @export
setMethod(".multiSelectionAvailable", "Panel", function(x, contents) nrow(contents))

#' @export
setMethod(".singleSelectionDimension", "Panel", function(x) "none")

#' @export
setMethod(".singleSelectionSlots", "Panel", function(x) list())
