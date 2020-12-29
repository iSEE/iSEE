#' The Panel virtual class
#'
#' The Panel is a virtual base class for all \pkg{iSEE} panels.
#' It provides slots and methods to control the height and width of each panel, as well as functionality to control the choice of \dQuote{transmitting} panels from which to receive a multiple row/column selection.
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
#' \item \code{RowSelectionDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source for multiple row selections.
#' Defaults to \code{FALSE}.
#' \item \code{RowSelectionRestrict}, a logical scalar indicating whether the display of \code{x} should be restricted to the rows in the multiple selection received from a transmitting panel.
#' Defaults to \code{FALSE}.
#' }
#'
#' The following slots are relevant to \emph{receiving} a multiple selection on the columns:
#' \itemize{
#' \item \code{ColumnSelectionSource}, a string specifying the name of the transmitting panel from which to receive a multiple column selection (e.g., \code{"ColumnDataPlot1"}).
#' Defaults to \code{"---"}.
#' \item \code{ColumnSelectionDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source for multiple column selections.
#' Defaults to \code{FALSE}.
#' \item \code{ColumnSelectionRestrict}, a logical scalar indicating whether the display of \code{x} should be restricted to the columns in the multiple selection received from a transmitting panel.
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
#' \item \code{VersionInfo}, a named list of \link{package_version} objects specifying the versions of packages used to create a given \linkS4class{Panel} instance.
#' This information is used to inform \code{\link{updateObject}} of any updates that need to be applied.
#' By default, it is filled with a single \code{"iSEE"} entry containing the current version of \pkg{iSEE}.
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
#' \item \code{\link{.refineParameters}(x, se)} calls \code{\link{updateObject}(x)}.
#' If \code{x} is up to date, this operation is a no-op and returns \code{x} without modification.
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
#' For documentation:
#' \itemize{
#' \item \code{\link{.definePanelTour}(x)} returns a data.frame containing the selection-related steps of the tour.
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
#' .defineSelectionEffectInterface,Panel-method
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
#' .definePanelTour,Panel-method
#' updateObject,Panel-method
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
    args <- .emptyDefault(args, .selectRowRestrict, FALSE)
    args <- .emptyDefault(args, .selectColRestrict, FALSE)

    args <- .emptyDefault(args, .selectRowDynamic, iSEEOptions$get("selection.dynamic.multiple"))
    args <- .emptyDefault(args, .selectColDynamic, iSEEOptions$get("selection.dynamic.multiple"))

    args <- .emptyDefault(args, .dataParamBoxOpen, FALSE)

    current <- c(.latest_version, args[[.packageVersion]])
    current <- current[!duplicated(names(current))]
    args[[.packageVersion]] <- current

    do.call(callNextMethod, c(list(.Object), args))
})

setValidity2("Panel", function(object) {
    msg <- character(0)

    msg <- .validLogicalError(msg, object, c(.selectParamBoxOpen, .dataParamBoxOpen,
        .selectRowDynamic, .selectColDynamic, .selectRowRestrict, .selectColRestrict))

    msg <- .singleStringError(msg, object, c(.selectRowSource, .selectColSource))

    msg <- .validNumberError(msg, object, .organizationHeight, lower=height_limits[1], upper=height_limits[2])
    msg <- .validNumberError(msg, object, .organizationWidth, lower=width_limits[1], upper=width_limits[2])

    if (length(val <- object[[.organizationId]])!=1 || (!is.na(val) && val <= 0L)) {
        msg <- c(msg, sprintf("'%s' must be a positive integer or NA for '%s'", .organizationId, class(object)[1]))
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
setMethod("[[", "Panel", function(x, i, j, ...) {
    if (i %in% c("ColumnSelectionType", "RowSelectionType", "ColumnSelectionSaved", "RowSelectionSaved")) {
        .Deprecated(msg=sprintf("<%s>[['%s']] is deprecated.", class(x)[1], i))
        if (i %in% c("ColumnSelectionType", "RowSelectionType")) {
            return(NA_character_)
        } else {
            return(NA_integer_)
        }
    }

    # Avoid having to call updateObject unnecessarily.
    out <- try(slot(x, i), silent=TRUE)

    if (is(out, "try-error")) {
        # nocov start
        x <- updateObject(x, check=FALSE)
        out <- slot(x, i)
        # nocov end
    }

    out
})

#' @export
setReplaceMethod("[[", "Panel", function(x, i, j, ..., value) {
    if (i %in% c("ColumnSelectionType", "RowSelectionType", "ColumnSelectionSaved", "RowSelectionSaved")) {
        .Deprecated(msg=sprintf("Setting <%s>[['%s']] is deprecated.", class(x)[1], i))
        return(x)
    }

    .attempt <- function(y) {
        slot(y, i) <- value
        if (iSEEOptions$get('.check.validity')) {
            validObject(y)
        }
        y
    }

    # Avoid having to call updateObject unnecessarily.
    out <- try(.attempt(x), silent=TRUE)

    if (is(out, "try-error")) {
        # nocov start
        x <- updateObject(x, check=FALSE)
        out <- .attempt(x)
        # nocov end
    }

    out 
})

#' @export
setMethod(".refineParameters", "Panel", function(x, se) {
    updateObject(x)
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
setMethod(".defineSelectionEffectInterface", "Panel", function(x) list())

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
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_choice_observer(panel_name, by_field=.selectColSource,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_restrict_observer(panel_name, by_field=.selectRowSource, res_field=.selectRowRestrict,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_restrict_observer(panel_name, by_field=.selectColSource, res_field=.selectColRestrict,
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
                input=input,
                session=session,
                pObjects=pObjects,
                rObjects=rObjects)

            if (!is.null(f$dynamic)) {
                .create_dynamic_single_selection_source_observer(panel_name, dyn_field=f$dynamic,
                    by_field=f$source, source_type=f$dimension,
                    input=input, session=session, pObjects=pObjects, rObjects=rObjects)
            }
        }
    }

    # nocov start
    if (!is.null(session)) {
        shinyjs::onclick(.input_FUN(.panelHelpTour), {
            ptour <- .definePanelTour(pObjects$memory[[panel_name]])
            if (nrow(ptour)) {
                introjs(session, options=list(steps=ptour))
            }
        })
    }
    # nocov end
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

#' @export
setMethod(".definePanelTour", "Panel", function(x) {
    collated <- list(
        .addTourStep(x, .selectParamBoxOpen, "Users can also control how this panel reacts to multiple selections being transmitted from other panels.<br/><br/><strong>Action:</strong> click on the header of this box to see the available options.")
    )

    for (mdim in c("row", "column")) {
        if (mdim=="row") {
            src_field <- .selectRowSource
            dyn_field <- .selectRowDynamic
            res_field <- .selectRowRestrict
        } else {
            src_field <- .selectColSource
            dyn_field <- .selectColDynamic
            res_field <- .selectColRestrict
        }

        collated <- c(collated, list(
            .addTourStep(x, src_field, paste0("PLACEHOLDER_", toupper(mdim), "_SELECT"), is_selectize=TRUE),
            .addTourStep(x, dyn_field, sprintf("Alternatively, we could turn on dynamic selection. This means that any selection in <emph>any</emph> %s-based panel would have an effect on this panel.", mdim)),
            .addTourStep(x, res_field, sprintf("Activating the <i>Restrict</i> mode will limit the display in the current panel to the %ss transmitted from the source panel.", mdim))
            )
        )
    }

    collated <- do.call(rbind, collated)
    data.frame(element=collated[,1], intro=collated[,2], stringsAsFactors=FALSE)
})

#' @export
#' @importFrom BiocGenerics updateObject
setMethod("updateObject", "Panel", function(object, ..., verbose=FALSE) {
    # NOTE: it is crucial that updateObject does not contain '[[' or '[[<-'
    # calls, lest we get sucked into infinite recursion with the calls to
    # 'updateObject' from '[['.

    # nocov start
    if (is(try(slot(object, .packageVersion), silent=TRUE), "try-error")) {
        .Deprecated(msg=sprintf("detected outdated '%s' instance, run 'updateObject(<%s>)'", class(object)[1], class(object)[1]))
        slot(object, .packageVersion) <- .latest_version 

        # Handling the updated restriction settings.
        slot(object, .selectRowRestrict) <- FALSE
        slot(object, .selectColRestrict) <- FALSE
    }
    object
    # nocov end
})
