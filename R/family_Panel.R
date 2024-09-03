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
#' Bootstrap coordinates are used so this value should lie between 2 and 12; defaults to 4 in \code{\link{getPanelDefault}}.
#' \item \code{PanelHeight}, an integer scalar specifying the height of the panel in pixels.
#' This is expected to lie between 400 and 1000; defaults to 500 in \code{\link{getPanelDefault}}.
#' }
#'
#' The following slots are relevant to \emph{receiving} a multiple selection on the rows:
#' \itemize{
#' \item \code{RowSelectionSource}, a string specifying the name of the transmitting panel from which to receive a multiple row selection (e.g., \code{"RowDataPlot1"}).
#' Defaults to \code{"---"}.
#' \item \code{RowSelectionDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source for multiple row selections.
#' Defaults to \code{FALSE} in \code{\link{getPanelDefault}}.
#' \item \code{RowSelectionRestrict}, a logical scalar indicating whether the display of \code{x} should be restricted to the rows in the multiple selection received from a transmitting panel.
#' Defaults to \code{FALSE}.
#' }
#'
#' The following slots are relevant to \emph{receiving} a multiple selection on the columns:
#' \itemize{
#' \item \code{ColumnSelectionSource}, a string specifying the name of the transmitting panel from which to receive a multiple column selection (e.g., \code{"ColumnDataPlot1"}).
#' Defaults to \code{"---"}.
#' \item \code{ColumnSelectionDynamicSource}, a logical scalar indicating whether \code{x} should dynamically change its selection source for multiple column selections.
#' Defaults to \code{FALSE} in \code{\link{getPanelDefault}}.
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
#' \item \code{show(x)} will print a summary of all (non-hidden) slots and their values.
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
#' show,Panel-method
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
#' .isBrushable,Panel-method
#' .singleSelectionDimension,Panel-method
#' .singleSelectionValue,Panel-method
#' .singleSelectionSlots,Panel-method
#' .definePanelTour,Panel-method
#' updateObject,Panel-method
NULL

#' @export
setMethod("initialize", "Panel", function(.Object, ..., RowSelectionSaved=NULL, ColumnSelectionSaved=NULL, RowSelectionType=NULL, ColumnSelectionType=NULL) {
    args <- list(...)

    args <- .emptyDefault(args, .organizationId, NA_integer_)
    args <- .emptyDefault(args, .organizationHeight, getPanelDefault("PanelHeight"))
    args <- .emptyDefault(args, .organizationWidth, getPanelDefault("PanelWidth"))

    args <- .emptyDefault(args, .selectParamBoxOpen, FALSE)
    args <- .emptyDefault(args, .selectRowSource, .noSelection)
    args <- .emptyDefault(args, .selectColumnSource, .noSelection)
    args <- .emptyDefault(args, .selectRowRestrict, FALSE)
    args <- .emptyDefault(args, .selectColumnRestrict, FALSE)

    args <- .emptyDefault(args, .selectRowDynamic, getPanelDefault("MultipleSelectionDynamicSource"))
    args <- .emptyDefault(args, .selectColumnDynamic, getPanelDefault("MultipleSelectionDynamicSource"))

    args <- .emptyDefault(args, .dataParamBoxOpen, FALSE)

    current <- c(.latest_version, args[[.packageVersion]])
    current <- current[!duplicated(names(current))]
    args[[.packageVersion]] <- current

    if (!is.null(RowSelectionSaved)) {
        .Deprecated(msg="'RowSelectionSaved=' is deprecated and will be ignored.")
    }
    if (!is.null(ColumnSelectionSaved)) {
        .Deprecated(msg="'ColumnSelectionSaved=' is deprecated and will be ignored.")
    }
    if (!is.null(RowSelectionType)) {
        .Deprecated(msg="'RowSelectionType=' is deprecated and will be ignored.")
    }
    if (!is.null(ColumnSelectionType)) {
        .Deprecated(msg="'ColumnSelectionType=' is deprecated and will be ignored.")
    }

    do.call(callNextMethod, c(list(.Object), args))
})

setValidity2("Panel", function(object) {
    msg <- character(0)

    msg <- .validLogicalError(msg, object, c(.selectParamBoxOpen, .dataParamBoxOpen,
        .selectRowDynamic, .selectColumnDynamic, .selectRowRestrict, .selectColumnRestrict))

    msg <- .singleStringError(msg, object, c(.selectRowSource, .selectColumnSource))

    msg <- .validNumberError(msg, object, .organizationHeight, lower=height_limits[1], upper=height_limits[2])
    msg <- .validNumberError(msg, object, .organizationWidth, lower=width_limits[1], upper=width_limits[2])

    val <- slot(object, .organizationId)
    if (length(val) !=1 || (!is.na(val) && val <= 0L)) {
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

check.validity.env <- new.env()
check.validity.env$check <- TRUE

#' @export
setReplaceMethod("[[", "Panel", function(x, i, j, ..., value) {
    if (i %in% c("ColumnSelectionType", "RowSelectionType", "ColumnSelectionSaved", "RowSelectionSaved")) {
        .Deprecated(msg=sprintf("Setting <%s>[['%s']] is deprecated.", class(x)[1], i))
        return(x)
    }

    # Avoid having to call updateObject unnecessarily when we're operating inside 
    # the app (and thus everything is known to be most up to date, given that
    # .refineParameters would have run the necessary updateObject already).
    check <- check.validity.env$check
    out <- try(.assign_and_check(x, i, value, check=check), silent=TRUE)

    if (is(out, "try-error")) {
        # nocov start
        x <- updateObject(x, check=FALSE)
        out <- .assign_and_check(x, i, value, check=check)
        # nocov end
    }

    out
})

.assign_and_check <- function(x, i, value, check) {
    slot(x, i) <- value
    if (check) {
        validObject(x)
    }
    x
}

#' @export
setMethod("show", "Panel", function(object) {
    cat("Panel object of class", paste0(class(object)[1], "\n"))
    cat("  Get or set individual parameters with", sQuote('[['), '\n')
    cat("  Available parameters:\n")

    all.slots <- sort(slotNames(object))
    for (x in all.slots) {
        if (.hideInterface(object, x)) {
            next
        }
        cat(paste0("    ", x, ": "))

        val <- slot(object, x)
        if (is.atomic(val)) {
            if (length(val) > 5) {
                extra <- sprintf("... + %i more", length(val) - 3)
                val <- head(val, 3)
            } else {
                extra <- NULL
            }

            if (is.character(val)) {
                # All strings greater than hard_limit are truncated to soft_limit with an added "...".
                # All strings containing a newline or tab are similarly truncated at that character.
                if (any(edit <- grepl("[\t\n]", val), na.rm=TRUE)) {
                    val[edit] <- sub("[\t\n].*", "...", val[edit])
                }
                if (any(edit <- (nchar(val) > 60), na.rm=TRUE)) {
                    val[edit] <- paste0(substr(val[edit], 1, 50), "...")
                }
            }

            val <- c(val, extra)
            wrapped <- strwrap(paste(val, collapse=" "), exdent=6)
            cat(paste(wrapped, collapse="\n"))
        } else if (is.list(val)) {
            if (length(val)) {
                cat("list of length", length(val))
            }
        } else {
            cat("a", class(val)[1], "object")
        }
        cat("\n")
    }
})

#' @export
setMethod(".refineParameters", "Panel", function(x, se) {
    updateObject(x)
})

#' @export
setMethod(".cacheCommonInfo", "Panel", function(x, se) {
    se
})

###############################################################################

#' @export
setMethod(".defineInterface", "Panel", function(x, se, select_info) {
    list(
        do.call(.collapseBoxHidden,
            c(
                list(x=x, field=.dataParamBoxOpen, title="Data parameters"),
                open=slot(x, .dataParamBoxOpen),
                .defineDataInterface(x, se, select_info)
            )
        ),
        .create_selection_param_box(x, select_info$multi$row, select_info$multi$column)
    )
})

#' Multiple selection parameter box
#'
#' Create a multiple selection parameter box for a given instance of a \linkS4class{Panel}.
#'
#' @param x A \linkS4class{Panel} object.
#' @param row_selectable A character vector of names for available panels that can transmit a row selection.
#' @param col_selectable A character vector of names for available panels that can transmit a column selection.
#' @param selectable A character vector of decoded names for available transmitting panels.
#' @param source_type String specifying the type of the panel that is source of the selection,
#' either \code{"row"} or \code{"column"}.
#' @param by_field String specifying the name of the slot containing the identity of the panel transmitting to \code{x}.
#' @param res_field String specifying the name of the slot indicating whether to restrict \code{x}'s display to the selected points.
#' @param dyn_field String specifying the name of the slot indicating whether to use a dynamic selection source.
#'
#' @return
#' For \code{.create_selection_param_box} and \code{.create_dotplot_selection_param_box},
#' a HTML tag object is returned containing a parameter box of UI elements for changing multiple selection parameters.
#' The latter will also contain elements to control the visual effects of the transmitted selection for \linkS4class{DotPlot}s.
#'
#' For \code{.define_selection_choices}, a tag list of interface elements is returned to choose the identity of transmitting panel,
#' the type of multiple selection and the index of the saved selection to use.
#'
#' All return values may potentially also be \code{NULL}, depending on \code{\link{.hideInterface}}.
#'
#' @details
#' These functions are used to create a collapsible box that contains point selection options,
#' initialized with the choices in \code{memory}.
#' Options include the choice of transmitting plot and the type of selection effect.
#' Each effect option, once selected, may yield a further subset of nested options.
#' For example, choosing to colour on the selected points will open up a choice of colour to use.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_create_selection_param_box
#' @seealso
#' \code{\link{.defineInterface}}, where this function is typically called.
#'
#' @importFrom shiny selectInput actionButton hr strong br
#' @importFrom shinyjs disabled
.create_selection_param_box <- function(x, row_selectable, col_selectable) {
    # initialize active "Delete" button only if a preconfigured selection history exists
    deleteFUN <- identity
    deleteLabel <- .buttonDeleteLabel
    if (length(slot(x, .multiSelectHistory)) == 0L) {
        deleteFUN <- disabled
        deleteLabel <- .buttonEmptyHistoryLabel
    }

    # initialize active "Save" button only if a preconfigured active selection exists
    saveFUN <- identity
    saveLabel <- .buttonSaveLabel
    if (!.multiSelectionHasActive(x)) {
        saveFUN <- disabled
        saveLabel <- .buttonNoSelectionLabel
    }

    args <- list(
        x=x,
        field=.selectParamBoxOpen,
        title="Selection parameters",
        open=slot(x, .selectParamBoxOpen),

        .define_selection_choices(x, by_field=.selectRowSource,
            dyn_field=.selectRowDynamic, res_field=.selectRowRestrict,
            selectable=row_selectable, "row"),

        .define_selection_choices(x, by_field=.selectColumnSource,
            dyn_field=.selectColumnDynamic, res_field=.selectColumnRestrict,
            selectable=col_selectable, "column")
    )

    if (!.hideInterface(x, .multiSelectHistory)) {
        .addSpecificTour(class(x), .multiSelectHistory, {
            mdim <- .multiSelectionDimension(x)
            function(panel_name) {
                data.frame(
                    rbind(
                        c(
                            element=paste0("#", panel_name, "_", .multiSelectSave),
                            intro=sprintf("Users can save the multiple %s selections made in this panel.
When this button is clicked, any \"active\" selection is saved for later use.
(For example, in point-based plotting panels, the current brush or lasso is the active selection.)
The identities of the %ss in the saved and active selections can then be transmitted to other panels,
which is useful if there are separate groups of %ss of interest;
each group can be represented by a separate saved selection.", mdim, mdim, mdim)
                        ),
                        c(
                            element=paste0("#", panel_name, "_", .multiSelectDelete),
                            intro="Users can also delete any saved selections.
For simplicity, this operates on a first-in-last-out basis, i.e., you can only delete the last saved selection."
                        )
                    )
                )
            }
        })

        panel_name <- .getEncodedName(x)
        args <- c(args,
            list(
                hr(),
                strong(.label_with_help("Manage multiple selections:", paste0(panel_name, "_", .multiSelectHistory))),
                br(),
                saveFUN(actionButton(paste0(panel_name, "_", .multiSelectSave), label=saveLabel)),
                deleteFUN(actionButton(paste0(panel_name, "_", .multiSelectDelete), label=deleteLabel))
            )
        )
    }

    do.call(.collapseBoxHidden, args)
}

#' @rdname INTERNAL_create_selection_param_box
#' @importFrom shiny tagList radioButtons selectizeInput
.define_selection_choices <- function(x, by_field,
    dyn_field, res_field, selectable, source_type="row")
{
    force(source_type)

    .addSpecificTour(class(x), by_field, function(panel_name) {
        tour_df <- data.frame(
            element=paste0("#", panel_name, "_", by_field, " + .selectize-control"),
            intro=sprintf("One of <strong>iSEE</strong>'s most powerful features is the ability to transmit multiple %s selections from one panel to another.
For example, if we have another panel that visualizes each %s as a point, and we created a brush or lasso on that panel, we can transmit the identity of the selected %ss to this panel.
This enables intuitive interactive exploration of multi-dimensional data involving different variables in our <code>SummarizedExperiment</code> object.
<br/><br/>
Here, we can choose the \"source\" panel to receive a multiple %s selection from, i.e., the selection made in the chosen panel will be transmitted to the current panel.
The exact effect of receiving a selection will depend on how the current panel takes advantage of the identity of the transmitted points.
For example, point-based panels might allow users to color, facet, or group points by whether or not they are selected in the source panel.",
                source_type, source_type, source_type, source_type)
            )
        # Some panel classes (e.g. ComplexHeatmapPlot) do not have the checkbox to restrict to selected rows
        if (!.hideInterface(x, res_field)) {
            tour_df <- rbind(tour_df,
                c(
                    element=paste0("#", panel_name, "_", res_field),
                    intro=sprintf("One obvious effect would be to restrict the dataset to only those %ss in the transmitted selection.
This is achieved by clicking this box, in which case the current panel will only use the subset of selected %ss for visualization and computation.
Note that no restriction is performed if no multiple selection was made in the source panel;
for example, a point-based panel that does not contain a lasso or brush will not be considered to have made any selection,
and if that panel was chosen as the source, it would have no effect on the current panel.",
                        source_type, source_type)
                    )
                )
        }

        tour_df <- rbind(tour_df,
            c(
                    element=paste0("#", panel_name, "_", dyn_field),
                    intro=sprintf("Sometimes it's a bother to have to change the choice of source panel.
If this option is checked, the source panel will change dynamically in response to <em>any</em> multiple %s selection made in any panel.
For example, creating a brush or lasso in another plot will automatically transmit the selected points to the current panel,
regardless of whether the brushed plot was chosen as the source panel.
This is useful for allowing the current panel to immediately respond to any interactions elsewhere in the <strong>iSEE</strong> application.",
                        source_type)
                )
            )

        tour_df
    })

    tagList(
        .selectInput.iSEE(
            x=x, field=by_field,
            label=sprintf("Receive %s selection from:", source_type),
            choices=selectable,
            selected=.choose_link(slot(x, by_field), selectable)
        ),

        .checkboxInput.iSEE(x, field=dyn_field,
            label=paste("Use dynamic", source_type, "selection"),
            value=slot(x, dyn_field),
            help=FALSE
        ),

        .checkboxInput.iSEE(x, field=res_field,
            label=paste0("Restrict to selected ", source_type, "s"),
            value=slot(x, res_field),
            help=FALSE
        )
    )
}

#' @export
setMethod(".defineDataInterface", "Panel", function(x, se, select_info) list())

###############################################################################

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

    .create_multi_selection_choice_observer(panel_name, by_field=.selectColumnSource,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_restrict_observer(panel_name, by_field=.selectRowSource, res_field=.selectRowRestrict,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_restrict_observer(panel_name, by_field=.selectColumnSource, res_field=.selectColumnRestrict,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_multi_selection_history_observers(panel_name,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_dynamic_multi_selection_source_observer(panel_name,
        dyn_field=.selectRowDynamic, by_field=.selectRowSource, source_type="row",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .create_dynamic_multi_selection_source_observer(panel_name,
        dyn_field=.selectColumnDynamic, by_field=.selectColumnSource, source_type="column",
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

        # We only run this _AFTER_ rendering is done, as the identities of the
        # tour-enabled elements are only defined in the interface methods.
        cls <- class(x)
        observeEvent(rObjects$rerendered, {
            tours <- .getSpecificTours(cls)
            for (i in names(tours)) {
                local({
                    i0 <- i
                    shinyjs::onclick(paste0(panel_name, "_", i0, "_specific_help"), {
                        spec.df <- tours[[i0]](panel_name)
                        introjs(session, options=list(steps=spec.df))
                    })
                })
            }
        }, once=TRUE)
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
setMethod(".isBrushable", "Panel", function(x) FALSE)

#' @export
setMethod(".singleSelectionDimension", "Panel", function(x) "none")

#' @export
setMethod(".singleSelectionSlots", "Panel", function(x) list())

###############################################################################

#' @export
setMethod(".definePanelTour", "Panel", function(x) {
    collated <- list(
        .addTourStep(x, .selectParamBoxOpen, "Users can also control how this panel reacts to multiple selections being transmitted from other panels.<br/><br/><strong>Action:</strong> click on the header of this box to see the available options.")
    )
    collated <- do.call(rbind, collated)
    data.frame(element=collated[,1], intro=collated[,2], stringsAsFactors=FALSE)
})

###############################################################################

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
        slot(object, .selectColumnRestrict) <- FALSE
    }
    object
    # nocov end
})
