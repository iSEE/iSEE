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
#' @param type_field String specifying the name of the slot containing the type of multiple selection to use in \code{x}.
#' @param saved_field String specifying the name of the slot containing the index of the saved selection to use in \code{x}.
#' @param dyn_field String specifying the name of the slot indicating whether to use a dynamic selection source.
#'
#' @return
#' For \code{.create_selection_param_box} and \code{.create_dotplot_selection_param_box},
#' a HTML tag object is returned containing a parameter box of UI elements for changing multiple selection parameters.
#' The latter will also contain elements to control the visual effects of the transmitted selection for \linkS4class{DotPlot}s.
#'
#' For \code{.define_selection_transmitter}, an interface element is returned for selecting the transmitting panel.
#' For \code{.define_selection_choices}, a tag list of interface elements is returned to choose the identity of transmitting panel, the type of multiple selection and the index of the saved selection to use.
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
#' \code{\link{.defineSelectEffectInterface}}, to control the interface for additional selection effects.
#'
#' @importFrom shiny selectInput actionButton hr strong br
#' @importFrom shinyjs disabled
.create_selection_param_box <- function(x, row_selectable, col_selectable) {
    # initialize active "Delete" button only if a preconfigured selection history exists
    deleteFUN <- identity
    deleteLabel <- .buttonDeleteLabel
    if (length(x[[.multiSelectHistory]]) == 0L) {
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
        open=x[[.selectParamBoxOpen]],

        .define_selection_choices(x, by_field=.selectRowSource,
            type_field=.selectRowType, saved_field=.selectRowSaved,
            dyn_field=.selectRowDynamic, selectable=row_selectable, "row"),

        .define_selection_choices(x, by_field=.selectColSource,
            type_field=.selectColType, saved_field=.selectColSaved,
            dyn_field=.selectColDynamic, selectable=col_selectable, "column"),
    )

    args <- c(args, .defineSelectEffectInterface(x)) 

    if (!.hideInterface(x, .multiSelectHistory)) {
        panel_name <- .getEncodedName(x)
        args <- c(args,
            list(
                hr(),
                strong("Manage multiple selections:"),
                br(),
                saveFUN(actionButton(paste0(panel_name, "_", .multiSelectSave), label=saveLabel)),
                deleteFUN(actionButton(paste0(panel_name, "_", .multiSelectDelete), label=deleteLabel))
            )
        )
    }

    do.call(.collapseBoxHidden, args)
}

#' @rdname INTERNAL_create_selection_param_box
#' @importFrom shiny selectInput
.define_selection_transmitter <- function(x, by_field, selectable, source_type="row") {
    .selectInputHidden(
        x=x, field=by_field,
        label=sprintf("Receive %s selection from:", source_type),
        choices=selectable,
        selected=.choose_link(x[[by_field]], selectable)
    )
}

#' @rdname INTERNAL_create_selection_param_box
#' @importFrom shiny tagList radioButtons selectizeInput
.define_selection_choices <- function(x, by_field, type_field,
    saved_field, dyn_field, selectable, source_type="row")
{
    select_type <- paste0(.getEncodedName(x), "_", type_field)

    tagList(
        .define_selection_transmitter(x, by_field, selectable, source_type),

        .checkboxInputHidden(x, field=dyn_field,
            label=paste("Use dynamic", source_type, "selection"),
            value=x[[dyn_field]]),

        .radioButtonsHidden(
            x, field=type_field, label=NULL, inline=TRUE,
            choices=c(.selectMultiActiveTitle, .selectMultiUnionTitle, .selectMultiSavedTitle),
            selected=x[[type_field]]
        ),

        .conditional_on_radio(
            select_type, .selectMultiSavedTitle,
            .selectizeInputHidden(
                x, field=saved_field,
                label=NULL, selected=NULL, choices=NULL, multiple=FALSE
            )
        )
    )
}
