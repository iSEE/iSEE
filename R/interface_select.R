#' Point selection parameter box
#'
#' Create a point selection parameter box for all point-based plots.
#'
#' @param mode String specifying the encoded panel type of the current plot.
#' @param id Integer scalar specifying the index of a panel of the specified type, for the current plot.
#' @param param_choices A DataFrame with one row, containing the parameter choices for the current plot.
#' @param selectable A character vector of decoded names for available transmitting panels.
#' @param source_type Type of the panel that is source of the selection. Either \code{"row"} or \code{"column"}.
#' @param ... Additional arguments passed to \code{\link{collapseBox}}.
#' @param field Column name in the DataFrame of parameters choices for the current plot.
#'
#' @return
#' For \code{.create_selection_param_box} and \code{.create_selection_param_box_define_box},
#' a HTML tag object containing a \code{\link{collapseBox}} with UI elements for changing point selection parameters.
#'
#' For \code{.create_selection_param_box_define_choices}, a HTML tag object containing a \code{selectInput} for choosing the transmitting panels.
#'
#' @details
#' The \code{.create_selection_param_box} function creates a collapsible box that contains point selection options, initialized with the choices in \code{memory}.
#' Options include the choice of transmitting plot and the type of selection effect.
#' Each effect option, once selected, may yield a further subset of nested options.
#' For example, choosing to colour on the selected points will open up a choice of colour to use.
#'
#' The other three functions are helper functions that avoid re-writing related code in the \code{\link{.panel_generation}} function.
#' This is mostly for other panel types that take selections but do not follow the exact structure produced by \code{.create_selection_param_box}.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_create_selection_param_box
#' @seealso
#' \code{\link{.panel_generation}}
#'
#' @importFrom shiny selectInput actionButton hr strong br
#' @importFrom shinyjs disabled
.create_selection_param_box <- function(mode, id, param_choices, row_selectable, col_selectable, ...) {
    # initialize active "Delete" button only if a preconfigured selection history exists
    deleteFUN <- identity
    deleteLabel <- .buttonDeleteLabel
    if (length(param_choices[[.multiSelectHistory]]) == 0L) {
        deleteFUN <- disabled
        deleteLabel <- .buttonEmptyHistoryLabel
    }

    # initialize active "Save" button only if a preconfigured active selection exists
    saveFUN <- identity
    saveLabel <- .buttonSaveLabel
    if (!.multiSelectionHasActive(param_choices)) {
        saveFUN <- disabled
        saveLabel <- .buttonNoSelectionLabel
    }

    args <- list(
        param_choices=param_choices,

        .define_selection_choices(param_choices, by_field=.selectRowSource,
            type_field=.selectRowType, saved_field=.selectRowSaved, 
            selectable=row_selectable, "row"),

        .define_selection_choices(param_choices, by_field=.selectColSource, 
            type_field=.selectColType, saved_field=.selectColSaved, 
            selectable=col_selectable, "column"),

        ...
    )

    if (!.hideInterfaceElement(param_choices, .multiSelectHistory)) {
        args <- c(args,
            list(
                hr(),
                strong("Manage multiple selections:"),
                br(),
                saveFUN(actionButton(paste0(mode, id, "_", .multiSelectSave), label=saveLabel)),
                deleteFUN(actionButton(paste0(mode, id, "_", .multiSelectDelete), label=deleteLabel))
            )
        )
    }

    do.call(.define_selection_param_box, args)
}

#' @importFrom colourpicker colourInput
#' @importFrom shiny sliderInput
.create_dotplot_selection_param_box <- function(mode, id, param_choices, row_selectable, col_selectable) {
    select_effect <- paste0(mode, id, "_", .selectEffect)

    .create_selection_param_box(mode, id, param_choices, row_selectable, col_selectable,
        .radioButtonsHidden(param_choices, field=select_effect, 
            label="Selection effect:", inline=TRUE,
            choices=c(.selectRestrictTitle, .selectColorTitle, .selectTransTitle),
            selected=param_choices[[.selectEffect]]),

        .conditional_on_radio(
            select_effect, .selectColorTitle,
            colourInput(
                paste0(mode, id, "_", .selectColor), label=NULL,
                value=param_choices[[.selectColor]])
        ),
        .conditional_on_radio(
            select_effect, .selectTransTitle,
            sliderInput(
                paste0(mode, id, "_", .selectTransAlpha), label=NULL,
                min=0, max=1, value=param_choices[[.selectTransAlpha]])
        )
    )
}

#' @rdname INTERNAL_create_selection_param_box
.define_selection_param_box <- function(param_choices, ...) {
    .collapseBoxHidden(
        x=param_choices, field=.selectParamBoxOpen,
        title="Selection parameters",
        open=param_choices[[.selectParamBoxOpen]],
        ...)
}

#' @rdname INTERNAL_create_selection_param_box
#' @importFrom shiny selectInput
.define_selection_transmitter <- function(param_choices, field, selectable, source_type="row") {
    .selectInputHidden(
        x=param_choices, field=field, 
        label=sprintf("Receive %s selection from:", source_type),
        choices=selectable,
        selected=.choose_link(param_choices[[field]], selectable)
    )
}

#' @rdname INTERNAL_create_selection_param_box
#' @importFrom shiny tagList radioButtons selectizeInput
.define_selection_choices <- function(param_choices, by_field, type_field, 
    saved_field, selectable, source_type="row") 
{
    select_type <- paste0(
        .getEncodedName(param_choices),
        param_choices[[.organizationId]],
        "_", type_field
    )

    tagList(
        .define_selection_transmitter(param_choices, by_field, selectable, source_type),

        .radioButtonsHidden(
            param_choices, field=type_field, label=NULL, inline=TRUE,
            choices=c(.selectMultiActiveTitle, .selectMultiUnionTitle, .selectMultiSavedTitle),
            selected=param_choices[[type_field]]
        ),

        .conditional_on_radio(
            select_type, .selectMultiSavedTitle,
            .selectizeInputHidden(
                param_choices, field=saved_field, 
                label=NULL, selected=NULL, choices=NULL, multiple=FALSE
            )
        )
    )
}
