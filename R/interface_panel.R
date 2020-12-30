width_limits <- c(2L, 12L)
height_limits <- c(400L, 1000L)

#' Generate the panel organization UI
#'
#' Generates the user interface to control the organization of the panels, specifically their sizes.
#'
#' @param all_memory A list of \linkS4class{Panel} objects specifying the current state of the application.
#'
#' @return
#' A HTML tag object containing the UI elements for panel sizing.
#'
#' @details
#' This function will create a series of UI elements for all active panels, specifying the width or height of the panels.
#' We use a select element for the width as this is very discrete, and we use a slider for the height.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_panel_organization
#' @seealso
#' \code{\link{.create_organization_observers}}, where this UI element is used.
#'
#' \code{\link{.panel_generation}}, which uses the set parameters.
#'
#' @importFrom shiny tagList selectInput sliderInput
#' @importFrom shinydashboard box
.panel_organization <- function(all_memory) {
    N <- length(all_memory)
    collected <- vector("list", N)
    counter <- 1L

    for (i in seq_len(N)) {
        instance <- all_memory[[i]]
        panel_name <- .getEncodedName(instance)
        prefix <- paste0(panel_name, "_")

        ctrl_panel <- box(
            selectInput(paste0(prefix, .organizationWidth), label="Width",
                choices=seq(width_limits[1], width_limits[2]), selected=slot(instance, .organizationWidth)),
            sliderInput(paste0(prefix, .organizationHeight), label="Height",
                min=height_limits[1], max=height_limits[2], value=slot(instance, .organizationHeight), step=10),
            title=.getFullName(instance), status="danger", width=NULL, solidHeader=TRUE
        )

        # Coercing to a different box status ('danger' is a placeholder, above).
        collected[[i]] <- .coerce_box_status(ctrl_panel, .encodedName(instance))
    }
    do.call(tagList, collected)
}

#' Generate the panels in the app body
#'
#' Constructs the interface and output elements of all panels in the main body of the app,
#' using the parameters in memory to set appropriate initial values for all widgets.
#'
#' @param all_memory A list of \linkS4class{Panel} objects specifying the current state of the application.
#' @param se A \linkS4class{SummarizedExperiment} object.
#'
#' @return
#' A HTML tag object containing the UI elements for the main body of the app.
#' This includes the output plots/tables as well as UI elements to control them.
#'
#' @details
#' This function generates the various panels in the main body of the app, taking into account their variable widths to dynamically assign them to particular rows.
#' It will try to assign as many panels to the same row until the row is filled, at which point it will start on the next row.
#'
#' Each panel contains the actual endpoint element (i.e., the plot or table to display) as well as a number of control elements to set the parameters.
#' All control elements lie within \code{\link{collapseBox}} elements to avoid cluttering the interface.
#' The open/closed status of these boxes are retrieved from memory, and are generally closed by default.
#'
#' Construction of each panel is done by retrieving all of the memorized parameters and using them to set the initial values of various control elements.
#' This ensures that the plots are not reset during re-rendering.
#' The exception is that of the Shiny brush, which cannot be fully restored in the current version - instead, only the bounding box is shown.
#'
#' Note that feature name selections will open up a \code{selectizeInput} where the values are filled on the server-side, rather than being sent to the client.
#' This avoids long start-up times during re-rendering.
#'
#' @author Aaron Lun
#' @seealso
#' \code{\link{.defineInterface}} and \code{\link{.defineOutput}}, for panel-specific definition of interface elements.
#'
#' @rdname INTERNAL_panel_generation
.panel_generation <- function(se, all_memory) {
    collected <- list()
    counter <- 1L
    cumulative.width <- 0L
    cur.row <- list()
    row.counter <- 1L

    # Getting all panel choices for single/multiselection links.
    all_names <- vapply(all_memory, .getEncodedName, "")
    names(all_names) <- vapply(all_memory, .getFullName, "")

    multi_sources <- .get_selection_sources(all_memory, all_names)
    single_sources <- .get_selection_sources(all_memory, all_names, multiple=FALSE)

    for (i in seq_along(all_memory)) {
        instance <- all_memory[[i]]
        plot_name <- .getEncodedName(instance)
        .input_FUN <- function(field) paste0(plot_name, "_", field)

        select_info <- list(
            single=lapply(single_sources, FUN=.setdiffWithNames, y=plot_name),
            multi=lapply(multi_sources, FUN=.setdiffWithNames, y=plot_name)
        )
        all.params <- .defineInterface(instance, se=se, select_info=select_info)
        param <- do.call(tags$div, c(list(class="panel-group", role="tablist"), all.params))

        # Deciding whether to continue on the current row, or start a new row.
        panel_width <- slot(instance, .organizationWidth)
        extra <- cumulative.width + panel_width
        if (extra > 12L) {
            collected[[counter]] <- do.call(fluidRow, cur.row)
            counter <- counter + 1L
            collected[[counter]] <- hr()
            counter <- counter + 1L
            cur.row <- list()
            row.counter <- 1L
            cumulative.width <- 0L
        }

        # Aggregating together everything into a box, and then into a column.
        cur_box <- box(
            .defineOutput(instance), 
            param,
            uiOutput(.input_FUN(.panelMultiSelectInfo)), 
            uiOutput(.input_FUN(.panelSelectLinkInfo)),
            title=.getFullName(instance),
            solidHeader=TRUE, width=NULL, status="danger"
        )

        cur_box <- .coerce_box_status(cur_box, .encodedName(instance))
        cur_box <- .insert_help_icon(cur_box, .input_FUN(.panelHelpTour))

        cur.row[[row.counter]] <- column(width=panel_width, cur_box, style='padding:3px;')
        row.counter <- row.counter + 1L
        cumulative.width <- cumulative.width + panel_width
    }

    # Cleaning up the leftovers.
    collected[[counter]] <- do.call(fluidRow, cur.row)
    counter <- counter + 1L
    collected[[counter]] <- hr()

    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, collected)
}

#' Coerce box status to custom classes
#'
#' Coerce the status of a \code{shinydashboard::box} to use a custom \pkg{iSEE} class.
#'
#' @param in_box A HTML tag object corresponding to a \code{box} object from the \pkg{shinydashboard} package.
#' @param mode String specifying the encoded panel type of the current plot.
#' @param old_status String specifying the current status of the \code{box}, to be replaced by \code{mode}.
#'
#' @return A modified \code{in_box} where the status is changed from \code{old_status} to \code{mode}.
#'
#' @details
#' The \code{\link[shinydashboard]{box}} function does not allow use of custom statuses.
#' As a result, we generate the box using the \code{"danger"} status, and replace it afterwards with our custom status.
#' This gives us full control over the box colours, necessary for proper colour-coding of each panel type.
#'
#' Note that the boxes from \pkg{shinydashboard} are used to enclose each plot/table panel in the \code{iSEE} app.
#' They do \emph{not} represent the parameter boxes, which are instead enclosed in Bootstrap panels (see \code{\link{collapseBox}}).
#'
#' @author Aaron Lun
#' @rdname INTERNAL_coerce_box_status
#' @seealso
#' \code{\link{.panel_organization}} and \code{\link{.panel_generation}}, which call this function.
#'
#' \code{\link{.define_box_statuses}}, to set up the Javascript classes for each custom status.
.coerce_box_status <- function(in_box, mode, old_status="danger") {
    in_box$children[[1]]$attribs$class <- sub(
        paste0("box-", old_status),
        paste0("box-", tolower(mode)),
        in_box$children[[1]]$attribs$class)

    in_box
}

.actionbutton_biocstyle <- "color: #ffffff; background-color: #0092AC; border-color: #2e6da4"

#' Insert a right-aligned help icon
#'
#' Insert the clickable icon for the self-help tour for each panel.
#'
#' @param in_box A HTML tag object corresponding to a \code{box} object from the \pkg{shinydashboard} package.
#' @param id A string containing the identifier for the element to be inserted.
#'
#' @return A modified \code{in_box} where a right-aligned help icon is inserted into the header.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_insert_help_icon
#' @importFrom shiny div
.insert_help_icon <- function(in_box, id) { 
    title_elements <- in_box$children[[1]]$children[[1]]$children
    title_elements <- c(title_elements,     
        list(div(id=id, style="display: inline-block; float: right;", icon("question-circle fa-1g")))
    )
    in_box$children[[1]]$children[[1]]$children <- title_elements
    in_box
}
