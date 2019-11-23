#' Create reduced dimension plot parameter UI
#'
#' Create the parameter selection UI elements for the reduced dimension plot panel type.
#'
#' @param se A \linkS4class{SingleCellExperiment} object with precomputed UI information from \code{\link{.precompute_UI_info}}.
#' @param active_panels A data.frame specifying the currently active panels, see the output of \code{\link{.setup_initial}}.
#' @param mode String specifying the encoded panel type (e.g., \code{"redDimPlot"}).
#' @param id Integer specifying the identity of the panel.
#' @param param_choices A \linkS4class{DataFrame} with one row containing the parameter choices for the current plot.
#'
#' @return 
#' A list of standardized UI elements for parameter selection in any reduced dimension plot.
#'
#' @author Aaron Lun
#'
#' @importFrom SingleCellExperiment reducedDim reducedDimNames
#' @importFrom shiny selectInput
.create_redDimPlot_parameter_ui <- function(mode, id, param_choices, se, active_panels) {
    cur_reddim <- param_choices[[.redDimType]]
    max_dim <- ncol(reducedDim(se, cur_reddim))
    choices <- seq_len(max_dim)
    names(choices) <- choices

    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    plot.param <- list(
        selectInput(.input_FUN(.redDimType), label="Type",
            choices=.get_internal_info(se, "red_dim_names"), selected=cur_reddim),
        selectInput(.input_FUN(.redDimXAxis), label="Dimension 1",
            choices=choices, selected=param_choices[[.redDimXAxis]]),
        selectInput(.input_FUN(.redDimYAxis), label="Dimension 2",
            choices=choices, selected=param_choices[[.redDimYAxis]])
    )
       
    do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=param_choices[[.dataParamBoxOpen]]), plot.param))
}

#' Define reduced dimension plot observers
#'
#' Define a series of observers to track changes to the standard parameters for a given reduced dimension plot panel.
#'
#' @param mode String specifying the encoded panel type - this should be \code{"redDimPlot"}.
#' @param id Integer specifying the index of the current panel.
#' @param input The Shiny input object from the server function.
#' @param output The Shiny output object from the server function.
#' @param session The Shiny session object from the server function.
#' @param pObjects An environment containing global parameters generated in the \code{\link{iSEE}} app.
#' @param rObjects A reactive list of values generated in the \code{\link{iSEE}} app.
#'
#' @return
#' Observers pertaining to standard parameters for a reduced dimension plot are created.
#' A \code{NULL} is invisibly returned.
#'
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom shiny observeEvent updateSelectInput
.define_redDimPlot_parameter_observers <- function(mode, id, 
    input, output, session, pObjects, rObjects) 
{
    .define_plot_parameter_observers(mode, id,
        protected=c(.redDimXAxis, .redDimYAxis),
        input=input, output=output, session=session, pObjects=pObjects, rObjects=rObjects)

    plot_name <- paste0(mode, id)
    cur_field <- paste0(plot_name, "_", .redDimType)
    dim_fieldX <- paste0(plot_name, "_", .redDimXAxis)
    dim_fieldY <- paste0(plot_name, "_", .redDimYAxis)

    observeEvent(input[[cur_field]], {
        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[mode]][[.redDimType]]))
        if (identical(matched_input, pObjects$memory[[mode]][[.redDimType]][id])) {
            return(NULL)
        }
        pObjects$memory[[mode]][[.redDimType]][id] <- matched_input

        # Updating the selectInputs as well. This should not trigger re-plotting as the identical() check in the
        # corresponding observers should stop the replotting flag from being set.
        new_max <- ncol(reducedDim(se, matched_input))
        capped_X <- pmin(new_max, pObjects$memory[[mode]][[.redDimXAxis]][id])
        capped_Y <- pmin(new_max, pObjects$memory[[mode]][[.redDimYAxis]][id])
        pObjects$memory[[mode]][[.redDimXAxis]][id] <- capped_X
        pObjects$memory[[mode]][[.redDimYAxis]][id] <- capped_Y

        new_choices <- seq_len(new_max)
        names(new_choices) <- new_choices
        updateSelectInput(session, dim_fieldX, choices=new_choices, selected=capped_X)
        updateSelectInput(session, dim_fieldY, choices=new_choices, selected=capped_Y)

        .regenerate_unselected_plot(mode, id, pObjects, rObjects)
    }, ignoreInit=TRUE)
}
