#' The reduced dimension plot panel
#'
#' Plots reduced dimensions. What more do I have to say?
#'
#' @section Constructor:
#' \code{RedDimPlot()} creates an instance of a RedDimPlot class.
#'
#' @section Panel parameters:
#' \code{\link{.defineParamInterface}} will create parameter elements for choosing the reduced dimensions to plot.
#' More details to be added.
#'
#' @author Aaron Lun
#'
#' @docType methods
#' @aliases RedDimPlot RedDimPlot-class
#' .defineParamInterface,RedDimPlot-method
#' .createParamObservers,RedDimPlot-method
#' @name RedDimPlot
NULL

RedDimPlot <- function() {
    new("RedDimPlot")
}

#' @export
#' @importFrom SingleCellExperiment reducedDim reducedDimNames
#' @importFrom shiny selectInput
setMethod(".defineParamInterface", "RedDimPlot", function(x, id, param_choices, se, active_panels) {
    cur_reddim <- param_choices[[.redDimType]]
    max_dim <- ncol(reducedDim(se, cur_reddim))
    choices <- seq_len(max_dim)
    names(choices) <- choices

    mode <- .getEncodedName(x)
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
  
    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=param_choices[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom shiny observeEvent updateSelectInput
setMethod(".createParamObservers", "RedDimPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    .define_plot_parameter_observers(mode, id,
        protected=c(.redDimXAxis, .redDimYAxis),
        nonfundamental=character(0),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_box_observers(mode, id, .dataParamBoxOpen, input, pObjects)

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
})

#' @export
setMethod(".getEncodedName", "RedDimPlot", function(x) "redDimPlot")

#' @export
setMethod(".getPlottingFunction", "RedDimPlot", function(x) .make_redDimPlot)
