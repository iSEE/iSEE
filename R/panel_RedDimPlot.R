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
#' @examples
#' #################
#' # For end-users #
#' #################
#'
#' x <- RedDimPlot()
#' x[["Type"]] 
#' x[["Type"]] <- "TSNE"
#' 
#' ##################
#' # For developers #
#' ##################
#' 
#' library(scater)
#' sce <- mockSCE()
#' sce <- logNormCounts(sce)
#'
#' # Spits out a NULL and a warning if no reducedDims are available.
#' sce <- iSEE:::.set_common_info(sce, .getEncodedName(x), 
#'     .cacheCommonInfo(x, sce))
#' .refineParameters(x, sce)
#' 
#' # Replaces the default with something sensible.
#' sce <- runPCA(sce)
#' sce <- iSEE:::.set_common_info(sce, .getEncodedName(x), 
#'     .cacheCommonInfo(x, sce))
#' .refineParameters(x, sce)
#'
#' @docType methods
#' @aliases RedDimPlot RedDimPlot-class
#' .defineParamInterface,RedDimPlot-method
#' .createParamObservers,RedDimPlot-method
#' @name RedDimPlot
NULL

#' @export
RedDimPlot <- function() {
    new("RedDimPlot") 
}

#' @export
setMethod("initialize", "RedDimPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object <- .empty_default(.Object, .redDimType)
    .Object <- .empty_default(.Object, .redDimXAxis, 1L)
    .Object <- .empty_default(.Object, .redDimYAxis, 2L)
    .Object
})

#' @export
#' @importFrom SingleCellExperiment reducedDimNames reducedDim
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
setMethod(".cacheCommonInfo", "RedDimPlot", function(x, se) {
    if (is(se, "SingleCellExperiment")) {
        available <- reducedDimNames(se)
        for (y in seq_along(available)) {
            if (ncol(reducedDim(se, y))==0L) {
                available[y] <- NA_character_
            }
        }
    } else {
        available <- character(0)
    }

    # Namespacing to avoid clashes with parent class' common info.
    out <- callNextMethod()
    out$RedDimPlot <- list(available=available[!is.na(available)])
    out
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
setMethod(".refineParameters", "RedDimPlot", function(x, se, active_panels) {
    available <- .get_common_info(se, .getEncodedName(x))$RedDimPlot$available

    if (!is.na(chosen <- x[[.redDimType]]) &&
        chosen %in% available &&
        x[[.redDimXAxis]] <= ncol(reducedDim(se, chosen)) &&
        x[[.redDimYAxis]] <= ncol(reducedDim(se, chosen)))
    {   
        # All is well, nothing needs to be done here.
    } else {
        if (length(available)==0L) {
            warning(sprintf("no 'reducedDims' with non-zero dimensions for '%s'", class(x)[1]))
            return(NULL)
        }

        y <- available[1]
        x[[.redDimType]] <- y
        x[[.redDimXAxis]] <- 1L
        x[[.redDimYAxis]] <- min(ncol(reducedDim(se, y)), 2L)
    }

    callNextMethod()
})

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("RedDimPlot", function(object) {
    msg <- character(0)
    if (!isSingleString(val <- object[[.redDimType]]) || is.na(val)) {
        msg <- c(msg, sprintf("'%s' must be a single string", .redDimType))
    }
    if (length(val <- object[[.redDimXAxis]])!=1 || is.na(val) || val <= 0L) {
        msg <- c(msg, sprintf("'%s' must be a single positive integer", .redDimXAxis))
    }
    if (length(val <- object[[.redDimYAxis]])!=1 || is.na(val) || val <= 0L) {
        msg <- c(msg, sprintf("'%s' must be a single positive integer", .redDimYAxis))
    }
    if (length(msg)>0) {
        return(msg)
    }
    TRUE   
})

#' @export
#' @importFrom SingleCellExperiment reducedDim reducedDimNames
#' @importFrom shiny selectInput
setMethod(".defineParamInterface", "RedDimPlot", function(x, id, param_choices, se, active_panels) {
    cur_reddim <- param_choices[[.redDimType]]
    max_dim <- ncol(reducedDim(se, cur_reddim))
    choices <- seq_len(max_dim)

    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    plot.param <- list(
        selectInput(.input_FUN(.redDimType), label="Type",
            choices=.get_common_info(se, mode)$RedDimPlot$available, 
            selected=cur_reddim),
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

    callNextMethod()
})

#' @export
setMethod(".getEncodedName", "RedDimPlot", function(x) "redDimPlot")

#' @export
setMethod(".getPlottingFunction", "RedDimPlot", function(x) .make_redDimPlot)
