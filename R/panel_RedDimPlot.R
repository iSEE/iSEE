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
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' # Replaces the default with something sensible.
#' sce <- runPCA(sce)
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
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
#' @importFrom methods callNextMethod
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
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "RedDimPlot", function(x, se) {
    if (is.null(.get_common_info(se, "RedDimPlot"))) {
        if (is(se, "SingleCellExperiment")) {
            available <- reducedDimNames(se)
            for (y in seq_along(available)) {
                if (ncol(reducedDim(se, y))==0L) {
                    available[y] <- NA_character_
                }
            }
            available <- available[!is.na(available)]
        } else {
            available <- character(0)
        }

        se <- .set_common_info(se, "RedDimPlot",
            valid.reducedDim.names=available)
    }

    callNextMethod()
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "RedDimPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    available <- .get_common_info(se, "RedDimPlot")$valid.reducedDim.names
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

    x
})

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("RedDimPlot", function(object) {
    msg <- character(0)

    if (!isSingleString(val <- object[[.redDimType]])) {
        msg <- c(msg, sprintf("'%s' must be a single string", .redDimType))
    }

    for (field in c(.redDimXAxis, .redDimYAxis)) {
        if (length(val <- object[[field]])!=1 || is.na(val) || val <= 0L) {
            msg <- c(msg, sprintf("'%s' must be a single positive integer", field))
        }
    }

    if (length(msg)>0) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom SingleCellExperiment reducedDim reducedDimNames
#' @importFrom shiny selectInput
#' @importFrom methods callNextMethod
setMethod(".defineParamInterface", "RedDimPlot", function(x, se, active_panels) {
    cur_reddim <- x[[.redDimType]]
    max_dim <- ncol(reducedDim(se, cur_reddim))
    choices <- seq_len(max_dim)

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    plot.param <- list(
        selectInput(.input_FUN(.redDimType), label="Type",
            choices=.get_common_info(se, "RedDimPlot")$valid.reducedDim.names,
            selected=cur_reddim),
        selectInput(.input_FUN(.redDimXAxis), label="Dimension 1",
            choices=choices, selected=x[[.redDimXAxis]]),
        selectInput(.input_FUN(.redDimYAxis), label="Dimension 2",
            choices=choices, selected=x[[.redDimYAxis]])
    )

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=x[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom shiny observeEvent updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createParamObservers", "RedDimPlot", function(x, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_protected_parameter_observers(plot_name, fields=c(.redDimXAxis, .redDimYAxis),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_box_observers(plot_name, .dataParamBoxOpen, input, pObjects)

    cur_field <- paste0(plot_name, "_", .redDimType)
    dim_fieldX <- paste0(plot_name, "_", .redDimXAxis)
    dim_fieldY <- paste0(plot_name, "_", .redDimYAxis)

    observeEvent(input[[cur_field]], {
        matched_input <- as(input[[cur_field]], typeof(pObjects$memory[[plot_name]][[.redDimType]]))
        if (identical(matched_input, pObjects$memory[[plot_name]][[.redDimType]])) {
            return(NULL)
        }
        pObjects$memory[[plot_name]][[.redDimType]] <- matched_input

        # Updating the selectInputs as well. This should not trigger re-plotting as the identical() check in the
        # corresponding observers should stop the replotting flag from being set.
        new_max <- ncol(reducedDim(se, matched_input))
        capped_X <- pmin(new_max, pObjects$memory[[plot_name]][[.redDimXAxis]])
        capped_Y <- pmin(new_max, pObjects$memory[[plot_name]][[.redDimYAxis]])
        pObjects$memory[[plot_name]][[.redDimXAxis]] <- capped_X
        pObjects$memory[[plot_name]][[.redDimYAxis]] <- capped_Y

        new_choices <- seq_len(new_max)
        updateSelectInput(session, dim_fieldX, choices=new_choices, selected=capped_X)
        updateSelectInput(session, dim_fieldY, choices=new_choices, selected=capped_Y)

        .regenerate_unselected_plot(plot_name, pObjects, rObjects)
    }, ignoreInit=TRUE)

    callNextMethod()
})

#' @export
setMethod(".getEncodedName", "RedDimPlot", function(x) "redDimPlot")

#' @export
setMethod(".getFullName", "RedDimPlot", function(x) "Reduced dimension plot")

#' @export
setMethod(".getPlottingFunction", "RedDimPlot", function(x) .make_redDimPlot)

#' @export
setMethod(".getCommandsDataXY", "RedDimPlot", function(x, param_choices) {
    data_cmds <- list()

    data_cmds[["reducedDim"]] <- sprintf(
        "red.dim <- reducedDim(se, %s);", deparse(param_choices[[.redDimType]]))
    data_cmds[["xy"]] <- sprintf(
        "plot.data <- data.frame(X=red.dim[, %i], Y=red.dim[, %i], row.names=colnames(se));",
        param_choices[[.redDimXAxis]], param_choices[[.redDimYAxis]])

    plot_title <- param_choices[[.redDimType]]
    x_lab <- sprintf("Dimension %s", param_choices[[.redDimXAxis]])
    y_lab <- sprintf("Dimension %s", param_choices[[.redDimYAxis]])

    return(list(data_cmds=data_cmds, plot_title=plot_title, x_lab=x_lab, y_lab=y_lab))
})
