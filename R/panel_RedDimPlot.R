#' The RedDimPlot panel
#'
#' The RedDimPlot is a panel class for creating a \linkS4class{ColumnDotPlot} where the coordinates of each column/sample are taken from the \code{\link{reducedDims}} of a \linkS4class{SingleCellExperiment} object.
#' It provides slots and methods for specifying which dimensionality reduction result to use.
#'
#' @section Slot overview:
#' The following slots control the dimensionality reduction result that is used:
#' \itemize{
#' \item \code{RedDimType}, a string specifying the name of the dimensionality reduction result.
#' If \code{NA}, defaults to the first entry of \code{\link{reducedDims}}.
#' \item \code{RedDimXAxis}, integer scalar specifying the dimension to plot on the x-axis.
#' Defaults to 1.
#' \item \code{RedDimYAxis}, integer scalar specifying the dimension to plot on the y-axis.
#' Defaults to 2.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{RedDimPlot(...)} creates an instance of a RedDimPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Contract description:
#' The RedDimPlot will provide user interface elements to change all above slots as well as slots in its parent classes.
#' It will also provide observers to respond to any input changes in those slots and trigger rerendering of the output.
#' Subclasses do not have to provide any methods, as this is a concrete class.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{RedDimPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.cacheCommonInfo}(x)} adds a \code{"RedDimPlot"} entry containing \code{valid.reducedDim.names}, a character vector of names of valid dimensionality reduction results (i.e., at least one dimension).
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method.
#' \item \code{\link{.refineParameters}(x, se)} replaces \code{NA} values in \code{RedDimType} with the first valid dimensionality reduction result name in \code{se}.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method for further refinements to \code{x}.
#' If no dimensionality reduction results are available, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineDataInterface}(x, se, select_info)} returns a list of interface elements for manipulating all slots described above.
#' \item \code{\link{.fullName}(x)} will return the full name of the panel class.
#' \item \code{\link{.panelColor}(x)} will return the specified default color for this panel class.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.generateDotPlotData}(x, envir)} will create a data.frame of reduced dimension coordinates in \code{envir}.
#' It will return the commands required to do so as well as a list of labels.
#' }
#' @author Aaron Lun
#'
#' @seealso
#' \linkS4class{ColumnDotPlot}, for the immediate parent class.
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
#' initialize,RedDimPlot-method
#' .refineParameters,RedDimPlot-method
#' .cacheCommonInfo,RedDimPlot-method
#' .defineDataInterface,RedDimPlot-method
#' .createObservers,RedDimPlot-method
#' .fullName,RedDimPlot-method
#' .panelColor,RedDimPlot-method
#' .generateDotPlotData,RedDimPlot-method
#' @name RedDimPlot-class
NULL

#' @export
RedDimPlot <- function(...) {
    new("RedDimPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "RedDimPlot", function(.Object, ...) {
    args <- list(...)
    args <- .empty_default(args, .redDimType, NA_character_)
    args <- .empty_default(args, .redDimXAxis, 1L)
    args <- .empty_default(args, .redDimYAxis, 2L)
    do.call(callNextMethod, c(list(.Object), args))
})

#' @export
#' @importFrom SingleCellExperiment reducedDimNames reducedDim
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importFrom methods callNextMethod
setMethod(".cacheCommonInfo", "RedDimPlot", function(x, se) {
    if (!is.null(.get_common_info(se, "RedDimPlot"))) {
        return(se)
    }

    se <- callNextMethod()

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

    .set_common_info(se, "RedDimPlot",
        valid.reducedDim.names=available)
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

    msg <- .single_string_error(msg, object, .redDimType)

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
setMethod(".defineDataInterface", "RedDimPlot", function(x, se, select_info) {
    cur_reddim <- x[[.redDimType]]
    max_dim <- ncol(reducedDim(se, cur_reddim))
    choices <- seq_len(max_dim)

    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    list(
        selectInput(.input_FUN(.redDimType), label="Type",
            choices=.get_common_info(se, "RedDimPlot")$valid.reducedDim.names,
            selected=cur_reddim),
        selectInput(.input_FUN(.redDimXAxis), label="Dimension 1",
            choices=choices, selected=x[[.redDimXAxis]]),
        selectInput(.input_FUN(.redDimYAxis), label="Dimension 2",
            choices=choices, selected=x[[.redDimYAxis]])
    )
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom shiny observeEvent updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createObservers", "RedDimPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .createProtectedParameterObservers(plot_name,
        fields=c(.redDimXAxis, .redDimYAxis),
        input=input, pObjects=pObjects, rObjects=rObjects)

    cur_field <- paste0(plot_name, "_", .redDimType)
    dim_fieldX <- paste0(plot_name, "_", .redDimXAxis)
    dim_fieldY <- paste0(plot_name, "_", .redDimYAxis)

    # nocov start
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

        .requestCleanUpdate(plot_name, pObjects, rObjects)
    }, ignoreInit=TRUE)
    # nocov end

    invisible(NULL)
})

#' @export
setMethod(".fullName", "RedDimPlot", function(x) "Reduced dimension plot")

#' @export
setMethod(".panelColor", "RedDimPlot", function(x) "#3565AA")

#' @export
setMethod(".generateDotPlotData", "RedDimPlot", function(x, envir) {
    data_cmds <- list()

    data_cmds[["reducedDim"]] <- sprintf(
        "red.dim <- reducedDim(se, %s);", deparse(x[[.redDimType]]))
    data_cmds[["xy"]] <- sprintf(
        "plot.data <- data.frame(X=red.dim[, %i], Y=red.dim[, %i], row.names=colnames(se));",
        x[[.redDimXAxis]], x[[.redDimYAxis]])

    plot_title <- x[[.redDimType]]
    x_lab <- sprintf("Dimension %s", x[[.redDimXAxis]])
    y_lab <- sprintf("Dimension %s", x[[.redDimYAxis]])

    data_cmds <- unlist(data_cmds)
    .text_eval(data_cmds, envir)

    list(commands=data_cmds, labels=list(title=plot_title, X=x_lab, Y=y_lab))
})
