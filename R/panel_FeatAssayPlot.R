#' The FeatAssayPlot panel
#'
#' The FeatAssayPlot is a panel class for creating a \linkS4class{ColumnDotPlot} where the y-axis represents the expression of a feature of interest, using the \code{\link{assay}} values of the \linkS4class{SummarizedExperiment}.
#' It provides slots and methods for specifying which feature to use and what to plot on the x-axis.
#'
#' @section Slot overview:
#' The following slots control the dimensionality reduction result that is used:
#' \itemize{
#' \item \code{YAxisFeatName}, a string specifying the name of the feature to plot on the y-axis.
#' If \code{NA}, defaults to the first row name of the SummarizedExperiment object.
#' \item \code{Assay}, string specifying the name of the assay to use for obtaining expression values.
#' Defaults to the first valid assay name (see \code{?"\link{.refineParameters,DotPlot-method}"} for details).
#' \item \code{YAxisRowTable}, string specifying the encoded name of the transmitting panel to obtain a single selection that replaces \code{YAxisFeatName}.
#' Defaults to \code{"---"}, i.e., no transmission is performed.
#' \item \code{XAxis}, string specifying what should be plotting on the x-axis.
#' This can be any one of \code{"None"}, \code{"Feature name"} or \code{"Column data"}.
#' Defaults to \code{"None"}.
#' \item \code{XAxisColData}, string specifying which column of the \code{\link{colData}} should be shown on the x-axis,
#' if \code{XAxis="Column data"}.
#' Defaults to the first valid \code{\link{colData}} field (see \code{?"\link{.refineParameters,ColumnDotPlot-method}"} for details).
#' \item \code{XAaxisFeatName}, string specifying the name of the feature to plot on the x-axis,
#' if \code{XAxis="Feature name"}.
#' Defaults to the first row name.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{FeatAssayPlot(...)} creates an instance of a FeatAssayPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Contract description:
#' The FeatAssayPlot will provide user interface elements to change all above slots as well as slots in its parent classes.
#' It will also provide observers to respond to any input changes in those slots and trigger rerendering of the output.
#' Subclasses do not have to provide any methods, as this is a concrete class.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{FeatAssayPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} replaces any \code{NA} values in \code{XAxisFeatName} and \code{YAxisFeatName} with the first row name; any \code{NA} value in \code{Assay} with the first valid assay name; and any \code{NA} value in \code{XAxisColData} with the first valid column metadata field.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method for further refinements to \code{x}.
#' If no rows or assays are present, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineInterface}(x, se, select_info)} defines the user interface for manipulating all slots described above and in the parent classes.
#' This is combined with the interface elements provided by the \linkS4class{ColumnDotPlot}.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method.
#' }
#'
#' For defining the panel name:
#' \itemize{
#' \item \code{\link{.fullName}(x)} will return \code{"Feature assay plot"}.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.getCommandsDataXY}(x)} will return a list of plotting information, including a character vector of commands to construct a data.frame of feature expression values.
#' }
#'
#' For managing selections:
#' \itemize{
#' \item \code{\link{.singleSelectionSlots}(x)} will return a data.frame specifying the slots that can be updated by single selections in transmitter panels (\code{YAxisFeatName}, \code{XAxisFeatName}) and the corresponding slots governing the choice of those panels (\code{YAxisColTable}, \code{XAxisColTable}).
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
#' x <- FeatAssayPlot()
#' x[["XAxis"]]
#' x[["Assay"]] <- "logcounts"
#' x[["XAxisColData"]] <- "stuff"
#'
#' ##################
#' # For developers #
#' ##################
#'
#' library(scater)
#' sce <- mockSCE()
#' sce <- logNormCounts(sce)
#'
#' old_assay_names <- assayNames(sce)
#' assayNames(sce) <- character(length(old_assay_names))
#'
#' # Spits out a NULL and a warning if no assays are named.
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' # Replaces the default with something sensible.
#' assayNames(sce) <- old_assay_names
#' sce0 <- .cacheCommonInfo(x, sce)
#' .refineParameters(x, sce0)
#'
#' @docType methods
#' @aliases FeatAssayPlot FeatAssayPlot-class
#' .refineParameters,FeatAssayPlot-method
#' .defineInterface,FeatAssayPlot-method
#' .createObservers,FeatAssayPlot-method
#' .getFullName,FeatAssayPlot-method
#' .getCommandsDataXY,FeatAssayPlot-method
#'
#' @name FeatAssayPlot-class
NULL

#' @export
FeatAssayPlot <- function(...) {
    new("FeatAssayPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "FeatAssayPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object <- .empty_default(.Object, .featAssayAssay)
    .Object <- .empty_default(.Object, .featAssayXAxis, .featAssayXAxisNothingTitle)
    .Object <- .empty_default(.Object, .featAssayXAxisColData)
    .Object <- .empty_default(.Object, .featAssayXAxisRowTable, .noSelection)
    .Object <- .empty_default(.Object, .featAssayXAxisFeatName)
    .Object <- .empty_default(.Object, .featAssayYAxisRowTable, .noSelection)
    .Object <- .empty_default(.Object, .featAssayYAxisFeatName)
    .Object
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "FeatAssayPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    if (nrow(se)==0L) {
        warning(sprintf("no rows available for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names
    if (length(all_assays)==0L) {
        warning(sprintf("no valid 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    assay_choice <- x[[.featAssayAssay]]
    if (is.na(assay_choice) || !assay_choice %in% all_assays) {
        x[[.featAssayAssay]] <- all_assays[1]
    }

    x[[.featAssayXAxisFeatName]] <- rownames(se)[1]
    x[[.featAssayYAxisFeatName]] <- rownames(se)[1]

    column_covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names
    column_choice <- x[[.featAssayXAxisColData]]
    if ((is.na(column_choice) || !column_choice %in% column_covariates) && length(column_covariates)) {
        x[[.featAssayXAxisColData]] <- column_covariates[1]
    }

    x
})

.featAssayXAxisNothingTitle <- "None"
.featAssayXAxisColDataTitle <- "Column data"
.featAssayXAxisFeatNameTitle <- "Feature name"

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("FeatAssayPlot", function(object) {
    msg <- character(0)

    allowable <- c(.featAssayXAxisNothingTitle, .featAssayXAxisColDataTitle, .featAssayXAxisFeatNameTitle)
    if (!object[[.featAssayXAxis]] %in% allowable) {
        msg <- c(msg, sprintf("choice of '%s' should be one of %s", .featAssayXAxis,
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }

    for (field in c(.featAssayAssay, .featAssayXAxisColData, .featAssayXAxisRowTable,
        .featAssayXAxisFeatName, .featAssayYAxisRowTable, .featAssayYAxisFeatName))
    {
        if (!isSingleString(val <- object[[field]])) {
            msg <- c(msg, sprintf("'%s' must be a single string", field))
        }
    }

    if (length(msg)) {
        return(msg)
    }
    TRUE
})

#' @export
#' @importFrom shiny selectInput radioButtons
#' @importFrom methods callNextMethod
setMethod(".defineInterface", "FeatAssayPlot", function(x, se, select_info) {
    panel_name <- .getEncodedName(x)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names
    column_covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names
    tab_by_row <- select_info$single$row

    xaxis_choices <- c(.featAssayXAxisNothingTitle)
    if (length(column_covariates)) { # As it is possible for this plot to be _feasible_ but for no column data to exist.
        xaxis_choices <- c(xaxis_choices, .featAssayXAxisColDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .featAssayXAxisFeatNameTitle)

    plot.param <- list(
        selectizeInput(.input_FUN(.featAssayYAxisFeatName),
            label="Y-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
        selectInput(.input_FUN(.featAssayYAxisRowTable), label=NULL, choices=tab_by_row,
            selected=.choose_link(x[[.featAssayYAxisRowTable]], tab_by_row, force_default=TRUE)),
        selectInput(.input_FUN(.featAssayAssay), label=NULL,
            choices=all_assays, selected=x[[.featAssayAssay]]),
        radioButtons(.input_FUN(.featAssayXAxis), label="X-axis:", inline=TRUE,
            choices=xaxis_choices, selected=x[[.featAssayXAxis]]),
        .conditional_on_radio(.input_FUN(.featAssayXAxis),
            .featAssayXAxisColDataTitle,
            selectInput(.input_FUN(.featAssayXAxisColData),
                label="X-axis column data:",
                choices=column_covariates, selected=x[[.featAssayXAxisColData]])),
        .conditional_on_radio(.input_FUN(.featAssayXAxis),
            .featAssayXAxisFeatNameTitle,
            selectizeInput(.input_FUN(.featAssayXAxisFeatName),
                label="X-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(.input_FUN(.featAssayXAxisRowTable), label=NULL,
                choices=tab_by_row, selected=x[[.featAssayXAxisRowTable]]))
    )

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=x[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createObservers", "FeatAssayPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    plot_name <- .getEncodedName(x)

    .define_box_observers(plot_name, .dataParamBoxOpen, input, pObjects)

    .define_protected_parameter_observers(plot_name,
        fields=c(.featAssayAssay, .featAssayXAxisColData),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dimname_observers(plot_name,
        name_field=.featAssayXAxisFeatName,
        choices=rownames(se),
        in_use_field=.featAssayXAxis,
        in_use_value=.featAssayXAxisFeatNameTitle,
        is_protected=TRUE,
        table_field=.featAssayXAxisRowTable,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dimname_observers(plot_name,
        name_field=.featAssayYAxisFeatName,
        choices=rownames(se),
        in_use_field=NA,
        in_use_value=NA,
        is_protected=TRUE,
        table_field=.featAssayYAxisRowTable,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)
})

#' @export
setMethod(".singleSelectionSlots", "FeatAssayPlot", function(x) {
    c(callNextMethod(),
        list(
            c(parameter=.featAssayXAxisFeatName, source=.featAssayXAxisRowTable),
            c(parameter=.featAssayYAxisFeatName, source=.featAssayYAxisRowTable)
        )
    )
})

#' @export
setMethod(".fullName", "FeatAssayPlot", function(x) "Feature assay plot")

#' @export
setMethod(".panelColor", "FeatAssayPlot", function(x) "#7BB854")

#' @export
setMethod(".getCommandsDataXY", "FeatAssayPlot", function(x) {
    data_cmds <- list()

    ## Setting up the y-axis:
    gene_selected_y <- x[[.featAssayYAxisFeatName]]
    assay_choice <- x[[.featAssayAssay]]
    plot_title <- gene_selected_y
    y_lab <- sprintf("%s (%s)", gene_selected_y, assay_choice)
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=assay(se, %s, withDimnames=FALSE)[%s, ], row.names=colnames(se))",
        deparse(assay_choice), deparse(gene_selected_y)
    )

    ## Checking X axis choice:
    x_choice <- x[[.featAssayXAxis]]

    if (x_choice == .featAssayXAxisColDataTitle) { # colData column selected
        x_lab <- x[[.featAssayXAxisColData]]
        plot_title <- paste(plot_title, "vs", x_lab)
        data_cmds[["x"]] <- sprintf("plot.data$X <- colData(se)[, %s];", deparse(x_lab))

    } else if (x_choice == .featAssayXAxisFeatNameTitle) { # gene selected
        gene_selected_x <- x[[.featAssayXAxisFeatName]]
        plot_title <- paste(plot_title, "vs", gene_selected_x)
        x_lab <- sprintf("%s (%s)", gene_selected_x, assay_choice)
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- assay(se, %s, withDimnames=FALSE)[%s, ];",
            deparse(assay_choice), deparse(gene_selected_x)
        )

    } else { # no x axis variable specified: show single violin
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(ncol(se)))"
    }

    return(list(data_cmds=data_cmds, plot_title=plot_title, x_lab=x_lab, y_lab=y_lab))
})
