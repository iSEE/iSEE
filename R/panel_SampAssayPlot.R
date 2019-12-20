#' The SampAssayPlot panel
#'
#' The SampAssayPlot is a panel class for creating a \linkS4class{ColumnDotPlot} where the y-axis represents the expression of a sample of interest, using the \code{\link{assay}} values of the \linkS4class{SummarizedExperiment}.
#' It provides slots and methods for specifying which sample to use and what to plot on the x-axis.
#'
#' @section Slot overview:
#' The following slots control the dimensionality reduction result that is used:
#' \itemize{
#' \item \code{YAxisSampName}, a string specifying the name of the sample to plot on the y-axis.
#' If \code{NA}, defaults to the first column name of the SummarizedExperiment object.
#' \item \code{Assay}, string specifying the name of the assay to use for obtaining expression values.
#' Defaults to the first valid assay name (see \code{?"\link{.refineParameters,DotPlot-method}"} for details).
#' \item \code{YAxisRowTable}, string specifying the encoded name of the transmitting panel to obtain a single selection that replaces \code{YAxisSampName}.
#' Defaults to \code{"---"}, i.e., no transmission is performed.
#' \item \code{XAxis}, string specifying what should be plotting on the x-axis.
#' This can be any one of \code{"None"}, \code{"Sample name"} or \code{"Column data"}.
#' Defaults to \code{"None"}.
#' \item \code{XAxisColData}, string specifying which column of the \code{\link{colData}} should be shown on the x-axis,
#' if \code{XAxis="Column data"}.
#' Defaults to the first valid \code{\link{colData}} field (see \code{?"\link{.refineParameters,ColumnDotPlot-method}"} for details).
#' \item \code{XAaxisSampName}, string specifying the name of the sample to plot on the x-axis,
#' if \code{XAxis="Sample name"}.
#' Defaults to the first column name.
#' }
#'
#' In addition, this class inherits all slots from its parent \linkS4class{ColumnDotPlot}, \linkS4class{DotPlot} and \linkS4class{Panel} classes.
#'
#' @section Constructor:
#' \code{SampAssayPlot(...)} creates an instance of a SampAssayPlot class, where any slot and its value can be passed to \code{...} as a named argument.
#'
#' @section Contract description:
#' The SampAssayPlot will provide user interface elements to change all above slots as well as slots in its parent classes.
#' It will also provide observers to respond to any input changes in those slots and trigger rerendering of the output.
#' Subclasses do not have to provide any methods, as this is a concrete class.
#'
#' @section Supported methods:
#' In the following code snippets, \code{x} is an instance of a \linkS4class{SampAssayPlot} class.
#' Refer to the documentation for each method for more details on the remaining arguments.
#'
#' For setting up data values:
#' \itemize{
#' \item \code{\link{.refineParameters}(x, se)} replaces any \code{NA} values in \code{XAxisSampName} and \code{YAxisSampName} with the first column name; any \code{NA} value in \code{Assay} with the first valid assay name; and any \code{NA} value in \code{XAxisColData} with the first valid column metadata field.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method for further refinements to \code{x}.
#' If no columns or assays are present, \code{NULL} is returned instead.
#' }
#'
#' For defining the interface:
#' \itemize{
#' \item \code{\link{.defineInterface}(x, se, active_panels)} defines the user interface for manipulating all slots described above and in the parent classes.
#' This is combined with the interface elements provided by the \linkS4class{ColumnDotPlot}.
#' }
#'
#' For monitoring reactive expressions:
#' \itemize{
#' \item \code{\link{.createParamObservers}(x, se, input, session, pObjects, rObjects)} sets up observers for all slots described above and in the parent classes.
#' This will also call the equivalent \linkS4class{ColumnDotPlot} method.
#' }
#'
#' For defining the panel name:
#' \itemize{
#' \item \code{\link{.getEncodedName}(x)} will return \code{"SampAssayPlot"}.
#' \item \code{\link{.getFullName}(x)} will return \code{"Sample assay plot"}.
#' }
#'
#' For creating the plot:
#' \itemize{
#' \item \code{\link{.getCommandsDataXY}(x)} will return a list of plotting information, including a character vector of commands to construct a data.frame of sample expression values.
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
#' x <- SampAssayPlot()
#' x[["XAxis"]]
#' x[["Assay"]] <- "logcounts"
#' x[["XAxisRowData"]] <- "stuff"
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
#' @aliases SampAssayPlot SampAssayPlot-class
#' .refineParameters,SampAssayPlot-method
#' .defineInterface,SampAssayPlot-method
#' .createParamObservers,SampAssayPlot-method
#' .getEncodedName,SampAssayPlot-method
#' .getFullName,SampAssayPlot-method
#' .getCommandsDataXY,SampAssayPlot-method
#'
#' @name SampAssayPlot-class
NULL

#' @export
SampAssayPlot <- function(...) {
    new("SampAssayPlot", ...)
}

#' @export
#' @importFrom methods callNextMethod
setMethod("initialize", "SampAssayPlot", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object <- .empty_default(.Object, .sampAssayAssay)
    .Object <- .empty_default(.Object, .sampAssayXAxis, .sampAssayXAxisNothingTitle)
    .Object <- .empty_default(.Object, .sampAssayXAxisRowData)
    .Object <- .empty_default(.Object, .sampAssayXAxisColTable, .noSelection)
    .Object <- .empty_default(.Object, .sampAssayXAxisSampName)
    .Object <- .empty_default(.Object, .sampAssayYAxisColTable, .noSelection)
    .Object <- .empty_default(.Object, .sampAssayYAxisSampName)
    .Object
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
#' @importFrom methods callNextMethod
setMethod(".refineParameters", "SampAssayPlot", function(x, se) {
    x <- callNextMethod()
    if (is.null(x)) {
        return(NULL)
    }

    if (ncol(se)==0L) {
        warning(sprintf("no columns for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    mode <- .getEncodedName(x)
    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names
    if (length(all_assays)==0L) {
        warning(sprintf("no named 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    assay_choice <- x[[.sampAssayAssay]]
    if (is.na(assay_choice) || !assay_choice %in% all_assays) {
        x[[.sampAssayAssay]] <- all_assays[1]
    }

    x[[.sampAssayXAxisSampName]] <- colnames(se)[1]
    x[[.sampAssayYAxisSampName]] <- colnames(se)[1]

    row_covariates <- .get_common_info(se, "RowDotPlot")$valid.rowData.names
    row_choice <- x[[.sampAssayXAxisRowData]]
    if ((is.na(row_choice) || !row_choice %in% row_covariates) && length(row_covariates)) {
        x[[.sampAssayXAxisRowData]] <- row_covariates[1]
    }

    x
})

.sampAssayXAxisNothingTitle <- "None"
.sampAssayXAxisRowDataTitle <- "Row data"
.sampAssayXAxisSampNameTitle <- "Sample name"

#' @importFrom S4Vectors setValidity2 isSingleString
setValidity2("SampAssayPlot", function(object) {
    msg <- character(0)

    allowable <- c(.sampAssayXAxisNothingTitle, .sampAssayXAxisRowDataTitle, .sampAssayXAxisSampNameTitle)
    if (!object[[.sampAssayXAxis]] %in% allowable) {
        msg <- c(msg, sprintf("choice of '%s' should be one of %s", .sampAssayXAxis,
            paste(sprintf("'%s'", allowable), collapse=", ")))
    }

    for (field in c(.sampAssayAssay, .sampAssayXAxisRowData, .sampAssayXAxisColTable,
        .sampAssayXAxisSampName, .sampAssayYAxisColTable, .sampAssayYAxisSampName))
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
setMethod(".defineInterface", "SampAssayPlot", function(x, se, select_info) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    row_covariates <- .get_common_info(se, "RowDotPlot")$valid.rowData.names
    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names
    tab_by_col <- select_info$single$column

    xaxis_choices <- c(.sampAssayXAxisNothingTitle)
    if (length(row_covariates)) { # As it is possible for this plot to be _feasible_ but for no row data to exist.
        xaxis_choices <- c(xaxis_choices, .sampAssayXAxisRowDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .sampAssayXAxisSampNameTitle)

    plot.param <- list(
        selectizeInput(
            .input_FUN(.sampAssayYAxisSampName),
            label="Sample of interest (Y-axis):",
            choices=NULL, selected=NULL, multiple=FALSE),
        selectInput(
            .input_FUN(.sampAssayYAxisColTable), label=NULL, choices=tab_by_col,
            selected=.choose_link(x[[.sampAssayYAxisColTable]], tab_by_col, force_default=TRUE)),
        selectInput(
            .input_FUN(.sampAssayAssay), label=NULL,
            choices=all_assays, selected=x[[.sampAssayAssay]]),
        radioButtons(
            .input_FUN(.sampAssayXAxis), label="X-axis:", inline=TRUE,
            choices=xaxis_choices, selected=x[[.sampAssayXAxis]]),
        .conditional_on_radio(
            .input_FUN(.sampAssayXAxis),
            .sampAssayXAxisRowDataTitle,
            selectInput(
                .input_FUN(.sampAssayXAxisRowData),
                label="Row data of interest (X-axis):",
                choices=row_covariates, selected=x[[.sampAssayXAxisRowData]])),
        .conditional_on_radio(
            .input_FUN(.sampAssayXAxis),
            .sampAssayXAxisSampNameTitle,
            selectizeInput(
                .input_FUN(.sampAssayXAxisSampName),
                label="Sample of interest (X-axis):",
                choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(.input_FUN(.sampAssayXAxisColTable), label=NULL,
                choices=tab_by_col, selected=x[[.sampAssayXAxisColTable]]))
    )

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=x[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
#' @importFrom methods callNextMethod
setMethod(".createParamObservers", "SampAssayPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_box_observers(plot_name, .dataParamBoxOpen, input, pObjects)

    .define_protected_parameter_observers(plot_name,
        fields=c(.sampAssayAssay, .sampAssayXAxisRowData),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dimname_observers(plot_name,
        name_field=.sampAssayXAxisSampName,
        choices=colnames(se),
        in_use_field=.sampAssayXAxis,
        in_use_value=.sampAssayXAxisSampNameTitle,
        is_protected=TRUE,
        table_field=.sampAssayXAxisColTable,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dimname_observers(plot_name,
        name_field=.sampAssayYAxisSampName,
        choices=colnames(se),
        in_use_field=NA,
        in_use_value=NA,
        is_protected=TRUE,
        table_field=.sampAssayYAxisColTable,
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    for (field in c(.sampAssayXAxisColTable, .sampAssayYAxisColTable)) {
        pObjects$aesthetics_links <- .add_interpanel_link(pObjects$aesthetics_links,
            panel_name=plot_name, parent_name=x[[field]], field=field)
    }
})

#' @export
setMethod(".getEncodedName", "SampAssayPlot", function(x) "sampAssayPlot") # TODO change to class name.

#' @export
setMethod(".getFullName", "SampAssayPlot", function(x) "Sample assay plot") # TODO change to class name.

#' @export
setMethod(".getCommandsDataXY", "SampAssayPlot", function(x) {
    data_cmds <- list()

    samp_selected_y <- x[[.sampAssayYAxisSampName]]
    assay_choice <- x[[.sampAssayAssay]]

    plot_title <- samp_selected_y
    y_lab <- sprintf("%s (%s)", samp_selected_y, assay_choice)
    data_cmds[["y"]] <- sprintf(
        "plot.data <- data.frame(Y=assay(se, %s, withDimnames=FALSE)[,%s], row.names=rownames(se));",
        deparse(assay_choice), deparse(samp_selected_y)
    )

    # Prepare X-axis data.
    x_choice <- x[[.sampAssayXAxis]]

    if (x_choice == .sampAssayXAxisNothingTitle) {
        x_lab <- ''
        data_cmds[["x"]] <- "plot.data$X <- factor(character(nrow(se)));"

    } else if (x_choice == .sampAssayXAxisRowDataTitle) {
        x_lab <- x[[.sampAssayXAxisRowData]]
        plot_title <- paste(plot_title, "vs", x_lab)
        data_cmds[["x"]] <- sprintf("plot.data$X <- rowData(se)[, %s];", deparse(x_lab))

    } else {
        samp_selected_x <- x[[.sampAssayXAxisSampName]]
        plot_title <- paste(plot_title, "vs", samp_selected_x)
        x_lab <- sprintf("%s (%s)", samp_selected_x, assay_choice)
        data_cmds[["x"]] <- sprintf(
            "plot.data$X <- assay(se, %s, withDimnames=FALSE)[, %s];",
            deparse(assay_choice), deparse(samp_selected_x)
        )
    }

    return(list(data_cmds=data_cmds, plot_title=plot_title, x_lab=x_lab, y_lab=y_lab))
})
