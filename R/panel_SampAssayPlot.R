#' The sample assay plot panel
#'
#' Plots sample assay values. What more do I have to say?
#'
#' @section Constructor:
#' \code{SampAssayPlot()} creates an instance of a SampAssayPlot class.
#'
#' @section Panel parameters:
#' \code{\link{.defineParamInterface}} will create parameter elements for choosing the reduced dimensions to plot.
#' More details to be added.
#'
#' @author Aaron Lun
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
#' .defineParamInterface,SampAssayPlot-method
#' .createParamObservers,SampAssayPlot-method
#' @name SampAssayPlot
NULL

#' @export
SampAssayPlot <- function() {
    new("SampAssayPlot")
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
setMethod(".defineParamInterface", "SampAssayPlot", function(x, se, active_panels) {
    mode <- .getEncodedName(x) 
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    link_sources <- .define_link_sources(active_panels)
    tab_by_col <- c(.noSelection, link_sources$col_tab)

    common_info <- .get_common_info(se, "SampAssayPlot")
    row_covariates <- common_info$covariates
    all_assays <- common_info$assays

    xaxis_choices <- c(.sampAssayXAxisNothingTitle)
    if (length(row_covariates)) { # As it is possible for this plot to be _feasible_ but for no row data to exist.
        xaxis_choices <- c(xaxis_choices, .sampAssayXAxisRowDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .sampAssayXAxisSampNameTitle)

    sample_names <- seq_len(ncol(se))
    names(sample_names) <- int_metadata(se)$iSEE$sample_names

    plot.param <- list(
        selectInput(
            .input_FUN(.sampAssayYAxisSampName),
            label="Sample of interest (Y-axis):",
            choices=sample_names, selected=x[[.sampAssayYAxisSampName]]),
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
            selectInput(
                .input_FUN(.sampAssayXAxisSampName),
                label="Sample of interest (X-axis):",
                choices=sample_names, selected=x[[.sampAssayXAxisSampName]]),
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
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]

    .define_box_observers(mode, id, .dataParamBoxOpen, input, pObjects)

    .define_plot_parameter_observers(mode, id,
        protected=c(.sampAssayAssay, .sampAssayXAxisRowData),
        nonfundamental=character(0),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    sample_choices <- seq_len(ncol(se))
    names(sample_choices) <- colnames(se)

    .define_dim_name_observer(mode, id,
        name_field=.sampAssayXAxisSampName,
        choices=sample_choices,
        in_use_field=.sampAssayXAxis,
        in_use_value=.sampAssayXAxisSampNameTitle,
        is_protected=TRUE,
        table_field=.sampAssayXAxisColTable,
        link_type="xaxis",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dim_name_observer(mode, id,
        name_field=.sampAssayYAxisSampName,
        choices=sample_choices,
        in_use_field=NA,
        in_use_value=NA,
        is_protected=TRUE,
        table_field=.sampAssayYAxisColTable,
        link_type="yaxis",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

#' @export
setMethod(".getEncodedName", "SampAssayPlot", function(x) "sampAssayPlot") # TODO change to class name.

#' @export
setMethod(".getFullName", "SampAssayPlot", function(x) "Sample assay plot") # TODO change to class name.

#' @export
setMethod(".getPlottingFunction", "SampAssayPlot", function(x) .make_sampAssayPlot)
