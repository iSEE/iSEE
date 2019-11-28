#' The feature assay plot panel
#'
#' Plots feature assay values. What more do I have to say?
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
#' sce <- iSEE:::.set_common_info(sce, .getEncodedName(x), 
#'     .cacheCommonInfo(x, sce))
#' .refineParameters(x, sce, list())
#' 
#' # Replaces the default with something sensible.
#' assayNames(sce) <- old_assay_names
#' sce <- iSEE:::.set_common_info(sce, .getEncodedName(x), 
#'     .cacheCommonInfo(x, sce))
#' .refineParameters(x, sce, list())
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
#' @importFrom SummarizedExperiment assayNames rowData
setMethod(".cacheCommonInfo", "SampAssayPlot", function(x, se) {
    # Only using named assays.
    named_assays <- assayNames(se)
    named_assays <- named_assays[named_assays!=""]

    # Only allowing atomic covariates.
    covariates <- .find_atomic_fields(rowData(se))

    # Namespacing.
    out <- callNextMethod()
    out$SampAssayPlot <- list(assays=named_assays, covariates=covariates)
    out
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
setMethod(".refineParameters", "SampAssayPlot", function(x, se, active_panels) {
    if (any(dim(se)==0L)) {
        warning(sprintf("no dimensions for plotting '%s'", class(x)[1]))
        return(NULL)
    }
    if (is.null(colnames(se))) {
        warning(sprintf("no column names for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    mode <- .getEncodedName(x)
    all_assays <- .get_common_info(se, mode)$SampAssayPlot$assays
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

    row_covariates <- .get_common_info(se, mode)$SampAssayPlot$covariates
    row_choice <- x[[.sampAssayXAxisRowData]]
    if ((is.na(row_choice) || !row_choice %in% row_covariates) && length(row_covariates)) {
        x[[.sampAssayXAxisRowData]] <- row_covariates[1]
    }

    callNextMethod()
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
setMethod(".defineParamInterface", "SampAssayPlot", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    link_sources <- .define_link_sources(active_panels)
    tab_by_col <- c(.noSelection, link_sources$col_tab)

    common_info <- .get_common_info(se, mode)$SampAssayPlot
    row_covariates <- common_info$covariates
    all_assays <- common_info$assays

    xaxis_choices <- c(.sampAssayXAxisNothingTitle)
    if (length(row_covariates)) { # As it is possible for this plot to be _feasible_ but for no row data to exist.
        xaxis_choices <- c(xaxis_choices, .sampAssayXAxisRowDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .sampAssayXAxisSampNameTitle)

    plot.param <- list(
        selectInput(
            .input_FUN(.sampAssayYAxisSampName),
            label="Sample of interest (Y-axis):",
            choices=sample_names, selected=param_choices[[.sampAssayYAxisSampName]]),
        selectInput(
            .input_FUN(.sampAssayYAxisColTable), label=NULL, choices=tab_by_col,
            selected=.choose_link(param_choices[[.sampAssayYAxisColTable]], tab_by_col, force_default=TRUE)),
        selectInput(
            .input_FUN(.sampAssayAssay), label=NULL,
            choices=all_assays, selected=param_choices[[.sampAssayAssay]]),
        radioButtons(
            .input_FUN(.sampAssayXAxis), label="X-axis:", inline=TRUE,
            choices=xaxis_choices, selected=param_choices[[.sampAssayXAxis]]),
        .conditional_on_radio(
            .input_FUN(.sampAssayXAxis),
            .sampAssayXAxisRowDataTitle,
            selectInput(
                .input_FUN(.sampAssayXAxisRowData),
                label="Row data of interest (X-axis):",
                choices=row_covariates, selected=param_choices[[.sampAssayXAxisRowData]])),
        .conditional_on_radio(
            .input_FUN(.sampAssayXAxis),
            .sampAssayXAxisSampNameTitle,
            selectInput(
                .input_FUN(.sampAssayXAxisSampName),
                label="Sample of interest (X-axis):",
                choices=sample_names, selected=param_choices[[.sampAssayXAxisSampName]]),
            selectInput(.input_FUN(.sampAssayXAxisColTable), label=NULL,
                choices=tab_by_col, selected=param_choices[[.sampAssayXAxisColTable]]))
    )

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=param_choices[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
setMethod(".createParamObservers", "SampAssayPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)

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
setMethod(".getPlottingFunction", "SampAssayPlot", function(x) .make_sampAssayPlot)
