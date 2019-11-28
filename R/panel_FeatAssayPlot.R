#' The feature assay plot panel
#'
#' Plots feature assay values. What more do I have to say?
#'
#' @section Constructor:
#' \code{FeatAssayPlot()} creates an instance of a FeatAssayPlot class.
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
#' @aliases FeatAssayPlot FeatAssayPlot-class
#' .defineParamInterface,FeatAssayPlot-method
#' .createParamObservers,FeatAssayPlot-method
#' @name FeatAssayPlot
NULL

#' @export
FeatAssayPlot <- function() {
    new("FeatAssayPlot")
}

#' @export
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
#' @importFrom SummarizedExperiment assayNames colData
setMethod(".cacheCommonInfo", "FeatAssayPlot", function(x, se) {
    # Only using named assays.
    named_assays <- assayNames(se)
    named_assays <- named_assays[named_assays!=""]

    # Only allowing atomic covariates.
    covariates <- .find_atomic_fields(colData(se))

    # Namespacing.
    out <- callNextMethod()
    out$FeatAssayPlot <- list(assays=named_assays, covariates=covariates)
    out
})

#' @export
#' @importFrom SingleCellExperiment reducedDim
setMethod(".refineParameters", "FeatAssayPlot", function(x, se, active_panels) {
    if (any(dim(se)==0L)) {
        warning(sprintf("no dimensions for plotting '%s'", class(x)[1]))
        return(NULL)
    }
    if (is.null(rownames(se))) {
        warning(sprintf("no row names for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    mode <- .getEncodedName(x)
    all_assays <- .get_common_info(se, mode)$FeatAssayPlot$assays
    if (length(all_assays)==0L) {
        warning(sprintf("no named 'assays' for plotting '%s'", class(x)[1]))
        return(NULL)
    }

    assay_choice <- x[[.featAssayAssay]] 
    if (is.na(assay_choice) || !assay_choice %in% all_assays) {
        x[[.featAssayAssay]] <- all_assays[1]
    }

    x[[.featAssayXAxisFeatName]] <- rownames(se)[1]
    x[[.featAssayYAxisFeatName]] <- rownames(se)[1]

    column_covariates <- .get_common_info(se, mode)$FeatAssayPlot$covariates
    column_choice <- x[[.featAssayXAxisColData]]
    if ((is.na(column_choice) || !column_choice %in% column_covariates) && length(column_covariates)) {
        x[[.featAssayXAxisColData]] <- column_covariates[1]
    }

    callNextMethod()
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
setMethod(".defineParamInterface", "FeatAssayPlot", function(x, id, param_choices, se, active_panels) {
    mode <- .getEncodedName(x)
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    link_sources <- .define_link_sources(active_panels)
    tab_by_row <- c(.noSelection, link_sources$row_tab)

    common_info <- .get_common_info(se, mode)$FeatAssayPlot
    all_assays <- common_info$assays
    column_covariates <- common_info$covariates

    xaxis_choices <- c(.featAssayXAxisNothingTitle)
    if (length(column_covariates)) { # As it is possible for this plot to be _feasible_ but for no column data to exist.
        xaxis_choices <- c(xaxis_choices, .featAssayXAxisColDataTitle)
    }
    xaxis_choices <- c(xaxis_choices, .featAssayXAxisFeatNameTitle)

    plot.param <- list(
        selectizeInput(.input_FUN(.featAssayYAxisFeatName),
            label="Y-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
        selectInput(.input_FUN(.featAssayYAxisRowTable), label=NULL, choices=tab_by_row,
            selected=.choose_link(param_choices[[.featAssayYAxisRowTable]], tab_by_row, force_default=TRUE)),
        selectInput(.input_FUN(.featAssayAssay), label=NULL,
            choices=all_assays, selected=param_choices[[.featAssayAssay]]),
        radioButtons(.input_FUN(.featAssayXAxis), label="X-axis:", inline=TRUE,
            choices=xaxis_choices, selected=param_choices[[.featAssayXAxis]]),
        .conditional_on_radio(.input_FUN(.featAssayXAxis),
            .featAssayXAxisColDataTitle,
            selectInput(.input_FUN(.featAssayXAxisColData),
                label="X-axis column data:",
                choices=column_covariates, selected=param_choices[[.featAssayXAxisColData]])),
        .conditional_on_radio(.input_FUN(.featAssayXAxis),
            .featAssayXAxisFeatNameTitle,
            selectizeInput(.input_FUN(.featAssayXAxisFeatName),
                label="X-axis feature:", choices=NULL, selected=NULL, multiple=FALSE),
            selectInput(.input_FUN(.featAssayXAxisRowTable), label=NULL,
                choices=tab_by_row, selected=param_choices[[.featAssayXAxisRowTable]]))
    )

    param <- do.call(collapseBox, c(list(id=.input_FUN(.dataParamBoxOpen),
        title="Data parameters", open=param_choices[[.dataParamBoxOpen]]), plot.param))

    c(list(param), callNextMethod())
})

#' @export
#' @importFrom shiny observeEvent updateSelectInput
setMethod(".createParamObservers", "FeatAssayPlot", function(x, id, se, input, session, pObjects, rObjects) {
    mode <- .getEncodedName(x)

    .define_box_observers(mode, id, .dataParamBoxOpen, input, pObjects)

    .define_plot_parameter_observers(mode, id,
        protected=c(.featAssayAssay, .featAssayXAxisColData),
        nonfundamental=character(0),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    feature_choices <- seq_len(nrow(se))
    names(feature_choices) <- rownames(se)

    .define_dim_name_observer(mode, id, 
        name_field=.featAssayXAxisFeatName, 
        choices=feature_choices,
        in_use_field=.featAssayXAxis, 
        in_use_value=.featAssayXAxisFeatNameTitle,
        is_protected=TRUE,
        table_field=.featAssayXAxisRowTable, 
        link_type="xaxis",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dim_name_observer(mode, id, 
        name_field=.featAssayYAxisFeatName, 
        choices=feature_choices,
        in_use_field=NA, 
        in_use_value=NA,
        is_protected=TRUE,
        table_field=.featAssayYAxisRowTable, 
        link_type="yaxis",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    callNextMethod()
})

#' @export
setMethod(".getEncodedName", "FeatAssayPlot", function(x) "featAssayPlot") # TODO change to class name.

#' @export
setMethod(".getPlottingFunction", "FeatAssayPlot", function(x) .make_featAssayPlot)
