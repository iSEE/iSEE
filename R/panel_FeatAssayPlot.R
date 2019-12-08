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
#' .defineParamInterface,FeatAssayPlot-method
#' .createParamObservers,FeatAssayPlot-method
#' @name FeatAssayPlot
NULL

#' @export
FeatAssayPlot <- function() {
    new("FeatAssayPlot")
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

    mode <- .getEncodedName(x)
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
setMethod(".defineParamInterface", "FeatAssayPlot", function(x, se, active_panels) {
    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    panel_name <- paste0(mode, id)
    .input_FUN <- function(field) { paste0(panel_name, "_", field) }

    link_sources <- .define_link_sources(active_panels)
    tab_by_row <- c(.noSelection, link_sources$row_tab)

    all_assays <- .get_common_info(se, "DotPlot")$valid.assay.names
    column_covariates <- .get_common_info(se, "ColumnDotPlot")$valid.colData.names

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
setMethod(".createParamObservers", "FeatAssayPlot", function(x, se, input, session, pObjects, rObjects) {
    callNextMethod()

    mode <- .getEncodedName(x)
    id <- x[[.organizationId]]
    plot_name <- paste0(mode, id)

    .define_box_observers(plot_name, .dataParamBoxOpen, input, pObjects)

    .define_protected_parameter_observers(plot_name,
        fields=c(.featAssayAssay, .featAssayXAxisColData),
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dim_name_observer(plot_name,
        name_field=.featAssayXAxisFeatName,
        choices=rownames(se),
        in_use_field=.featAssayXAxis,
        in_use_value=.featAssayXAxisFeatNameTitle,
        is_protected=TRUE,
        table_field=.featAssayXAxisRowTable,
        link_type="xaxis",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_dim_name_observer(plot_name,
        name_field=.featAssayYAxisFeatName,
        choices=rownames(se),
        in_use_field=NA,
        in_use_value=NA,
        is_protected=TRUE,
        table_field=.featAssayYAxisRowTable,
        link_type="yaxis",
        input=input, session=session, pObjects=pObjects, rObjects=rObjects)

    .define_selectize_update_observer(plot_name, .featAssayYAxisFeatName,
        choices=rownames(se), selected=x[[.featAssayYAxisFeatName]],
        session=session, rObjects=rObjects)

    .define_selectize_update_observer(plot_name, .featAssayXAxisFeatName,
        choices=rownames(se), selected=x[[.featAssayXAxisFeatName]],
        session=session, rObjects=rObjects)

    for (field in c(.featAssayXAxisRowTable, .featAssayYAxisRowTable)) {
        pObjects$aesthetics_links <- .add_interpanel_link(pObjects$aesthetics_links, 
            panel_name=plot_name, parent_name=x[[field]], field=field, protected=TRUE)
    }
})

#' @export
setMethod(".getEncodedName", "FeatAssayPlot", function(x) "featAssayPlot") # TODO change to class name.

#' @export
setMethod(".getFullName", "FeatAssayPlot", function(x) "Feature assay plot")

#' @export
setMethod(".getPlottingFunction", "FeatAssayPlot", function(x) .make_featAssayPlot)
