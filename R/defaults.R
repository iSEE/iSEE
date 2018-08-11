#' @name iSEE point parameters
#'
#' @title Point plot aesthetic parameters
#'
#' @description Parameters related to aesthetics for point-based plotting panels.
#'
#' @section Coloring parameters:
#' \describe{
#' \item{\code{ColorBy}:}{Character, what type of data should be used for coloring?
#' Defaults to \code{"None"}, but can also be \code{"Feature name"} or \code{"Column data"} (for column-based plots) or \code{"Row data"} for (row-based plots).}
#' \item{\code{ColorByDefaultColor}:}{String specifying the default point colour when \code{ColorBy="None"}.
#' Defaults to \code{"black"}.}
#' \item{\code{ColorByFeatName}:}{Integer, the index of the feature to use for colouring based on expression, if \code{ColorBy="Feature name"}?
#' Defaults to 1, i.e., the first feature in \code{se}.
#' Alternatively, a string can be supplied containing the name of the feature.}
#' \item{\code{ColorByRowTable}:}{Character, which row statistic table should be used to choose a feature to color by, if \code{ColorBy="Feature name"}?
#' Any setting will override \code{ColorByFeatName} upon initialization of the app.
#' Defaults to \code{"---"}, which means that no table will be used.}
#' }
#'
#' For the plots where each point represents a sample (i.e., all plots except for heatmaps and row data plots), the following additional options apply:
#' \describe{
#' \item{\code{ColorByColData}:}{Character, which column of \code{colData(se)} should be used for colouring if \code{ColorBy="Column data"}?
#' Defaults to the first entry of \code{colData(se)}.}
#' \item{\code{ColorByFeatNameAssay}:}{Integer, which assay should be used to supply the expression values for colouring if \code{ColorBy="Feature name"}?
#' Defaults to 1, i.e., the first assay in \code{se}.
#' Alternatively, a string can also be supplied containing the name of the assay, if \code{assays(se)} has names.}
#' }
#'
#' For plots where each point represents a feature (i.e., row data plots), the following additional options apply:
#' \describe{
#' \item{\code{ColorByRowData}:}{Character, which column of \code{rowData(se)} should be used for colouring if \code{ColorBy="Row data"}?
#' Defaults to the first entry of \code{rowData(se)}.}
#' \item{\code{ColorByFeatNameColor}:}{String specifying the colour to be used to highlight the selected feature from the text if \code{ColorBy="Feature name"}.
#' Defaults to \code{"red"}.}
#' }
#'
#' @section Shape parameters:
#' \describe{
#' \item{\code{ShapeBy}:}{Character, what type of data should be used for controlling shape?
#' Defaults to \code{"None"}, but can also be \code{"Column data"}.}
#' }
#'
#' For the plots where each point represents a sample (i.e., all plots except for heatmaps and row data plots), the following additional options apply:
#' \describe{
#' \item{\code{ShapeByColData}:}{Character, which column of \code{colData(se)} should be used for shaping if \code{ShapeBy="Column data"}?
#' This should refer to a categorical variable, and will default to the first such entry of \code{colData(se)}.}
#' }
#'
#' For plots where each point represents a feature (i.e., row data plots), the following additional options apply:
#' \describe{
#' \item{\code{ShapeByRowData}:}{Character, which column of \code{rowData(se)} should be used for shaping if \code{ShapeBy="Row data"}?
#' This should refer to a categorical variable, and will default to the first such entry of \code{rowData(se)}.}
#' }
#'
#' @section Contour line parameters:
#' \describe{
#' \item{\code{ContourAdd}:}{Logical, should contour lines be added to the plot?}
#' \item{\code{ContourColor}:}{String specifying the default colour of contour lines.}
#' }
#'
#' @section Faceting parameters:
#' \describe{
#' \item{\code{FacetByRow}:}{Logical indicating whether the plot should be faceted by row.}
#' \item{\code{FacetByColumn}:}{Logical indicating whether the plot should be faceted by row.}
#' }
#'
#' For the plots where each point represents a sample (i.e., all plots except for heatmaps and row data plots), the following additional options apply:
#' \describe{
#' \item{\code{RowFacetByColData}:}{Character, which column of \code{colData(se)} should be used to facet by row?
#' This should refer to a categorical variable, and will default to the first such entry of \code{colData(se)}.}
#' \item{\code{ColumnFacetByColData}:}{Character, which column of \code{colData(se)} should be used to facet by column?
#' This should refer to a categorical variable, and will default to the first such entry of \code{colData(se)}.}
#' }
#'
#' For the plots where each point represents a feature (i.e., row data plots), the following additional options apply:
#' \describe{
#' \item{\code{RowFacetByRowData}:}{Character, which column of \code{colData(se)} should be used to facet by row?
#' This should refer to a categorical variable, and will default to the first such entry of \code{colData(se)}.}
#' \item{\code{ColumnFacetByRowData}:}{Character, which column of \code{colData(se)} should be used to facet by column?
#' This should refer to a categorical variable, and will default to the first such entry of \code{colData(se)}.}
#' }
#'
#' @section Other plot parameters:
#' Parameter visibility options are:
#' \describe{
#' \item{\code{DataBoxOpen}:}{Logical, should the data parameter box be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{VisualBoxOpen}:}{Logical, should the visual parameter box be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{VisualChoices}:}{A list containing one character vector, specifying the visual box parameters to be shown upon initialization.
#' This defaults to \code{list("Color")} to show only the coloring parameters, but the internal vector can also contain \code{"Points"}, \code{"Facets"} and \code{"Other"}.}
#' }
#'
#' Options related to the appearance of points are:
#' \describe{
#' \item{\code{PointSize}:}{Numeric, the size of the points.
#' Defaults to 1.}
#' \item{\code{PointAlpha}:}{Numeric, what level of transparency should be used for the points?
#' Ignored when \code{SelectEffect="Transparent"} and the transmitting plot has a non-\code{NULL} selection of points.
#' Defaults to 1.}
#' \item{\code{Downsample}:}{Logical, indicating whether downsampling of overlapping points should be performed.
#' Defaults to \code{FALSE}.}
#' \item{\code{SampleRes}:}{Numeric, specifying the downsampling resolution, i.e., the granularity at which points are considered to overlap.
#' Higher values result in a more stringent definition of overlaps, and thus less downsampling.
#' Defaults to 200.}
#' }
#'
#' Text-related parameters are:
#' \describe{
#' \item{\code{FontSize}:}{Numeric, size of the font.
#' Defaults to 1.}
#' \item{\code{LegendPosition}:}{String specifying the legend position.
#' Defaults to \code{"Bottom"} but can also be \code{"Right"}.}
#' \item{\code{ZoomData}:}{A list containing numeric vectors of length 4, containing values with names \code{"xmin"}, \code{"xmax"}, \code{"ymin"} and \code{"ymax"}.
#' These define the zoom window on the x- and y-axes.
#' Each element of the list defaults to \code{NULL}, i.e., no zooming is performed.}
#' }
#'
#' @author
#' Aaron Lun, Kevin Rue-Albrecht, Charlotte Soneson
#'
#' @rdname pointDefaults
#' @seealso
#' \code{\link{redDimPlotDefaults}},
#' \code{\link{featAssayPlotDefaults}},
#' \code{\link{colDataPlotDefaults}},
#' \code{\link{rowDataPlotDefaults}},
#' \code{\link{sampAssayPlotDefaults}}
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' redDimPlotDefaults(sce, n=1)
#' rowDataPlotDefaults(sce, n=1)
NULL

#' @name iSEE selection parameters
#'
#' @title Selection parameters
#'
#' @description Parameters that control the selection of transmitter plots and the effect of selection in a variety of panels.
#'
#' @section Selection parameters for plots:
#' \describe{
#' \item{\code{SelectBoxOpen}:}{Logical, should the selection parameter box be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{SelectByPlot}:}{Character, which other plot should be used for point selection in the current plot?
#' Defaults to \code{"---"}, which means that no plot is used for point selection.}
#' \item{\code{SelectEffect}:}{Character, what is the effect of receiving point selection information?
#' Can be \code{"Restrict"}, where only the selected points are shown; \code{"Color"}, where the selected points have a different color;
#' or \code{"Transparent"}, where all points other than those selected are made transparent.
#' Defaults to \code{"Transparent"}.}
#' \item{\code{SelectColor}:}{Character, what color should be used for the selected points when \code{SelectEffect="Color"}?
#' Defaults to \code{"red"}.}
#' \item{\code{SelectAlpha}:}{Numeric, what level of transparency should be used for the unselected points when \code{SelectEffect="Transparent"}?
#' This should lie in [0, 1], where 0 is fully transparent and 1 is fully opaque.
#' Defaults to 0.1.}
#' }
#' For row-based plots, each point represents a feature, while for column-based plots and heatmaps, each point represents a sample.
#'
#' @section Inter-plot transmission rules:
#' Point selection cannot occur between row-based and column-based plots.
#' This is because each point in a row-based plot is a feature, while each point represents a sample in the other plots.
#' Thus, point selection can only occur between plots of the same point type.
#' The only exception to this rule is the custom panels, which can receive transmissions from both row- and column-based plots -- see \code{\link{customDataPlotDefaults}}.
#' This is because they can generate arbitrary plots and are not truly row- or column-based.
#'
#' @section Selection parameters for tables:
#' For the row statistics tables, the following options apply:
#' \describe{
#' \item{\code{SelectBoxOpen}:}{Logical, should the point selection parameter box be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{SelectByPlot}:}{Character, which other plot should be used to select features in the current table?
#' Defaults to \code{"---"}, which means that no plot is used for point selection.}
#' }
#' Only row-based plots (i.e., row data and sample assay plots) can be used for selecting points to supply to tables, for the same reasons described above.
#'
#' @author
#' Aaron Lun, Kevin Rue-Albrecht
#'
#' @rdname selectDefaults
#' @seealso
#' \code{\link{redDimPlotDefaults}},
#' \code{\link{featAssayPlotDefaults}},
#' \code{\link{colDataPlotDefaults}},
#' \code{\link{rowDataPlotDefaults}},
#' \code{\link{sampAssayPlotDefaults}},
#' \code{\link{heatMapPlotDefaults}},
#' \code{\link{rowStatTableDefaults}}
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' redDimPlotDefaults(sce, n=1)
#' rowStatTableDefaults(sce, n=1)
NULL

#' Reduced dimension plot defaults
#'
#' Create default settings for reduced dimension plot panels in the iSEE interface.
#'
#' @param se A SummarizedExperiment object.
#' @param number An integer scalar, specifying the maximum number of reduced dimension plots that can be added to the interface.
#'
#' @details
#' Parameters available to reduced dimension plots are:
#' \describe{
#' \item{\code{Type}:}{Integer, which entry of \code{reducedDims(se)} should be shown?
#' Defaults to 1, i.e., the first entry.
#' Alternatively, a string can be supplied containing the name of the reduced dimension field, if \code{reducedDims(se)} has names.}
#' \item{\code{XAxis}:}{Integer, which component should be shown on the x-axis?
#' Defaults to 1.}
#' \item{\code{YAxis}:}{Integer, which component should be shown on the y-axis?
#' Defaults to 2.}
#' }
#'
#' All column-based parameters described in \code{?"\link{iSEE point parameters}"} are applicable.
#' All plot-based parameters described in \code{?"\link{iSEE selection parameters}"} are applicable.
#' 
#' @return
#' A DataFrame containing default settings for parameters of each of \code{number} reduced dimension panels. 
#' 
#' @author Aaron Lun
#'
#' @export
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom SingleCellExperiment reducedDim
#'
#' @seealso
#' \code{?"\link{iSEE point parameters}"},
#' \code{?"\link{iSEE selection parameters}"}
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' redDimPlotDefaults(sce, n=1)
redDimPlotDefaults <- function(se, number) {
    waszero <- number==0 # To ensure that we define all the fields with the right types.
    if (waszero) number <- 1

    out <- new("DataFrame", nrows=as.integer(number))
    out[[.redDimType]] <- 1L
    out[[.redDimXAxis]] <- 1L
    out[[.redDimYAxis]] <- min(2L, ncol(reducedDim(se))) # if first reduced dim type has only 1 dimension

    out <- .add_general_parameters_for_column_plots(out, se)
    if (waszero) out <- out[0,,drop=FALSE]
    return(out)
}

#' Feature assay plot defaults
#'
#' Create default settings for feature assay plot panels in the iSEE interface.
#'
#' @param se A SummarizedExperiment object.
#' @param number An integer scalar, specifying the maximum number of feature assay plots that can be added to the interface.
#'
#' @details
#' Parameters available to feature assay plots are:
#' \describe{
#' \item{\code{YAxisFeatName}:}{Integer, the index of the feature for which to show the expression on the y-axis if \code{YAxis="Feature name"}.
#' Defaults to 1, i.e., the first feature in \code{se}.
#' Alternatively, a string can be supplied containing the name of the feature, i.e., the row name.}
#' \item{\code{YAxisRowTable}:}{Character, what row statistic table should be used to choose a feature to display on the y-axis?
#' Any setting will override \code{YAxisFeatName} upon initialization of the app.
#' Defaults to \code{"---"}, which means that no table will be used.}
#' \item{\code{Assay}:}{Integer, which assay should be used to supply the expression values shown on the y-axis?
#' Defaults to 1, i.e., the first assay in \code{se}.
#' Alternatively, a string can also be supplied containing the name of the assay, if \code{assays(se)} has names.}
#' \item{\code{XAxis}:}{Character, what type of variable should be shown on the x-axis?
#' Defaults to \code{"None"}, but can also be \code{"Row table"}, \code{"Column data"} or \code{"Feature name"}.}
#' \item{\code{XAxisColData}:}{Character, what column of \code{colData(se)} should be shown on the x-axis if \code{XAxis="Column data"}?
#' Defaults to the first entry of \code{colData(se)}.}
#' \item{\code{XAxisFeatName}:}{Integer, the index of the feature for which to show the expression on the x-axis if \code{XAxis="Feature name"}.
#' Defaults to 1, i.e., the first feature in \code{se}.
#' Alternatively, a string can be supplied containing the name of the feature.}
#' \item{\code{XAxisRowTable}:}{Character, which row statistic table should be used to choose a feature to put on the x-axis if \code{XAxis="Row table"}?
#' Any setting will override \code{XAxisFeatName} upon initialization of the app.
#' Defaults to \code{"---"}, which means that no table will be used.}
#' }
#'
#' All column-based parameters described in \code{?"\link{iSEE point parameters}"} are applicable.
#' All plot-based parameters described in \code{?"\link{iSEE selection parameters}"} are applicable.
#' 
#' @return
#' A DataFrame containing default settings for parameters of each of \code{number} feature assay panels. 
#'
#' @author Aaron Lun
#' 
#' @export
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData
#' @importFrom BiocGenerics colnames
#'
#' @seealso
#' \code{?"\link{iSEE point parameters}"},
#' \code{?"\link{iSEE selection parameters}"}
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' featAssayPlotDefaults(sce, n=1)
featAssayPlotDefaults <- function(se, number) {
    waszero <- number==0
    if (waszero) number <- 1

    def_assay <- .set_default_assay(se)
    covariates <- colnames(colData(se))

    out <- new("DataFrame", nrows=as.integer(number))
    out[[.featAssayAssay]] <- def_assay
    out[[.featAssayXAxis]] <- .featAssayXAxisNothingTitle
    out[[.featAssayXAxisColData]] <- covariates[1]
    out[[.featAssayXAxisFeatName]] <- 1L
    out[[.featAssayXAxisRowTable]] <- .noSelection
    out[[.featAssayYAxisFeatName]] <- 1L
    out[[.featAssayYAxisRowTable]] <- .noSelection

    out <- .add_general_parameters_for_column_plots(out, se)
    if (waszero) out <- out[0,,drop=FALSE]
    return(out)
}

#' Column data plot defaults
#'
#' Create default settings for column data plot panels in the iSEE interface.
#'
#' @param se A SummarizedExperiment object.
#' @param number An integer scalar, specifying the maximum number of column data plots that can be added to the interface.
#'
#' @details
#' Parameters available to column data plots are:
#' \describe{
#' \item{\code{YAxis}:}{Character, which column of \code{colData(se)} should be shown on the y-axis?
#' Defaults to the first entry of \code{colData(se)}.}
#' \item{\code{XAxis}:}{Character, what type of variable should be shown on the x-axis?
#' Defaults to \code{"None"}, but can also be \code{"Column data"}.}
#' \item{\code{XAxisColData}:}{Character, which column of \code{colData(se)} should be shown on the x-axis if \code{XAxis="Column data"}?
#' Defaults to the first entry of \code{colData(se)}.}
#' }
#'
#' All column-based parameters described in \code{?"\link{iSEE point parameters}"} are applicable.
#' All plot-based parameters described in \code{?"\link{iSEE selection parameters}"} are applicable.
#' 
#' @return
#' A DataFrame containing default settings for parameters of each of \code{number} column data panels. 
#' 
#' @author Aaron Lun
#'
#' @export
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData
#' @importFrom BiocGenerics colnames
#'
#' @seealso
#' \code{?"\link{iSEE point parameters}"},
#' \code{?"\link{iSEE selection parameters}"}
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' colDataPlotDefaults(sce, n=1)
colDataPlotDefaults <- function(se, number) {
    waszero <- number==0
    if (waszero) number <- 1

    covariates <- colnames(colData(se))

    out <- new("DataFrame", nrows=as.integer(number))
    out[[.colDataYAxis]] <- covariates[1]
    out[[.colDataXAxis]] <- .colDataXAxisNothingTitle
    out[[.colDataXAxisColData]] <- ifelse(length(covariates)==1L, covariates[1], covariates[2])

    out <- .add_general_parameters_for_column_plots(out, se)
    if (waszero) out <- out[0,,drop=FALSE]
    return(out)
}

#' Custom data plot defaults
#'
#' Create default settings for custom data plot panels in the iSEE interface.
#'
#' @param se A SummarizedExperiment object.
#' @param number An integer scalar, specifying the maximum number of custom data plots that can be added to the interface.
#'
#' @details
#' Data parameters available to custom data plots are:
#' \describe{
#' \item{\code{DataBoxOpen}:}{Logical, should the data parameter box be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{Function}:}{String, which function should be used to generate the ggplot for the curent panel?
#' Defaults to \code{"---"}, i.e., no coordinates are generated.}
#' \item{\code{Arguments}:}{String with multiple lines specifying the initial arguments for the function, see \code{vignette("custom", package="iSEE")} for details.
#' Defaults to an empty string.}
#' }
#' 
#' Selection parameters for custom data plots are:
#' \describe{
#' \item{\code{SelectBoxOpen}:}{Logical, should the selection parameter box be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{ColumnSource}:}{Character, which other plot should transmit sample selections to the current plot? 
#' Defaults to \code{"---"}, which means that no plot is used for point selection.}
#' \item{\code{RowSource}:}{Character, which other plot should transmit feature selections to the current plot? 
#' Defaults to \code{"---"}, which means that no plot is used for point selection.}
#' }
#'
#' @return
#' A DataFrame containing default settings for parameters of each of \code{number} custom data plot panels. 
#'
#' @author 
#' Aaron Lun, Kevin Rue-Albrecht
#'
#' @export
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' customDataPlotDefaults(sce, n=1)
customDataPlotDefaults <- function(se, number) {
    waszero <- number==0
    if (waszero) number <- 1

    out <- new("DataFrame", nrows=as.integer(number))
    out[[.customFun]] <- .noSelection
    out[[.customArgs]] <- ""
    out[[.customColSource]] <- .noSelection
    out[[.customRowSource]] <- .noSelection
    out[[.dataParamBoxOpen]] <- FALSE
    out[[.selectParamBoxOpen]] <- FALSE

    if (waszero) out <- out[0,,drop=FALSE]
    return(out)
}

#' Row statistics table defaults
#'
#' Create default settings for row statistics table panels in the iSEE interface.
#'
#' @param se A SummarizedExperiment object.
#' @param number An integer scalar, specifying the maximum number of row statistics tables that can be added to the interface.
#'
#' @details
#' Parameters available to row statistics tables are:
#' \describe{
#' \item{\code{Selected}:}{Integer, containing the index of the row to be initially selected.
#' Defaults to the first row, i.e., 1.
#' Alternatively, a string can be supplied containing the row name.}
#' \item{\code{Search}:}{Character, containing the initial value of the search field.
#' Defaults to an empty string.}
#' \item{\code{SearchColumns}:}{A list containing character vectors of length equal to the number of columns in \code{rowData(se)},
#' specifying the initial value of the search field for each column.
#' All entries default to an empty string.}
#' }
#'
#' All table-based parameters described in \code{?"\link{iSEE selection parameters}"} are applicable.
#'
#' @return
#' A DataFrame containing default settings for parameters of each of \code{number} row statistics table panels. 
#'
#' @author Aaron Lun 
#'
#' @export
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment rowData
#'
#' @seealso
#' \code{?"\link{iSEE selection parameters}"}
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' rowStatTableDefaults(sce, n=1)
rowStatTableDefaults <- function(se, number) {
    waszero <- number==0
    if (waszero) number <- 1

    out <- new("DataFrame", nrows=as.integer(number))
    out[[.rowStatSelected]] <- 1L
    out[[.rowStatSearch]] <- ""

    # Defining an empty search for each column of the rowData.
    colsearch <- character(ncol(rowData(se)))
    out[[.rowStatColSearch]] <- rep(list(colsearch), as.integer(number))

    # Defining the rowDataPlot from which point selections are received.
    out[[.selectParamBoxOpen]] <- FALSE
    out[[.selectByPlot]] <- .noSelection

    if (waszero) out <- out[0,,drop=FALSE]
    return(out)
}

#' Custom statistics table defaults
#'
#' Create default settings for custom statistics table panels in the iSEE interface.
#'
#' @param se A SummarizedExperiment object.
#' @param number An integer scalar, specifying the maximum number of custom statistics tables that can be added to the interface.
#'
#' @details
#' Data parameters available to custom data plots are:
#' \describe{
#' \item{\code{DataBoxOpen}:}{Logical, should the data parameter box be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{Function}:}{String, which function should be used to generate a data.frame?
#' Defaults to \code{"---"}, i.e., no coordinates are generated.}
#' \item{\code{Arguments}:}{String with multiple lines specifying the initial arguments for the function, see \code{vignette("custom", package="iSEE")} for details.
#' Defaults to an empty string.}
#' }
#' 
#' Selection parameters for custom data plots are:
#' \describe{
#' \item{\code{SelectBoxOpen}:}{Logical, should the selection parameter box be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{ColumnSource}:}{Character, which other plot should transmit sample selections to the current table? 
#' Defaults to \code{"---"}, which means that no plot is used for point selection.}
#' \item{\code{RowSource}:}{Character, which other plot should transmit feature selections to the current table? 
#' Defaults to \code{"---"}, which means that no plot is used for point selection.}
#' }
#'
#' @return
#' A DataFrame containing default settings for parameters of each of \code{number} custom statistics table panels. 
#'
#' @author
#' Aaron Lun, Kevin Rue-Albrecht
#'
#' @export
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' customStatTableDefaults(sce, n=1)
customStatTableDefaults <- function(se, number) {
    waszero <- number==0
    if (waszero) number <- 1

    out <- new("DataFrame", nrows=as.integer(number))
    out[[.customFun]] <- .noSelection
    out[[.customArgs]] <- ""
    out[[.customColSource]] <- .noSelection
    out[[.customRowSource]] <- .noSelection
    out[[.customStatSearch]] <- ""

    out[[.dataParamBoxOpen]] <- FALSE
    out[[.selectParamBoxOpen]] <- FALSE

    if (waszero) out <- out[0,,drop=FALSE]
    return(out)
}

#' Row data plot defaults
#'
#' Create default settings for row data plot panels in the iSEE interface.
#'
#' @param se A SummarizedExperiment object.
#' @param number An integer scalar, specifying the maximum number of row data plots that can be added to the interface.
#'
#' @details
#' Parameters available to row data plots are:
#' \describe{
#' \item{\code{YAxis}:}{Character, which column of \code{rowData(se)} should be shown on the y-axis?
#' Defaults to the first entry of \code{colData(se)}.}
#' \item{\code{XAxis}:}{Character, what variable should be shown on the x-axis?
#' Defaults to \code{"None"}, but can also be \code{"Row data"} or \code{"Feature name"}.}
#' \item{\code{XAxisRowData}:}{Character, which column of \code{rowData(se)} should be shown on the x-axis if \code{XAxis="Row data"}?
#' Defaults to the first entry of \code{rowData(se)}.}
#' }
#'
#' All row-based parameters described in \code{?"\link{iSEE point parameters}"} are applicable.
#' All plot-based parameters described in \code{?"\link{iSEE selection parameters}"} are applicable.
#' 
#' @return
#' A DataFrame containing default settings for parameters of each of \code{number} row data panels. 
#' 
#' @author Aaron Lun
#'
#' @export
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment rowData
#' @importFrom BiocGenerics colnames
#'
#' @seealso
#' \code{?"\link{iSEE point parameters}"},
#' \code{?"\link{iSEE selection parameters}"}
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' rowDataPlotDefaults(sce, n=1)
rowDataPlotDefaults <- function(se, number) {
    waszero <- number==0
    if (waszero) number <- 1

    covariates <- colnames(rowData(se))

    out <- new("DataFrame", nrows=as.integer(number))
    out[[.rowDataYAxis]] <- covariates[1]
    out[[.rowDataXAxis]] <- .rowDataXAxisNothingTitle
    out[[.rowDataXAxisRowData]] <- ifelse(length(covariates)==1L, covariates[1], covariates[2])

    out <- .add_general_parameters_for_row_plots(out, se)
    if (waszero) out <- out[0,,drop=FALSE]
    return(out)
}

#' Sample assay plot defaults
#'
#' Create default settings for sample assay plot panels in the iSEE interface.
#'
#' @param se A SummarizedExperiment object.
#' @param number An integer scalar, specifying the maximum number of sample assay plots that can be added to the interface.
#'
#' @details
#' Parameters available to sample assay plots are:
#' \describe{
#' \item{\code{YAxis}:}{Integer, which column of \code{se} should be shown on the y-axis?
#' Defaults to 1, i.e., the first column.
#' Alternatively, a character field can be supplied containing the name of the column.}
#' \item{\code{Assay}:}{Integer, which assay should be used to supply the expression values shown on the y-axis?
#' Defaults to 1, i.e., the first assay in \code{se}.
#' Alternatively, a string can also be supplied containing the name of the assay, if \code{assays(se)} has names.}
#' \item{\code{XAxis}:}{Character, what variable should be shown on the x-axis?
#' Defaults to \code{"None"}, but can also be \code{"Row data"} or \code{"Sample"}.}
#' \item{\code{XAxisRowData}:}{Character, which column of \code{rowData(se)} should be shown on the x-axis if \code{XAxis="Row data"}?
#' Defaults to the first entry of \code{rowData(se)}.}
#' \item{\code{XAxisSample}:}{Integer, which column of \code{se} should be shown on the x-axis?
#' Defaults to 2 if \code{se} contains multiple columns, otherwise it is set to 1.
#' Alternatively, a character field can be supplied containing the name of the column.}
#' }
#'
#' All row-based parameters described in \code{?"\link{iSEE point parameters}"} are applicable.
#' All plot-based parameters described in \code{?"\link{iSEE selection parameters}"} are applicable.
#' 
#' @return
#' A DataFrame containing default settings for parameters of each of \code{number} sample assay panels. 
#'
#' @author Charlotte Soneson
#'
#' @export
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment rowData
#' @importFrom BiocGenerics colnames
#'
#' @seealso
#' \code{?"\link{iSEE point parameters}"},
#' \code{?"\link{iSEE selection parameters}"}
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' sampAssayPlotDefaults(sce, n=1)
sampAssayPlotDefaults <- function(se, number) {
    waszero <- number==0
    if (waszero) number <- 1

    covariates <- colnames(rowData(se))
    def_assay <- .set_default_assay(se)

    out <- new("DataFrame", nrows=as.integer(number))
    out[[.sampAssayYAxis]] <- 1L
    out[[.sampAssayAssay]] <- def_assay
    out[[.sampAssayXAxis]] <- .sampAssayXAxisNothingTitle
    out[[.sampAssayXAxisRowData]] <- covariates[1]
    out[[.sampAssayXAxisSample]] <- ifelse(ncol(se)==1L, 1L, 2L)

    out <- .add_general_parameters_for_row_plots(out, se)
    if (waszero) out <- out[0,,drop=FALSE]
    return(out)
}

#' Heatmap defaults
#'
#' Create default settings for heatmap panels in the iSEE interface.
#'
#' @param se A SummarizedExperiment object.
#' @param number An integer scalar, specifying the maximum number of heatmaps that can be added to the interface.
#'
#' @details
#' The features/rows to be used in the construction of the heatmap are specified with:
#' \describe{
#' \item{\code{FeatName}:}{List of length equal to the number of panels.
#' Each list entry corresponds to a panel and should be an integer vector with the indices of the feature(s) for which to show the expression in the heatmap.
#' Defaults to \code{1L} for each panel, i.e., the first feature in \code{se}.
#' Alternatively, a character vector can be supplied containing the names of the features.}
#' \item{\code{Assay}:}{Integer, which assay should be used to supply the expression values shown on the y-axis?
#' Defaults to 1, i.e., the first assay in \code{se}.
#' Alternatively, a string can also be supplied containing the name of the assay, if \code{assays(se)} has names.}
#' \item{\code{FeatNameSource}:}{Character, which other panel should be used to choose the features to show in the heatmap?
#'  Defaults to \code{"---"}, which means that no panel is used for feature selection.}
#' \item{\code{FeatNameBoxOpen}:}{Logical, should the feature selection box be open upon initialization?
#' Defaults to \code{FALSE}.}
#' }
#'
#' The column metadata variables control the ordering of the samples in the heatmap.
#' They can be controlled with:
#' \describe{
#' \item{\code{ColData}:}{List of length equal to the number of panels.
#' Each list entry corresponds to a panel and should contain a character vector specifying the field(s) of \code{colData(se)} that should be used to order the samples in the heatmap.
#' Note that these fields will also appear as annotation bars.
#' Each character vector defaults to the first entry of \code{colData(se)}.}
#' \item{\code{ColDataBoxOpen}:}{Logical, should the column data selection box be open upon initialization?
#' Defaults to \code{FALSE}.}
#' }
#'
#' A variety of parameters are available to control the color scale of the heatmap.
#' They can be specified with:
#' \describe{
#' \item{\code{ColorBoxOpen}:}{Logical, should the color selection panel for the heatmap be open upon initialization?
#' Defaults to \code{FALSE}.}
#' \item{\code{CenterScale}:}{List of length equal to the number of panels.
#' Each list entry corresponds to a panel and contains a character vector specifying whether each row of expression values should be mean-centered and/or scaled to unit variance.
#' Defaults to \code{"Centered"}.
#' Users can set it to \code{c("Centered", "Scaled")} to obtain mean-centered and unit-scaled rows.}
#' \item{\code{Lower}:}{Numeric, what should be the lower bound of the color scale for the values in the heatmap? All values below this threshold will be shown in the same color.
#' Defaults to -Inf, meaning that the lowest value in the data matrix will be used.}
#' \item{\code{Upper}:}{Numeric, what should be the upper bound of the color scale for the values in the heatmap? All values above this threshold will be shown in the same color.
#' Defaults to Inf, meaning that the highest value in the data matrix will be used.}
#' \item{\code{ColorScale}:}{Character, what color scale (in the form low-mid-high) should be used to color the heatmap when values are centered?
#' Defaults to \code{"purple-black-yellow"}.}
#' }
#'
#' The \code{ZoomData} field for heatmaps should contain an integer vector of consecutive indices to zoom into from the full heatmap.
#' This vector will subset the entries in \code{FeatName} for a given panel.
#' This defaults to \code{NULL}, i.e., all specified features in \code{FeatName} are shown.
#'
#' All plot-based parameters described in \code{?"\link{iSEE selection parameters}"} are also applicable.
#'
#' @return
#' A DataFrame containing default settings for parameters of each of \code{number} heatmap panels. 
#' 
#' @author Charlotte Soneson
#'
#' @export
#' @importFrom methods new
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom SummarizedExperiment colData
#' @importFrom BiocGenerics colnames
#'
#' @seealso
#' \code{?"\link{iSEE selection parameters}"}
#'
#' @examples
#' example(SingleCellExperiment, echo=FALSE) # mock up 'sce'.
#' heatMapPlotDefaults(sce, n=1)
heatMapPlotDefaults <- function(se, number) {
    waszero <- number==0 # To ensure that we define all the fields with the right types.
    if (waszero) number <- 1

    def_assay <- .set_default_assay(se)

    out <- new("DataFrame", nrows=as.integer(number))
    out[[.heatMapAssay]] <- def_assay
    out[[.heatMapFeatNameBoxOpen]] <- FALSE
    out[[.heatMapFeatName]] <- rep(list(1L), nrow(out))

    out[[.heatMapColDataBoxOpen]] <- FALSE
    out[[.heatMapColData]] <- rep(list(colnames(colData(se))[1]), nrow(out))
    out[[.heatMapImportSource]] <- .noSelection

    out[[.heatMapCenterScale]] <- rep(list(.heatMapCenterTitle), nrow(out))
    out[[.heatMapLower]] <- -Inf
    out[[.heatMapUpper]] <- Inf
    out[[.heatMapCenteredColors]] <- "purple-black-yellow"

    out[[.zoomData]] <- rep(list(NULL), nrow(out))

    out[[.selectParamBoxOpen]] <- FALSE
    out[[.selectByPlot]] <- .noSelection
    out[[.selectEffect]] <- .selectTransTitle
    out[[.selectTransAlpha]] <- 0.1
    out[[.selectColor]] <- "red"

    if (waszero) out <- out[0,,drop=FALSE]
    return(out)
}

#' Override default parameters
#'
#' Override the default settings of various parameters with whatever the user has supplied.
#'
#' @param def A DataFrame of default values, generated using \code{\link{redDimPlotDefaults}} or similar functions.
#' @param usr A DataFrame or data.frame of user-specified values, to use to replace the defaults.
#'
#' @return A DataFrame with the default parameter settings replaced by user-specified values, where appropriate.
#'
#' @details
#' Not all arguments in \code{def} need to be specified in \code{usr}.
#' Parameters will only be overridden for the specified arguments.
#'
#' This function expects that \code{nrow(def)} is greater than or equal to \code{nrow(usr)}.
#' Parameters in \code{def} will only be overwritten for the first \code{nrow(usr)} panels.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_override_defaults
#' @seealso
#' \code{?\link{defaults}},
#' \code{\link{.setup_memory}}
#' @importFrom BiocGenerics colnames
.override_defaults <- function(def, usr)
{
    ndef <- nrow(def)
    nusr <- nrow(usr)
    stopifnot(ndef >= nusr)
    replacement <- seq_len(nusr)

    for (x in colnames(usr)) {
        if (!x %in% colnames(def)) {
            warning(sprintf("unknown field '%s' in user-specified settings", x))
            next
        }

        # This method is safer than direct subset assignment,
        # as it works properly for lists.
        tmp <- def[[x]]
        tmp[replacement] <- usr[[x]]
        def[[x]] <- tmp
    }

    return(def)
}

#' Add general plot defaults
#'
#' Add default values for general plot parameters, including row and column-based plots.
#'
#' @param incoming A DataFrame with non-zero number of rows, containing default parameters that have already been filled for specific panel types.
#'
#' @return A DataFrame with additional fields for general plot parameters, filled with default values.
#'
#' @details
#' The \code{.add_general_parameters} function adds general parameters such as parameter box opening flags,
#' point selection specifications, and zoom, brush and lasso data fields.
#' All parameters are initialized at their default values.
#'
#' The \code{.add_general_parameters_for_column_plots} function adds general parameters for column-based plots,
#' while the \code{.add_general_parameters_for_row_plots} function adds them for row-based plots.
#' These mainly differ in how colouring is performed.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_add_general_parameters
#' @seealso
#' \code{\link{defaults}}
.add_general_parameters <- function(incoming) {
    incoming[[.dataParamBoxOpen]] <- FALSE
    incoming[[.visualParamBoxOpen]] <- FALSE
    incoming[[.selectParamBoxOpen]] <- FALSE

    incoming[[.selectByPlot]] <- .noSelection
    incoming[[.selectEffect]] <- .selectTransTitle
    incoming[[.selectTransAlpha]] <- 0.1
    incoming[[.selectColor]] <- "red"
    incoming[[.brushData]] <- rep(list(NULL), nrow(incoming))

    incoming[[.visualParamChoice]] <- rep(list(.visualParamChoiceColorTitle), nrow(incoming))

    incoming[[.plotPointSize]] <- 1
    incoming[[.plotPointAlpha]] <- 1
    incoming[[.plotPointDownsample]] <- FALSE
    incoming[[.plotPointSampleRes]] <- 200

    incoming[[.plotFontSize]] <- 1
    incoming[[.plotLegendPosition]] <- .plotLegendBottomTitle

    incoming[[.zoomData]] <- rep(list(NULL), nrow(incoming))
    incoming[[.lassoData]] <- rep(list(NULL), nrow(incoming))

    incoming[[.contourAddTitle]] <- FALSE
    incoming[[.contourColor]] <- "blue"
    return(incoming)
}

#' @param se A SummarizedExperiment object.
#' @rdname INTERNAL_add_general_parameters
#' @importFrom BiocGenerics colnames
#' @importFrom SummarizedExperiment colData
.add_general_parameters_for_column_plots <- function(incoming, se) {
    incoming <- .add_general_parameters(incoming)

    def_assay <- .set_default_assay(se)
    def_cov <- colnames(colData(se))[1]

    any_discrete <- .get_internal_info(se, "column_groupable", empty_fail=FALSE)  # if this is run internally, use precomputed; otherwise recompute.
    if (is.null(any_discrete)) {
        any_discrete <- colnames(colData(se))[.which_groupable(colData(se))]
    }
    dev_discrete <- any_discrete[1]

    incoming[[.colorByField]] <- .colorByNothingTitle
    incoming[[.colorByDefaultColor]] <- "black"
    incoming[[.colorByColData]] <- def_cov

    incoming[[.shapeByField]] <- .shapeByNothingTitle
    incoming[[.shapeByColData]] <- dev_discrete

    incoming[[.colorByRowTable]] <- .noSelection
    incoming[[.colorByFeatName]] <- 1L
    incoming[[.colorByFeatNameAssay]] <- def_assay

    incoming[[.facetByRow]] <- FALSE
    incoming[[.facetByColumn]] <- FALSE
    incoming[[.facetRowsByColData]] <- dev_discrete
    incoming[[.facetColumnsByColData]] <- dev_discrete

    return(incoming)
}

#' @rdname INTERNAL_add_general_parameters
#' @importFrom BiocGenerics colnames
#' @importFrom SummarizedExperiment rowData
.add_general_parameters_for_row_plots <- function(incoming, se) {
    incoming <- .add_general_parameters(incoming)

    def_cov <- colnames(rowData(se))[1]
    any_discrete <- .get_internal_info(se, "row_groupable", empty_fail=FALSE) # if this is run internally, use precomputed; otherwise recompute.
    if (is.null(any_discrete)) {
        any_discrete <- colnames(rowData(se))[.which_groupable(rowData(se))]
    }
    dev_discrete <- any_discrete[1]

    incoming[[.colorByField]] <- .colorByNothingTitle
    incoming[[.colorByDefaultColor]] <- "black"
    incoming[[.colorByRowData]] <- def_cov

    incoming[[.shapeByField]] <- .shapeByNothingTitle
    incoming[[.shapeByRowData]] <- dev_discrete

    incoming[[.colorByRowTable]] <- .noSelection
    incoming[[.colorByFeatName]] <- 1L
    incoming[[.colorByFeatNameColor]] <- "red"

    incoming[[.facetByRow]] <- FALSE
    incoming[[.facetByColumn]] <- FALSE
    incoming[[.facetRowsByRowData]] <- dev_discrete
    incoming[[.facetColumnsByRowData]] <- dev_discrete

    return(incoming)
}

#' Set the default assay
#'
#' Identifies the index of the assay containing the log-count matrix.
#'
#' @param se A SummarizedExperiment object.
#'
#' @return An integer scalar containing the index of the \code{"logcounts"} assay, if available; otherwise 1L.
#'
#' @details
#' It usually makes most sense to perform visualization (colouring, examination of expression) on the log-expression values,
#' as this provides good dynamic range and easy interpretability.
#' This function conveniently identifies the index of the assay named \code{"logcounts"} for use as the default when it is available.
#'
#' @author Aaron Lun
#' @rdname INTERNAL_set_default_assay
#' @seealso
#' \code{?\link{defaults}}
#' @importFrom SummarizedExperiment assayNames
.set_default_assay <- function(se) {
    def_assay <- which(assayNames(se)=="logcounts")
    if (length(def_assay) == 0L) {
        return(1L)
    } else {
        return(def_assay[1])
    }
}

#' Set the default discrete covariate in a DataFrame
#'
#' @param x A DataFrame (or equivalent).
#'
#' @return An integer scalar containing the index of the first groupable covariate, if available; otherwise 0L.
#'
#' @author Kevin Rue-Albrecht
#'
#' @rdname INTERNAL_set_default_discrete
#' @seealso
#' \code{?\link{defaults}}
.set_default_discrete <- function(x) {
    discrete_data <- .which_groupable(x)
    if (length(discrete_data) == 0L) {
        return(0L)
    } else {
        return(discrete_data[1])
    }
}
