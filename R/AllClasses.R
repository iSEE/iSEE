
# Validation function ----

.valid.Colormap <- function(object){
    # To avoid later checking inside the app, this object should be stored in a
    # slot of its parent SummarizedExperiment that would check its validity
    # (e.g. all assay colormaps should be present in the parent, etc.)
    # Note that default colormaps will be provided for elements
    # of the parent that do not have a defined colormap.

    errors <- c()

    # Different types of checks
    free_lists <- c("assays")
    named_lists <- c("colData", "rowData")
    controlled_lists <- c("all_discrete", "all_continuous")
    function_slots <- c("global_discrete", "global_continuous")

    controlled_names <- c("assays", "colData", "rowData")

    # Check that all colormaps are functions
    # and that they return non-empty vectors (character or color)
    for (slotname in c(free_lists, named_lists, controlled_lists)){
        slotmaps <- slot(object, slotname)
        check_function <- vapply(slotmaps, "is.function", logical(1))
        if (!all(check_function)){
            errors <- c(errors, sprintf(
                "Colormap `%s` in slot `%s` is not a function",
                names(slotmaps)[!check_function],
                slotname
            ))
            return(errors)
        }
    }

    for (slotname in named_lists){
        slotmaps <- slot(object, slotname)
        # Check that all colormaps are named
        check_named <- names(slotmaps) != ""
        if (!all(check_named)){
            errors <- c(errors, sprintf(
                "Colormap #%s in slot `%s` must be named",
                which(!check_named),
                slotname
            ))
        }
    }

    for (slotname in controlled_lists){
        slotmaps <- slot(object, slotname)
        # Check that all colormaps have the appropriate names
        if (!identical(names(slotmaps), controlled_names)){
            errors <- c(errors, sprintf(
                "Colormap in slot `%s` must be named %s",
                slotname,
                paste(deparse(controlled_names), collapse="")
            ))
        }
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
}

# ExperimentColorMap definition ----

.nullColorMap <- function(n){
    return(NULL)
}

setClass("ExperimentColorMap",
    contains="Vector",
    representation(
        # each slot has a list of closures
        assays="list",
        colData="list",
        rowData="list",
        all_discrete="list",
        all_continuous="list",
        global_discrete="function",
        global_continuous="function"
    ),
    prototype(
        assays=list(),
        colData=list(),
        rowData=list(),
        all_discrete=list(
            assays=.nullColorMap,
            colData=.nullColorMap,
            rowData=.nullColorMap
        ),
        all_continuous=list(
            assays=.nullColorMap,
            colData=.nullColorMap,
            rowData=.nullColorMap
        ),
        global_discrete=.nullColorMap,
        global_continuous=.nullColorMap
    ),
    validity=.valid.Colormap
)

####################################################

collated <- character(0)

.organizationId <- "PanelId"
.organizationWidth <- "PanelWidth"
.organizationHeight <- "PanelHeight"

collated[.organizationId] <- "integer"
collated[.organizationHeight] <- "integer"
collated[.organizationWidth] <- "integer"

.selectParamBoxOpen <- "SelectionBoxOpen"
.selectRowSource <- "RowSelectionSource"
.selectColSource <- "ColumnSelectionSource"

collated[.selectParamBoxOpen] <- "logical"
collated[.selectRowSource] <- "character"
collated[.selectColSource] <- "character"

.dataParamBoxOpen <- "DataBoxOpen"

collated[.dataParamBoxOpen] <- "logical"

.selectRowDynamic <- "RowSelectionDynamicSource"
.selectRowType <- "RowSelectionType"
.selectRowSaved <- "RowSelectionSaved"

collated[.selectRowDynamic] <- "logical"
collated[.selectRowType] <- "character"
collated[.selectRowSaved] <- "integer"

.selectColDynamic <- "ColumnSelectionDynamicSource"
.selectColType <- "ColumnSelectionType"
.selectColSaved <- "ColumnSelectionSaved"

collated[.selectColDynamic] <- "logical"
collated[.selectColType] <- "character"
collated[.selectColSaved] <- "integer"

# Practically, this is only a DotPlot feature, but we put it here otherwise the
# Saved concept is not generic.
.multiSelectHistory <- "SelectionHistory"

collated[.multiSelectHistory] <- "list"

#' @export
setClass("Panel", contains="VIRTUAL", slots=collated)

####################################################

collated <- character(0)

.facetByRow <- "FacetByRow"
.facetByColumn <- "FacetByColumn"

collated[.facetByRow] <- "character"
collated[.facetByColumn] <- "character"

.colorByField <- "ColorBy"
.colorByDefaultColor <- "ColorByDefaultColor"
.colorByFeatName <- "ColorByFeatureName"
.colorByRowTable <- "ColorByFeatureSource"
.colorByFeatDynamic <- "ColorByFeatureDynamicSource"
.colorBySampName <- "ColorBySampleName"
.colorByColTable <- "ColorBySampleSource"
.colorBySampDynamic <- "ColorBySampleDynamicSource"

collated[.colorByField] <- "character"
collated[.colorByDefaultColor] <- "character"
collated[.colorByFeatName] <- "character"
collated[.colorByRowTable] <- "character"
collated[.colorByFeatDynamic] <- "logical"
collated[.colorBySampName] <- "character"
collated[.colorByColTable] <- "character"
collated[.colorBySampDynamic] <- "logical"

.shapeByField <- "ShapeBy"

collated[.shapeByField] <- "character"

.sizeByField <- "SizeBy"

collated[.sizeByField] <- "character"

.selectEffect <- "SelectionEffect"
.selectColor <- "SelectionColor"
.selectTransAlpha <- "SelectionAlpha"

collated[.selectEffect] <- "character"
collated[.selectColor] <- "character"
collated[.selectTransAlpha] <- "numeric"

.zoomData <- "ZoomData"
.brushData <- "BrushData"

collated[.zoomData] <- "numeric"
collated[.brushData] <- "list"

.visualParamBoxOpen <- "VisualBoxOpen"
.visualParamChoice <- "VisualChoices"

collated[.visualParamBoxOpen] <- "logical"
collated[.visualParamChoice] <- "character"

.contourColor <- "ContourColor"
.contourAdd <- "ContourAdd"

collated[.contourAdd] <- "logical"
collated[.contourColor] <- "character"

.plotPointSize <- "PointSize"
.plotPointAlpha <- "PointAlpha"
.plotPointDownsample <- "Downsample"
.plotPointSampleRes <- "DownsampleResolution"

collated[.plotPointSize] <- "numeric"
collated[.plotPointAlpha] <- "numeric"
collated[.plotPointDownsample] <- "logical"
collated[.plotPointSampleRes] <- "numeric"

.plotFontSize <- "FontSize"
.legendPointSize <- "LegendPointSize"
.plotLegendPosition <- "LegendPosition"

collated[.plotFontSize] <- "numeric"
collated[.legendPointSize] <- "numeric"
collated[.plotLegendPosition] <- "character"

#' @export
setClass("DotPlot", contains=c("Panel", "VIRTUAL"), slots=collated)

####################################################

collated <- character(0)

.colorByColData <- "ColorByColumnData"
.colorByFeatNameAssay <- "ColorByFeatureNameAssay"
.colorBySampNameColor <- "ColorBySampleNameColor"

collated[.colorByColData] <- "character"
collated[.colorByFeatNameAssay] <- "character"
collated[.colorBySampNameColor] <- "character"

.shapeByColData <- "ShapeByColumnData"

collated[.shapeByColData] <- "character"

.sizeByColData <- "SizeByColumnData"

collated[.sizeByColData] <- "character"

#' @export
setClass("ColumnDotPlot", contains=c("DotPlot", "VIRTUAL"), slots=collated)

####################################################

collated <- character(0)

.colorByRowData <- "ColorByRowData"
.colorBySampNameAssay <- "ColorBySampleNameAssay"
.colorByFeatNameColor <- "ColorByFeatureNameColor"

collated[.colorByRowData] <- "character"
collated[.colorBySampNameAssay] <- "character"
collated[.colorByFeatNameColor] <- "character"

.shapeByRowData <- "ShapeByRowData"

collated[.shapeByRowData] <- "character"

.sizeByRowData <- "SizeByRowData"

collated[.sizeByRowData] <- "character"

#' @export
setClass("RowDotPlot", contains=c("DotPlot", "VIRTUAL"), slots=collated)

####################################################

.redDimType <- "Type"
.redDimXAxis <- "XAxis"
.redDimYAxis <- "YAxis"

collated <- character(0)
collated[.redDimType] <- "character"
collated[.redDimXAxis] <- "integer"
collated[.redDimYAxis] <- "integer"

#' @export
setClass("ReducedDimensionPlot", contains="ColumnDotPlot", slots=collated)

####################################################

.featAssayAssay <- "Assay"
.featAssayXAxis <- "XAxis"
.featAssayXAxisColData <- "XAxisColumnData"

.featAssayXAxisFeatName <- "XAxisFeatureName"
.featAssayXAxisRowTable <- "XAxisFeatureSource"
.featAssayXAxisFeatDynamic <- "XAxisFeatureDynamicSource"

.featAssayYAxisFeatName <- "YAxisFeatureName"
.featAssayYAxisRowTable <- "YAxisFeatureSource"
.featAssayYAxisFeatDynamic <- "YAxisFeatureDynamicSource"

collated <- character(0)
collated[.featAssayAssay] <- "character"
collated[.featAssayXAxis] <- "character"
collated[.featAssayXAxisColData] <- "character"

collated[.featAssayXAxisFeatName] <- "character"
collated[.featAssayXAxisRowTable] <- "character"
collated[.featAssayXAxisFeatDynamic] <- "logical"

collated[.featAssayYAxisFeatName] <- "character"
collated[.featAssayYAxisRowTable] <- "character"
collated[.featAssayYAxisFeatDynamic] <- "logical"

#' @export
setClass("FeatureAssayPlot", contains="ColumnDotPlot", slots=collated)

####################################################

.colDataYAxis <- "YAxis"
.colDataXAxis <- "XAxis"
.colDataXAxisColData <- "XAxisColumnData"

collated <- character(0)
collated[.colDataXAxis] <- "character"
collated[.colDataYAxis] <- "character"
collated[.colDataXAxisColData] <- "character"

#' @export
setClass("ColumnDataPlot", contains="ColumnDotPlot", slots=collated)

####################################################

.rowDataYAxis <- "YAxis"
.rowDataXAxis <- "XAxis"
.rowDataXAxisRowData <- "XAxisRowData"

collated <- character(0)
collated[.rowDataXAxis] <- "character"
collated[.rowDataYAxis] <- "character"
collated[.rowDataXAxisRowData] <- "character"

#' @export
setClass("RowDataPlot", contains="RowDotPlot", slots=collated)

####################################################

.sampAssayAssay <- "Assay"
.sampAssayXAxis <- "XAxis"
.sampAssayXAxisRowData <- "XAxisRowData"

.sampAssayXAxisSampName <- "XAxisSampleName"
.sampAssayXAxisColTable <- "XAxisSampleSource"
.sampAssayXAxisSampDynamic <- "XAxisSampleDynamicSource"

.sampAssayYAxisSampName <- "YAxisSampleName"
.sampAssayYAxisColTable <- "YAxisSampleSource"
.sampAssayYAxisSampDynamic <- "YAxisSampleDynamicSource"

collated <- character(0)
collated[.sampAssayAssay] <- "character"
collated[.sampAssayXAxis] <- "character"
collated[.sampAssayXAxisRowData] <- "character"

collated[.sampAssayXAxisSampName] <- "character"
collated[.sampAssayXAxisColTable] <- "character"
collated[.sampAssayXAxisSampDynamic] <- "logical"

collated[.sampAssayYAxisSampName] <- "character"
collated[.sampAssayYAxisColTable] <- "character"
collated[.sampAssayYAxisSampDynamic] <- "logical"

#' @export
setClass("SampleAssayPlot", contains="RowDotPlot", slots=collated)

####################################################

.TableSelected <- "Selected"
.TableSearch <- "Search"
.TableColSearch <- "SearchColumns"

collated <- character(0)
collated[.TableSelected] <- "character"
collated[.TableSearch] <- "character"
collated[.TableColSearch] <- "character"

#' @export
setClass("Table", contains=c("Panel", "VIRTUAL"), slots=collated)

#' @export
setClass("RowTable", contains="Table", representation("VIRTUAL"))

#' @export
setClass("ColumnTable", contains="Table", representation("VIRTUAL"))

#' @export
setClass("RowDataTable", contains="RowTable")

#' @export
setClass("ColumnDataTable", contains="ColumnTable")

####################################################

.heatMapAssay <- "Assay"
.heatMapCustomFeatNames <- "CustomRows"
.heatMapFeatNameText <- "CustomRowsText"
.heatMapClusterFeatures <- "ClusterRows"
.heatMapClusterDistanceFeatures <- "ClusterRowsDistance"
.heatMapClusterMethodFeatures <- "ClusterRowsMethod"

.heatMapColData <- "ColumnData"
.heatMapRowData <- "RowData"

.heatMapCustomAssayBounds <- "CustomBounds"
.assayLowerBound <- "LowerBound"
.assayUpperBound <- "UpperBound"
.assayCenterRows <- "AssayCenterRows"
.assayScaleRows <- "AssayScaleRows"
.heatMapCenteredColormap <- "DivergentColormap"

.showDimnames <- "ShowDimNames"
.plotLegendDirection <- "LegendDirection"

collated <- character(0)

collated[.heatMapAssay] <- "character"
collated[.heatMapCustomFeatNames] <- "logical"
collated[.heatMapFeatNameText] <- "character"
collated[.heatMapClusterFeatures] <- "logical"
collated[.heatMapClusterDistanceFeatures] <- "character"
collated[.heatMapClusterMethodFeatures] <- "character"
collated[.dataParamBoxOpen] <- "logical"

collated[.visualParamChoice] <- "character"
collated[.heatMapColData] <- "character"
collated[.heatMapRowData] <- "character"

collated[.heatMapCustomAssayBounds] <- "logical"
collated[.assayLowerBound] <- "numeric"
collated[.assayUpperBound] <- "numeric"
collated[.assayCenterRows] <- "logical"
collated[.assayScaleRows] <- "logical"
collated[.heatMapCenteredColormap] <- "character"

collated[.showDimnames] <- "character"
collated[.plotLegendPosition] <- "character"
collated[.plotLegendDirection] <- "character"
collated[.visualParamBoxOpen] <- "logical"

collated[.selectEffect] <- "character"
collated[.selectColor] <- "character"

#' @export
setClass("ComplexHeatmapPlot", contains="Panel", slots=collated)
