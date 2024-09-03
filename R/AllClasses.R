
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
    contains="Annotated",
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

.packageVersion <- "VersionInfo"

collated[.packageVersion] <- "list"

.organizationId <- "PanelId"
#' @export
.organizationWidth <- "PanelWidth"
#' @export
.organizationHeight <- "PanelHeight"

collated[.organizationId] <- "integer"
collated[.organizationHeight] <- "integer"
collated[.organizationWidth] <- "integer"

.selectParamBoxOpen <- "SelectionBoxOpen"
.selectRowSource <- "RowSelectionSource"
.selectColumnSource <- "ColumnSelectionSource"

collated[.selectParamBoxOpen] <- "logical"
collated[.selectRowSource] <- "character"
collated[.selectColumnSource] <- "character"

#' @export
.dataParamBoxOpen <- "DataBoxOpen"

collated[.dataParamBoxOpen] <- "logical"

.selectRowDynamic <- "RowSelectionDynamicSource"

collated[.selectRowDynamic] <- "logical"

.selectColumnDynamic <- "ColumnSelectionDynamicSource"

collated[.selectColumnDynamic] <- "logical"

.selectRowRestrict <- "RowSelectionRestrict"
.selectColumnRestrict <- "ColumnSelectionRestrict"

collated[.selectRowRestrict] <- "logical"
collated[.selectColumnRestrict] <- "logical"

# Practically, this is only a DotPlot feature, but we put it here otherwise the
# Saved concept is not generic.
#' @export
.multiSelectHistory <- "SelectionHistory"

collated[.multiSelectHistory] <- "list"

#' @export
setClass("Panel", contains="VIRTUAL", slots=collated)

####################################################

collated <- character(0)

.facetRow <- "FacetRowBy"
.facetColumn <- "FacetColumnBy"

collated[.facetRow] <- "character"
collated[.facetColumn] <- "character"

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

.selectTransAlpha <- "SelectionAlpha"

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

.plotCustomLabels <- "CustomLabels"
.plotCustomLabelsText <- "CustomLabelsText"
.plotFontSize <- "FontSize"
.legendPointSize <- "LegendPointSize"
.plotLegendPosition <- "LegendPosition"

collated[.plotCustomLabels] <- "logical"
collated[.plotCustomLabelsText] <- "character"
collated[.plotFontSize] <- "numeric"
collated[.legendPointSize] <- "numeric"
collated[.plotLegendPosition] <- "character"

.plotHoverInfo <- "HoverInfo"

collated[.plotHoverInfo] <- "logical"

.plotLabelCenters <- "LabelCenters"
.plotLabelCentersBy <- "LabelCentersBy"
.plotLabelCentersColor <- "LabelCentersColor"

collated[.plotLabelCenters] <- "logical"
collated[.plotLabelCentersBy] <- "character"
collated[.plotLabelCentersColor] <- "character"

#' @export
setClass("DotPlot", contains=c("Panel", "VIRTUAL"), slots=collated)

####################################################

collated <- character(0)

.facetRowByColData <- "FacetRowByColData"
.facetColumnByColData <- "FacetColumnByColData"

collated[.facetRowByColData] <- "character"
collated[.facetColumnByColData] <- "character"

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

.tooltipColData <- "TooltipColumnData"
collated[.tooltipColData] <- "character"

#' @export
setClass("ColumnDotPlot", contains=c("DotPlot", "VIRTUAL"), slots=collated)

####################################################

collated <- character(0)

.facetRowByRowData <- "FacetRowByRowData"
.facetColumnByRowData <- "FacetColumnByRowData"

collated[.facetRowByRowData] <- "character"
collated[.facetColumnByRowData] <- "character"

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

.tooltipRowData <- "TooltipRowData"
collated[.tooltipRowData] <- "character"

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

.TableHidden <- "HiddenColumns"
collated[.TableHidden] <- "character"

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

.namesRowFontSize <- "NamesRowFontSize"
.namesColumnFontSize <- "NamesColumnFontSize"

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
collated[.namesRowFontSize] <- "numeric"
collated[.namesColumnFontSize] <- "numeric"

.heatMapShowSelection <- "ShowColumnSelection"
.heatMapOrderSelection <- "OrderColumnSelection"

collated[.heatMapShowSelection] <- "logical"
collated[.heatMapOrderSelection] <- "logical"

#' @export
setClass("ComplexHeatmapPlot", contains="Panel", slots=collated)
