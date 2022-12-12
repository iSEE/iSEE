
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

#' iSEE Panel Slot Names 
#'
#' Access the name of slots in \pkg{iSEE} panel classes.
#' 
#' The \code{iSEEslots} object exports the name of slots for \pkg{iSEE} panel classes, and should only be used by developers of packages that extend \pkg{iSEE} functionality.
#' The name of slots should be accessed using the \code{$} operator and the identifier of the slot name, e.g. `iSEEslots$packageVersion`.
#' 
#' Developers of new panels that extend \pkg{iSEE} functionality should add any new slot name to the \code{iSEEslots} object using a new slot identifier, e.g. \code{iSEEslots$myNewSlotIdentifier <- "MyNewSlotName"}.
#' 
#' @section Slot names for the virtual class `Panel`:
#' \describe{
#' \item{\code{packageVersion}}{Named list of package versions active when the panel object was generated or last updated.}
#' \item{\code{organizationId}}{Integer scalar specifying the width of the panel;
#' unique across panels of the same concrete class.}
#' \item{\code{organizationWidth}}{Width of the panel, in Shiny's grid layout units (minimum: 2, maximum 12).}
#' \item{\code{organizationHeight}}{Height of the panel, in pixels (minimum: 400, maximum 1000).}
#' }
#'
#' @author Kevin Rue-Albrecht
#' 
#' @name iSEEslots
#' @aliases iSEEslots
#' 
#' @examples 
#' iSEEslots
NULL

# .iSEEslots ----

#' @export
iSEEslots <- list(
    # Panel ----
    packageVersion = "VersionInfo",
    
    organizationId = "PanelId",
    organizationWidth = "PanelWidth",
    organizationHeight = "PanelHeight",
    
    selectParamBoxOpen = "SelectionBoxOpen",
    selectRowSource = "RowSelectionSource",
    selectColSource = "ColumnSelectionSource",
    
    dataParamBoxOpen = "DataBoxOpen",
    
    selectRowDynamic = "RowSelectionDynamicSource",
    
    selectColDynamic = "ColumnSelectionDynamicSource",
    
    selectRowRestrict = "RowSelectionRestrict",
    selectColRestrict = "ColumnSelectionRestrict",
    
    multiSelectHistory = "SelectionHistory",
    # DotPlot ----
    facetRow = "FacetRowBy",
    facetColumn = "FacetColumnBy",
    
    colorByField = "ColorBy",
    colorByDefaultColor = "ColorByDefaultColor",
    colorByFeatName = "ColorByFeatureName",
    colorByRowTable = "ColorByFeatureSource",
    colorByFeatDynamic = "ColorByFeatureDynamicSource",
    colorBySampName = "ColorBySampleName",
    colorByColTable = "ColorBySampleSource",
    colorBySampDynamic = "ColorBySampleDynamicSource",
    
    shapeByField = "ShapeBy",
    
    sizeByField = "SizeBy",
    
    selectTransAlpha = "SelectionAlpha",
    
    zoomData = "ZoomData",
    brushData = "BrushData",
    
    visualParamBoxOpen = "VisualBoxOpen",
    visualParamChoice = "VisualChoices",
    
    contourColor = "ContourColor",
    contourAdd = "ContourAdd",
    
    plotPointSize = "PointSize",
    plotPointAlpha = "PointAlpha",
    plotPointDownsample = "Downsample",
    plotPointSampleRes = "DownsampleResolution",
    
    plotCustomLabels = "CustomLabels",
    plotCustomLabelsText = "CustomLabelsText",
    plotFontSize = "FontSize",
    legendPointSize = "LegendPointSize",
    plotLegendPosition = "LegendPosition",
    
    plotHoverInfo = "HoverInfo",
    
    plotLabelCenters = "LabelCenters",
    plotLabelCentersBy = "LabelCentersBy",
    plotLabelCentersColor = "LabelCentersColor",
    # ColumnDotPlot ----
    facetRowByColData = "FacetRowByColData",
    facetColumnByColData = "FacetColumnByColData",

    colorByColData = "ColorByColumnData",
    colorByFeatNameAssay = "ColorByFeatureNameAssay",
    colorBySampNameColor = "ColorBySampleNameColor",

    shapeByColData = "ShapeByColumnData",

    sizeByColData = "SizeByColumnData",
    
    tooltipColData = "TooltipColumnData",
    # RowDotPlot ----
    facetRowByRowData = "FacetRowByRowData",
    facetColumnByRowData = "FacetColumnByRowData",

    colorByRowData = "ColorByRowData",
    colorBySampNameAssay = "ColorBySampleNameAssay",
    colorByFeatNameColor = "ColorByFeatureNameColor",
    
    shapeByRowData = "ShapeByRowData",
    
    sizeByRowData = "SizeByRowData",
    
    tooltipRowData = "TooltipRowData",
    # ReducedDimensionPlot ----
    redDimType = "Type",
    redDimXAxis = "XAxis",
    redDimYAxis = "YAxis",
    # FeatureAssayPlot ----
    featAssayAssay = "Assay",
    featAssayXAxis = "XAxis",
    featAssayXAxisColData = "XAxisColumnData",
    
    featAssayXAxisFeatName = "XAxisFeatureName",
    featAssayXAxisRowTable = "XAxisFeatureSource",
    featAssayXAxisFeatDynamic = "XAxisFeatureDynamicSource",
    
    featAssayYAxisFeatName = "YAxisFeatureName",
    featAssayYAxisRowTable = "YAxisFeatureSource",
    featAssayYAxisFeatDynamic = "YAxisFeatureDynamicSource",
    # ColumnDataPlot ----
    colDataYAxis = "YAxis",
    colDataXAxis = "XAxis",
    colDataXAxisColData = "XAxisColumnData",
    # RowDataPlot ----
    rowDataYAxis = "YAxis",
    rowDataXAxis = "XAxis",
    rowDataXAxisRowData = "XAxisRowData",
    # SampleAssayPlot ----
    sampAssayAssay = "Assay",
    sampAssayXAxis = "XAxis",
    sampAssayXAxisRowData = "XAxisRowData",
    
    sampAssayXAxisSampName = "XAxisSampleName",
    sampAssayXAxisColTable = "XAxisSampleSource",
    sampAssayXAxisSampDynamic = "XAxisSampleDynamicSource",
    
    sampAssayYAxisSampName = "YAxisSampleName",
    sampAssayYAxisColTable = "YAxisSampleSource",
    sampAssayYAxisSampDynamic = "YAxisSampleDynamicSource",
    # Table ----
    TableSelected = "Selected",
    TableSearch = "Search",
    TableColSearch = "SearchColumns",
    
    TableHidden = "HiddenColumns",
    # ComplexHeatmapPlot ----
    heatMapAssay = "Assay",
    heatMapCustomFeatNames = "CustomRows",
    heatMapFeatNameText = "CustomRowsText",
    heatMapClusterFeatures = "ClusterRows",
    heatMapClusterDistanceFeatures = "ClusterRowsDistance",
    heatMapClusterMethodFeatures = "ClusterRowsMethod",
    
    heatMapColData = "ColumnData",
    heatMapRowData = "RowData",
    
    heatMapCustomAssayBounds = "CustomBounds",
    assayLowerBound = "LowerBound",
    assayUpperBound = "UpperBound",
    assayCenterRows = "AssayCenterRows",
    assayScaleRows = "AssayScaleRows",
    heatMapCenteredColormap = "DivergentColormap",
    
    showDimnames = "ShowDimNames",
    plotLegendDirection = "LegendDirection",
    
    namesRowFontSize = "NamesRowFontSize",
    namesColumnFontSize = "NamesColumnFontSize",
    
    heatMapShowSelection = "ShowColumnSelection",
    heatMapOrderSelection = "OrderColumnSelection"
)

####################################################

collated <- character(0)

collated[iSEEslots$packageVersion] <- "list"

collated[iSEEslots$organizationId] <- "integer"
collated[iSEEslots$organizationHeight] <- "integer"
collated[iSEEslots$organizationWidth] <- "integer"

collated[iSEEslots$selectParamBoxOpen] <- "logical"
collated[iSEEslots$selectRowSource] <- "character"
collated[iSEEslots$selectColSource] <- "character"

collated[iSEEslots$dataParamBoxOpen] <- "logical"

collated[iSEEslots$selectRowDynamic] <- "logical"

collated[iSEEslots$selectColDynamic] <- "logical"

collated[iSEEslots$selectRowRestrict] <- "logical"
collated[iSEEslots$selectColRestrict] <- "logical"

# Practically, this is only a DotPlot feature, but we put it here otherwise the
# Saved concept is not generic.

collated[iSEEslots$multiSelectHistory] <- "list"

#' @export
setClass("Panel", contains="VIRTUAL", slots=collated)

####################################################

collated <- character(0)

collated[iSEEslots$facetRow] <- "character"
collated[iSEEslots$facetColumn] <- "character"

collated[iSEEslots$colorByField] <- "character"
collated[iSEEslots$colorByDefaultColor] <- "character"
collated[iSEEslots$colorByFeatName] <- "character"
collated[iSEEslots$colorByRowTable] <- "character"
collated[iSEEslots$colorByFeatDynamic] <- "logical"
collated[iSEEslots$colorBySampName] <- "character"
collated[iSEEslots$colorByColTable] <- "character"
collated[iSEEslots$colorBySampDynamic] <- "logical"

collated[iSEEslots$shapeByField] <- "character"

collated[iSEEslots$sizeByField] <- "character"

collated[iSEEslots$selectTransAlpha] <- "numeric"

collated[iSEEslots$brushData] <- "list"

collated[iSEEslots$visualParamBoxOpen] <- "logical"
collated[iSEEslots$visualParamChoice] <- "character"

collated[iSEEslots$contourAdd] <- "logical"
collated[iSEEslots$contourColor] <- "character"

collated[iSEEslots$plotPointSize] <- "numeric"
collated[iSEEslots$plotPointAlpha] <- "numeric"
collated[iSEEslots$plotPointDownsample] <- "logical"
collated[iSEEslots$plotPointSampleRes] <- "numeric"

collated[iSEEslots$plotCustomLabels] <- "logical"
collated[iSEEslots$plotCustomLabelsText] <- "character"
collated[iSEEslots$plotFontSize] <- "numeric"
collated[iSEEslots$legendPointSize] <- "numeric"
collated[iSEEslots$plotLegendPosition] <- "character"

collated[iSEEslots$plotHoverInfo] <- "logical"

collated[iSEEslots$plotLabelCenters] <- "logical"
collated[iSEEslots$plotLabelCentersBy] <- "character"
collated[iSEEslots$plotLabelCentersColor] <- "character"

#' @export
setClass("DotPlot", contains=c("Panel", "VIRTUAL"), slots=collated)

####################################################

collated <- character(0)

collated[iSEEslots$facetRowByColData] <- "character"
collated[iSEEslots$facetColumnByColData] <- "character"

collated[iSEEslots$colorByColData] <- "character"
collated[iSEEslots$colorByFeatNameAssay] <- "character"
collated[iSEEslots$colorBySampNameColor] <- "character"

collated[iSEEslots$shapeByColData] <- "character"

collated[iSEEslots$sizeByColData] <- "character"

collated[iSEEslots$tooltipColData] <- "character"

#' @export
setClass("ColumnDotPlot", contains=c("DotPlot", "VIRTUAL"), slots=collated)

####################################################

collated <- character(0)

collated[iSEEslots$facetRowByRowData] <- "character"
collated[iSEEslots$facetColumnByRowData] <- "character"

collated[iSEEslots$colorByRowData] <- "character"
collated[iSEEslots$colorBySampNameAssay] <- "character"
collated[iSEEslots$colorByFeatNameColor] <- "character"

collated[iSEEslots$shapeByRowData] <- "character"

collated[iSEEslots$sizeByRowData] <- "character"

collated[iSEEslots$tooltipRowData] <- "character"

#' @export
setClass("RowDotPlot", contains=c("DotPlot", "VIRTUAL"), slots=collated)

####################################################

collated <- character(0)
collated[iSEEslots$redDimType] <- "character"
collated[iSEEslots$redDimXAxis] <- "integer"
collated[iSEEslots$redDimYAxis] <- "integer"

#' @export
setClass("ReducedDimensionPlot", contains="ColumnDotPlot", slots=collated)

####################################################

collated <- character(0)
collated[iSEEslots$featAssayAssay] <- "character"
collated[iSEEslots$featAssayXAxis] <- "character"
collated[iSEEslots$featAssayXAxisColData] <- "character"

collated[iSEEslots$featAssayXAxisFeatName] <- "character"
collated[iSEEslots$featAssayXAxisRowTable] <- "character"
collated[iSEEslots$featAssayXAxisFeatDynamic] <- "logical"

collated[iSEEslots$featAssayYAxisFeatName] <- "character"
collated[iSEEslots$featAssayYAxisRowTable] <- "character"
collated[iSEEslots$featAssayYAxisFeatDynamic] <- "logical"

#' @export
setClass("FeatureAssayPlot", contains="ColumnDotPlot", slots=collated)

####################################################

collated <- character(0)
collated[iSEEslots$colDataXAxis] <- "character"
collated[iSEEslots$colDataYAxis] <- "character"
collated[iSEEslots$colDataXAxisColData] <- "character"

#' @export
setClass("ColumnDataPlot", contains="ColumnDotPlot", slots=collated)

####################################################

collated <- character(0)
collated[iSEEslots$rowDataXAxis] <- "character"
collated[iSEEslots$rowDataYAxis] <- "character"
collated[iSEEslots$rowDataXAxisRowData] <- "character"

#' @export
setClass("RowDataPlot", contains="RowDotPlot", slots=collated)

####################################################

collated <- character(0)
collated[iSEEslots$sampAssayAssay] <- "character"
collated[iSEEslots$sampAssayXAxis] <- "character"
collated[iSEEslots$sampAssayXAxisRowData] <- "character"

collated[iSEEslots$sampAssayXAxisSampName] <- "character"
collated[iSEEslots$sampAssayXAxisColTable] <- "character"
collated[iSEEslots$sampAssayXAxisSampDynamic] <- "logical"

collated[iSEEslots$sampAssayYAxisSampName] <- "character"
collated[iSEEslots$sampAssayYAxisColTable] <- "character"
collated[iSEEslots$sampAssayYAxisSampDynamic] <- "logical"

#' @export
setClass("SampleAssayPlot", contains="RowDotPlot", slots=collated)

####################################################

collated <- character(0)
collated[iSEEslots$TableSelected] <- "character"
collated[iSEEslots$TableSearch] <- "character"
collated[iSEEslots$TableColSearch] <- "character"

collated[iSEEslots$TableHidden] <- "character"

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

collated <- character(0)

collated[iSEEslots$heatMapAssay] <- "character"
collated[iSEEslots$heatMapCustomFeatNames] <- "logical"
collated[iSEEslots$heatMapFeatNameText] <- "character"
collated[iSEEslots$heatMapClusterFeatures] <- "logical"
collated[iSEEslots$heatMapClusterDistanceFeatures] <- "character"
collated[iSEEslots$heatMapClusterMethodFeatures] <- "character"
collated[iSEEslots$dataParamBoxOpen] <- "logical"

collated[iSEEslots$visualParamChoice] <- "character"
collated[iSEEslots$heatMapColData] <- "character"
collated[iSEEslots$heatMapRowData] <- "character"

collated[iSEEslots$heatMapCustomAssayBounds] <- "logical"
collated[iSEEslots$assayLowerBound] <- "numeric"
collated[iSEEslots$assayUpperBound] <- "numeric"
collated[iSEEslots$assayCenterRows] <- "logical"
collated[iSEEslots$assayScaleRows] <- "logical"
collated[iSEEslots$heatMapCenteredColormap] <- "character"

collated[iSEEslots$showDimnames] <- "character"
collated[iSEEslots$plotLegendPosition] <- "character"
collated[iSEEslots$plotLegendDirection] <- "character"
collated[iSEEslots$visualParamBoxOpen] <- "logical"
collated[iSEEslots$namesRowFontSize] <- "numeric"
collated[iSEEslots$namesColumnFontSize] <- "numeric"

collated[iSEEslots$heatMapShowSelection] <- "logical"
collated[iSEEslots$heatMapOrderSelection] <- "logical"

#' @export
setClass("ComplexHeatmapPlot", contains="Panel", slots=collated)
