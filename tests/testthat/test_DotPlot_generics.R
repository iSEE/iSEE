# This tests the various DotPlot-related generics.
# library(testthat); library(iSEE); source("setup_sce.R"); source("setup_mimic_live_app.R"); 
# source("test_DotPlot_generics.R")

memory <- list(
    ReducedDimensionPlot(
        LegendPointSize = 2
    ),
    ColumnDataPlot(),
    FeatureAssayPlot(),
    RowDataPlot(),
    SampleAssayPlot(),
    SampleAssayPlot(),
    SampleAssayPlot()
)

pObjects <- mimic_live_app(sce, memory)
sce <- iSEE:::.set_colormap(sce, ExperimentColorMap())

########################################
# .addDotPlotDataColor

test_that(".addDotPlotDataColor handles metadata selection in column plots", {
    params <- pObjects$memory$ReducedDimensionPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle
    field <- colnames(colData(sce))[1]
    params[[iSEE:::.colorByFeatName]] <- field

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    color_out <- iSEE:::.addDotPlotDataColor(params, env)

    expect_match(color_out$commands, "colData", fixed=TRUE)
    expect_match(color_out$commands, field, fixed=TRUE)
    expect_true(!is.null(env$plot.data$ColorBy))
})

test_that(".addDotPlotDataColor handles feature selection in column plots", {
    params <- pObjects$memory$ReducedDimensionPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByFeatNameTitle
    rn <- rownames(sce)[1]
    params[[iSEE:::.colorByFeatName]] <- rn

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    color_out <- iSEE:::.addDotPlotDataColor(params, env)

    expect_match(color_out$commands, "assay", fixed=TRUE)
    expect_match(color_out$commands, rn, fixed=TRUE)
    expect_true(!is.null(env$plot.data$ColorBy))

    expect_match(color_out$labels$ColorBy, rn, fixed=TRUE)
    expect_match(color_out$labels$ColorBy, iSEEOptions$get("assay")[1], fixed=TRUE)
})

test_that(".addDotPlotDataColor handles sample selection in column plots", {
    params <- pObjects$memory$ReducedDimensionPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorBySampNameTitle
    cn <- colnames(sce)[3]
    params[[ iSEE:::.colorBySampName]] <- cn

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    color_out <- iSEE:::.addDotPlotDataColor(params, env)

    expect_match(color_out$commands, cn, fixed=TRUE)
    expect_identical(color_out$labels$ColorBy, cn)
    expect_true(!is.null(env$plot.data$ColorBy))
})

test_that(".addDotPlotDataColor handles multiple selections in column plots", {
    params <- pObjects$memory$ReducedDimensionPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByColSelectionsTitle

    # Trying with nothing first:
    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    color_out <- iSEE:::.addDotPlotDataColor(params, env)
    expect_match(color_out$commands, "multiSelectionToFactor(list()", fixed=TRUE)
    expect_true(unique(env$plot.data$ColorBy)=="unselected")

    # Filling with something:
    env$col_selected <- list(active=head(colnames(sce)), saved=tail(colnames(sce)))
    .generateDotPlotData(params, env)
    color_out <- iSEE:::.addDotPlotDataColor(params, env)
    expect_match(color_out$commands, "multiSelectionToFactor(col_selected", fixed=TRUE)
    expect_true(all(c("active", "saved") %in% env$plot.data$ColorBy))
})

test_that(".addDotPlotDataColor handles metadata selection in row plots", {
    params <- pObjects$memory$RowDataPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByRowDataTitle
    field <- colnames(rowData(sce))[1]
    params[[iSEE:::.colorByFeatName]] <- field

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    color_out <- iSEE:::.addDotPlotDataColor(params, env)

    expect_match(color_out$commands, "rowData", fixed=TRUE)
    expect_match(color_out$commands, field, fixed=TRUE)
    expect_true(!is.null(env$plot.data$ColorBy))
})

test_that(".addDotPlotDataColor handles feature selection in row plots", {
    params <- pObjects$memory$RowDataPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByFeatNameTitle
    rn <- rownames(sce)[3]
    params[[iSEE:::.colorByFeatName]] <- rn

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    color_out <- iSEE:::.addDotPlotDataColor(params, env)

    expect_match(color_out$commands, rn, fixed=TRUE)
    expect_identical(color_out$labels$ColorBy, rn)
    expect_true(!is.null(env$plot.data$ColorBy))
})

test_that(".addDotPlotDataColor handles sample selection in row plots", {
    params <- pObjects$memory$RowDataPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorBySampNameTitle
    cn <- colnames(sce)[3]
    params[[iSEE:::.colorBySampName]] <- cn

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    color_out <- iSEE:::.addDotPlotDataColor(params, env)

    expect_match(color_out$commands, "assay", fixed=TRUE)
    expect_match(color_out$commands, cn, fixed=TRUE)
    expect_true(!is.null(env$plot.data$ColorBy))

    expect_match(color_out$labels$ColorBy, cn, fixed=TRUE)
    expect_match(color_out$labels$ColorBy, iSEEOptions$get("assay")[1], fixed=TRUE)
})

test_that(".addDotPlotDataColor handles multiple selections in row plots", {
    params <- pObjects$memory$RowDataPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByRowSelectionsTitle

    # Trying with nothing first:
    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    color_out <- iSEE:::.addDotPlotDataColor(params, env)
    expect_match(color_out$commands, "multiSelectionToFactor(list()", fixed=TRUE)
    expect_true(unique(env$plot.data$ColorBy)=="unselected")

    # Filling with something:
    env$row_selected <- list(active=head(rownames(sce)), saved=tail(rownames(sce)))
    .generateDotPlotData(params, env)
    color_out <- iSEE:::.addDotPlotDataColor(params, env)
    expect_match(color_out$commands, "multiSelectionToFactor(row_selected", fixed=TRUE)
    expect_true(all(c("active", "saved") %in% env$plot.data$ColorBy))
})

########################################
# .colorDotPlot

test_that(".colorDotPlot returns NULL when coloring DotPlot by nothing", {
    x <- ColumnDataPlot()
    x[[iSEE:::.colorByField]] <- iSEE:::.colorByNothingTitle
    out <- iSEE:::.colorDotPlot(x, LETTERS)
    expect_null(out)

    x <- RowDataPlot()
    x[[iSEE:::.colorByField]] <- iSEE:::.colorByNothingTitle
    out <- iSEE:::.colorDotPlot(x, LETTERS)
    expect_null(out)
})

test_that(".colorDotPlot returns a command for coloring by metadata", {
    params <- pObjects$memory$ReducedDimensionPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle
    params[[iSEE:::.colorByFeatName]] <- colnames(colData(sce))[1]
    color_add <- iSEE:::.colorDotPlot(params, colData(sce)[,1])
    expect_match(color_add[1], "colDataColorMap") 

    # And again, for rows.
    params <- pObjects$memory$RowDataPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByRowDataTitle
    params[[iSEE:::.colorByFeatName]] <- "BLAH"
    color_add <- iSEE:::.colorDotPlot(params, letters)
    expect_match(color_add[1], "rowDataColorMap") 
})

test_that(".colorDotPlot returns a command for coloring by features", {
    params <- pObjects$memory$ReducedDimensionPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByFeatNameTitle
    params[[iSEE:::.colorByFeatName]] <- rownames(sce)[1]
    color_add <- iSEE:::.colorDotPlot(params, assay(sce)[,1])
    expect_match(color_add[1], "scale_color_gradientn.*assayColorMap") 

    # And again, for rows.
    params <- pObjects$memory$RowDataPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByFeatNameTitle
    params[[iSEE:::.colorByFeatName]] <- rownames(sce)[3]
    color_add <- iSEE:::.colorDotPlot(params, c(TRUE, FALSE))
    expect_identical(color_add, c(
        "scale_color_manual(values=c(`FALSE`='black', `TRUE`=\"red\"), drop=FALSE) +",
        "geom_point(aes(x=X, y=Y), data=subset(plot.data, ColorBy == 'TRUE'), col=\"red\", alpha=1, size=5*1) +"))
})

test_that(".colorDotPlot returns a command for coloring by samples", {
    params <- pObjects$memory$ReducedDimensionPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorBySampNameTitle
    params[[ iSEE:::.colorBySampName]] <- colnames(sce)[3]
    color_add <- iSEE:::.colorDotPlot(params, env$plot.data$ColorBy)
    expect_identical(color_add, c(
        "scale_color_manual(values=c(`FALSE`='black', `TRUE`=\"red\"), drop=FALSE) +",
        "geom_point(aes(x=X, y=Y), data=subset(plot.data, ColorBy == 'TRUE'), col=\"red\", alpha=1, size=5*1) +"))

    params <- pObjects$memory$RowDataPlot1
    params[[iSEE:::.colorByField]] <- iSEE:::.colorBySampNameTitle
    params[[iSEE:::.colorByFeatName]] <- rownames(sce)[3]
    color_add <- iSEE:::.colorDotPlot(params, 1:50)
    expect_match(color_add[1], "scale_color_gradientn.*assayColorMap")
})

test_that(".colorDotPlot behaves when coloring a DotPlot by selections", {
    x <- ColumnDataPlot()
    x[[iSEE:::.colorByField]] <- iSEE:::.colorByColSelectionsTitle
    out <- iSEE:::.colorDotPlot(x, LETTERS)
    expect_match(out, "columnSelectionColorMap")

    x <- RowDataPlot()
    x[[iSEE:::.colorByField]] <- iSEE:::.colorByRowSelectionsTitle
    out <- iSEE:::.colorDotPlot(x, LETTERS)
    expect_match(out, "rowSelectionColorMap")
})

########################################
# .addDotPlotDataFacets

test_that(".addDotPlotDataFacets works for column plots with nothing", {
    params <- pObjects$memory$ReducedDimensionPlot1
    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)

    facet_out <- iSEE:::.addDotPlotDataFacets(params, env)
    expect_null(facet_out$commands)
    expect_false("FacetRow" %in% colnames(env$plot.data))
    expect_false("FacetColumn" %in% colnames(env$plot.data))
})

test_that(".addDotPlotDataFacets works for column plots with column data", {
    params <- pObjects$memory$ReducedDimensionPlot1

    params[["FacetRowBy"]] <- "Column data"
    params[["FacetRowByColData"]] <- "driver_1_s"
    params[["FacetColumnBy"]] <- "Column data"
    params[["FacetColumnByColData"]] <- "Core.Type"

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    facet_out <- iSEE:::.addDotPlotDataFacets(params, env)

    expect_true("FacetRow" %in% colnames(env$plot.data))
    expect_true("FacetColumn" %in% colnames(env$plot.data))
    expect_match(facet_out$commands["FacetRow"], "driver_1_s", fixed=TRUE)
    expect_match(facet_out$commands["FacetColumn"], "Core.Type", fixed=TRUE)
})

test_that(".addDotPlotDataFacets works for column plots with column selections", {
    params <- pObjects$memory$ReducedDimensionPlot1

    params[["FacetRowBy"]] <- "Column selection"
    params[["FacetColumnBy"]] <- "Column selection"

    env <- new.env()
    env$se <- sce
    env$col_selected <- list(active=head(colnames(sce)), saved=tail(colnames(sce)))
    .generateDotPlotData(params, env)
    facet_out <- iSEE:::.addDotPlotDataFacets(params, env)

    expect_true("FacetRow" %in% colnames(env$plot.data))
    expect_true("FacetColumn" %in% colnames(env$plot.data))
    expect_match(facet_out$commands["FacetRow"], "multiSelectionToFactor.*col_selected")
    expect_match(facet_out$commands["FacetColumn"], "multiSelectionToFactor.*col_selected")
})

test_that(".addDotPlotDataFacets works for row plots with nothing", {
    params <- pObjects$memory$RowDataPlot1
    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)

    facet_out <- iSEE:::.addDotPlotDataFacets(params, env)
    expect_null(facet_out$commands)
    expect_false("FacetRow" %in% colnames(env$plot.data))
    expect_false("FacetColumn" %in% colnames(env$plot.data))
})

test_that(".addDotPlotDataFacets works for row plots with row data", {
    rowData(sce)[, "LETTERS"] <- sample(LETTERS[1:3], nrow(sce), replace=TRUE)

    params <- pObjects$memory$RowDataPlot1
    params[["FacetRowBy"]] <- "Row data"
    params[["FacetRowByRowData"]] <- "letters"
    params[["FacetColumnBy"]] <- "Row data"
    params[["FacetColumnByRowData"]] <- "LETTERS"

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    facet_out <- iSEE:::.addDotPlotDataFacets(params, env)

    expect_true("FacetRow" %in% colnames(env$plot.data))
    expect_true("FacetColumn" %in% colnames(env$plot.data))
    expect_match(facet_out$commands["FacetRow"], "letters", fixed=TRUE)
    expect_match(facet_out$commands["FacetColumn"], "LETTERS", fixed=TRUE)
})

test_that(".addDotPlotDataFacets works for row plots with row selections", {
    params <- pObjects$memory$RowDataPlot1

    params[["FacetRowBy"]] <- "Row selection"
    params[["FacetColumnBy"]] <- "Row selection"

    env <- new.env()
    env$se <- sce
    env$row_selected <- list(active=head(rownames(sce)), saved=tail(rownames(sce)))
    .generateDotPlotData(params, env)
    facet_out <- iSEE:::.addDotPlotDataFacets(params, env)

    expect_true("FacetRow" %in% colnames(env$plot.data))
    expect_true("FacetColumn" %in% colnames(env$plot.data))
    expect_match(facet_out$commands["FacetRow"], "multiSelectionToFactor.*row_selected")
    expect_match(facet_out$commands["FacetColumn"], "multiSelectionToFactor.*row_selected")
})
