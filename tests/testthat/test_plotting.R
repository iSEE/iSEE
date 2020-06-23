# Tests the various plotting functionality.
# library(testthat); library(iSEE); source("setup_sce.R"); source("setup_other.R"); source("test_plotting.R")

context("plotting")

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
metadata(sce)$colormap <- ExperimentColorMap()

########################################
# .make_redDimPlot/.scatter_plot ----

test_that(".make_redDimPlot/.scatter_plot produce a valid list",{
    p.out <- .generateOutput(pObjects$memory$ReducedDimensionPlot1, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    # return value is a named list
    expect_type(p.out, "list")
    expect_named(p.out, c("commands", "contents", "plot"))

    # cmd value is a named list
    expect_type(p.out$commands, "list")
    expect_true(all(vapply(p.out$commands, is.character, TRUE)))

    # xy value is a data frame
    expect_s3_class(p.out$contents, "data.frame")
    expect_named(p.out$contents, c("X","Y"))

    #plot
    expect_s3_class(p.out$plot, c("gg", "ggplot"))
})

test_that(".make_redDimPlot/.scatter_plot produce a valid xy with color", {
    rdp <- pObjects$memory$ReducedDimensionPlot1
    rdp[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle

    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c("X","Y","ColorBy"))
})

########################################
# .make_colDataPlot/.scatter_plot ----

test_that(".make_colDataPlot/.scatter_plot produce a valid list",{
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "NREADS"
    cdp[[iSEE:::.colDataYAxis]] <- "NALIGNED"

    p.out <- .generateOutput(cdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    # return value is a named list
    expect_type(p.out, "list")
    expect_named(p.out, c("commands", "contents", "plot"))

    # cmd value is a named list
    expect_type(p.out$commands, "list")
    expect_true(all(vapply(p.out$commands, is.character, TRUE)))

    # xy value is a data frame
    expect_s3_class(p.out$contents, "data.frame")
    expect_named(p.out$contents, c("Y","X"))

    #plot
    expect_s3_class(p.out$plot, c("gg", "ggplot"))
})

test_that(".make_colDataPlot/.scatter_plot produce a valid xy with color", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "NREADS"
    cdp[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle

    p.out <- .generateOutput(cdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c('Y', 'X', 'ColorBy'))
})

########################################
# .make_colDataPlot/.violin_plot ----

test_that(".make_colDataPlot/.violin_plot produce a valid list",{
    p.out <- .generateOutput(pObjects$memory$ColumnDataPlot1, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    # return value is a named list
    expect_type(p.out, "list")
    expect_named(p.out, c("commands", "contents", "plot"))

    # cmd value is a named list
    expect_type(p.out$commands, "list")
    expect_true(all(vapply(p.out$commands, is.character, TRUE)))

    # xy value is a data frame
    expect_s3_class(p.out$contents, "data.frame")
    expect_named(p.out$contents, c("Y", "X", "GroupBy", "jitteredX"))

    #plot
    expect_s3_class(p.out$plot, c("gg", "ggplot"))
})

test_that(".make_colDataPlot/.violin_plot produce a valid xy with color", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle

    p.out <- .generateOutput(cdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c("Y","X","ColorBy","GroupBy","jitteredX"))
})

########################################
# .make_colDataPlot/.square_plot ----

test_that(".make_colDataPlot/.square_plot produce a valid list",{
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "driver_1_s"
    cdp[[iSEE:::.colDataYAxis]] <- "passes_qc_checks_s"

    p.out <- .generateOutput(cdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    # return value is a named list
    expect_type(p.out, "list")
    expect_named(p.out, c("commands", "contents", "plot"))

    # cmd value is a named list
    expect_type(p.out$commands, "list")
    expect_true(all(vapply(p.out$commands, is.character, TRUE)))

    # xy value is a data frame
    expect_s3_class(p.out$contents, "data.frame")
    expect_named(p.out$contents, c("Y","X","jitteredX","jitteredY"))

    #plot
    expect_s3_class(p.out$plot, c("gg", "ggplot"))
})

test_that(".make_colDataPlot/.square_plot produce a valid xy with color", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "driver_1_s"
    cdp[[iSEE:::.colDataYAxis]] <- "passes_qc_checks_s"
    cdp[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle

    p.out <- .generateOutput(cdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c("Y","X","ColorBy","jitteredX","jitteredY"))
})

########################################
# .make_rowDataPlot/.scatter_plot ----

test_that(".make_rowDataPlot/.scatter_plot produce a valid list",{
    rdp <- pObjects$memory$RowDataPlot
    rdp[[iSEE:::.rowDataXAxis]] <- iSEE:::.rowDataXAxisRowData
    rdp[[iSEE:::.rowDataXAxisRowData]] <- "num_cells"
    rdp[[iSEE:::.rowDataYAxis]] <- "mean_count"

    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    # return value is a named list
    expect_type(p.out, "list")
    expect_named(p.out, c("commands", "contents", "plot"))

    # cmd value is a named list
    expect_type(p.out$commands, "list")
    expect_true(all(vapply(p.out$commands, is.character, TRUE)))

    # xy value is a data frame
    expect_s3_class(p.out$contents, "data.frame")
    expect_named(p.out$contents, c("Y","X"))

    #plot
    expect_s3_class(p.out$plot, c("gg", "ggplot"))
})

test_that(".make_rowDataPlot/.violin_plot produce a valid xy with color", {
    rdp <- pObjects$memory$RowDataPlot
    rdp[[iSEE:::.rowDataXAxis]] <- iSEE:::.rowDataXAxisRowData
    rdp[[iSEE:::.rowDataXAxisRowData]] <- "num_cells"
    rdp[[iSEE:::.rowDataYAxis]] <- "mean_count"

    rdp[[iSEE:::.colorByField]] <- iSEE:::.colorByRowDataTitle
    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c('Y', 'X', 'ColorBy'))

    # Color by feature name
    rdp[[iSEE:::.colorByField]] <- iSEE:::.colorByFeatNameTitle
    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c('Y', 'X', 'ColorBy'))
})

########################################
# .make_rowDataPlot/.violin_plot ----

test_that(".make_rowDataPlot/.violin_plot produce a valid list",{
    rdp <- pObjects$memory$RowDataPlot
    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    # return value is a named list
    expect_type(p.out, "list")
    expect_named(p.out, c("commands", "contents", "plot"))

    # cmd value is a named list
    expect_type(p.out$commands, "list")
    expect_true(all(vapply(p.out$commands, is.character, TRUE)))

    # xy value is a data frame
    expect_s3_class(p.out$contents, "data.frame")
    expect_named(p.out$contents, c("Y", "X", "GroupBy", "jitteredX"))

    #plot
    expect_s3_class(p.out$plot, c("gg", "ggplot"))
})

test_that(".make_rowDataPlot/.violin_plot produce a valid xy with color", {
    rdp <- pObjects$memory$RowDataPlot
    rdp[[iSEE:::.colorByField]] <- iSEE:::.colorByRowDataTitle

    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c("Y","X","ColorBy","GroupBy","jitteredX"))

    # Color by feature name
    rdp[[iSEE:::.colorByField]] <- iSEE:::.colorByFeatNameTitle

    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c("Y","X","ColorBy","GroupBy","jitteredX"))
})

########################################
# .make_rowDataPlot/.square_plot ----

test_that(".make_rowDataPlot/.square_plot produce a valid list",{
    rowData(sce)[, "LETTERS"] <- sample(LETTERS[1:3], nrow(sce), replace=TRUE)

    rdp <- pObjects$memory$RowDataPlot
    rdp[[iSEE:::.rowDataXAxis]] <- iSEE:::.rowDataXAxisRowData
    rdp[[iSEE:::.rowDataXAxisRowData]] <- "letters"
    rdp[[iSEE:::.rowDataYAxis]] <- "LETTERS"

    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    # return value is a named list
    expect_type(p.out, "list")
    expect_named(p.out, c("commands", "contents", "plot"))

    # cmd value is a named list
    expect_type(p.out$commands, "list")
    expect_true(all(vapply(p.out$commands, is.character, TRUE)))

    # xy value is a data frame
    expect_s3_class(p.out$contents, "data.frame")
    expect_named(p.out$contents, c('Y', 'X', 'jitteredX', 'jitteredY'))

    #plot
    expect_s3_class(p.out$plot, c("gg", "ggplot"))
})

test_that(".make_rowDataPlot/.square_plot produce a valid xy with color",{
    rowData(sce)[, "LETTERS"] <- sample(LETTERS[1:3], nrow(sce), replace=TRUE)

    rdp <- pObjects$memory$RowDataPlot
    rdp[[iSEE:::.rowDataXAxis]] <- iSEE:::.rowDataXAxisRowData
    rdp[[iSEE:::.rowDataXAxisRowData]] <- "letters"
    rdp[[iSEE:::.rowDataYAxis]] <- "LETTERS"

    rdp[[iSEE:::.colorByField]] <- iSEE:::.colorByRowDataTitle

    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    # return value is a named list
    expect_type(p.out, "list")
    expect_named(p.out, c("commands", "contents", "plot"))

    # cmd value is a named list
    expect_type(p.out$commands, "list")
    expect_true(all(vapply(p.out$commands, is.character, TRUE)))

    # xy value is a data frame
    expect_s3_class(p.out$contents, "data.frame")
    expect_named(p.out$contents, c('Y', 'X', 'ColorBy', 'jitteredX', 'jitteredY'))

    #plot
    expect_s3_class(p.out$plot, c("gg", "ggplot"))

    # Color by feature name
    rdp[[iSEE:::.colorByField]] <- iSEE:::.colorByFeatNameTitle

    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c('Y', 'X', 'ColorBy', 'jitteredX', 'jitteredY'))
})

########################################
# .make_featAssayPlot/.scatter_plot ----

test_that(".make_featAssayPlot/.violin_plot produce a valid list",{
    fdp <- pObjects$memory$FeatureAssayPlot1
    p.out <- .generateOutput(fdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    # return value is a named list
    expect_type(p.out, "list")
    expect_named(p.out, c("commands", "contents", "plot"))

    # cmd value is a named list
    expect_type(p.out$commands, "list")
    expect_true(all(vapply(p.out$commands, is.character, TRUE)))

    # xy value is a data frame
    expect_s3_class(p.out$contents, "data.frame")
    expect_named(p.out$contents, c("Y", "X", "GroupBy", "jitteredX"))

    #plot
    expect_s3_class(p.out$plot, c("gg", "ggplot"))
})

test_that(".make_featAssayPlot/.violin_plot produce a valid xy with color", {
    fdp <- pObjects$memory$FeatureAssayPlot1
    fdp[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle

    p.out <- .generateOutput(fdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c("Y","X","ColorBy","GroupBy","jitteredX"))
})

test_that(".make_featAssayPlot works for XAxis set to Column data", {
    fdp <- pObjects$memory$FeatureAssayPlot1
    fdp[[iSEE:::.featAssayXAxis]] <- iSEE:::.featAssayXAxisColDataTitle
    fdp[[iSEE:::.featAssayXAxisColData]] <- "dissection_s"

    p.out <- .generateOutput(fdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_true(any(grepl("dissection_s", unlist(p.out$commands))))
})

test_that(".make_featAssayPlot works for XAxis set to a character feature name", {
    selected_gene <- "0610009B22Rik"

    fdp <- pObjects$memory$FeatureAssayPlot1
    fdp[[iSEE:::.featAssayXAxis]] <- iSEE:::.featAssayXAxisFeatNameTitle
    fdp[[iSEE:::.featAssayXAxisFeatName]] <- selected_gene

    p.out <- .generateOutput(fdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_true(any(grepl(selected_gene, unlist(p.out$commands))))
})

test_that(".make_featAssayPlot works for groupable colour covariate", {
    selected_coldata <- "dissection_s"

    fdp <- pObjects$memory$FeatureAssayPlot1
    fdp[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle
    fdp[[iSEE:::.colorByColData]] <- selected_coldata

    p.out <- .generateOutput(fdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_true(any(grepl(selected_coldata, unlist(p.out$commands))))
    expect_named(p.out$contents, c("Y", "X", "ColorBy", "GroupBy", "jitteredX"))
})

########################################
# .make_sampAssayPlot ----

test_that(".make_sampAssayPlot works with X covariate set to None", {
    sap <- pObjects$memory$SampleAssayPlot1
    p.out <- .generateOutput(sap, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    # return value is a named list
    expect_type(p.out, "list")
    expect_named(p.out, c("commands", "contents", "plot"))

    # cmd value is a named list
    expect_type(p.out$commands, "list")
    expect_true(all(vapply(p.out$commands, is.character, TRUE)))

    # xy value is a data frame
    expect_s3_class(p.out$contents, "data.frame")
    expect_named(p.out$contents, c("Y", "X", "GroupBy", "jitteredX"))
    expect_true(all(p.out$contents$X==""))

    #plot
    expect_s3_class(p.out$plot, c("gg", "ggplot"))
})

test_that(".make_sampAssayPlot works with X variable set to Row data", {
    selected_rowdata <- "num_cells"

    sap <- pObjects$memory$SampleAssayPlot1
    sap[[iSEE:::.rowDataXAxis]] <- iSEE:::.sampAssayXAxisRowDataTitle
    sap[[iSEE:::.rowDataXAxisRowData]] <- selected_rowdata

    p.out <- .generateOutput(sap, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_true(any(grepl(selected_rowdata, unlist(p.out$commands))))
})

test_that(".make_sampAssayPlot works with X variable set to Sample name", {
    selected_sample <- colnames(sce)[2]

    sap <- pObjects$memory$SampleAssayPlot1
    sap[[iSEE:::.rowDataXAxis]] <- iSEE:::.sampAssayXAxisSampNameTitle
    sap[[iSEE:::.sampAssayXAxisSampName]] <- selected_sample

    p.out <- .generateOutput(sap, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_true(any(grepl(selected_sample, unlist(p.out$commands))))
})

########################################
# .make_colDataPlot/.create_plot horizontal violin plots ----

test_that(".make_colDataPlot/.create_plot can produce horizontal violins", {
    selected_coldataX <- "NREADS"
    selected_coldataY <- "driver_1_s"

    cdp <- pObjects$memory$ColumnDataPlot
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colorByColDataTitle

    cdp1 <- cdp
    cdp1[[iSEE:::.colDataXAxisColData]] <- selected_coldataX
    cdp1[[iSEE:::.colDataYAxis]] <- selected_coldataY

    cdp2 <- cdp
    cdp2[[iSEE:::.colDataXAxisColData]] <- selected_coldataY
    cdp2[[iSEE:::.colDataYAxis]] <- selected_coldataX

    p.out1 <- .generateOutput(cdp1, sce, all_memory=pObjects$memory, all_contents=pObjects$contents)
    p.out2 <- .generateOutput(cdp2, sce, all_memory=pObjects$memory, all_contents=pObjects$contents)

    # Contents should be the same.
    expect_identical(p.out1$contents, p.out2$contents)

    expect_true(any(grepl("coord_flip", unlist(p.out1$commands))))
    expect_false(any(grepl("coord_flip", unlist(p.out2$commands))))
})

########################################
# .scatter_plot plot with zoom ----

test_that(".scatter_plot works with zoom",{
    params <- pObjects$memory$ReducedDimensionPlot1
    ref <- .generateOutput(params, sce, all_memory=pObjects$memory, all_contents=pObjects$contents)

    # Identify range of data
    rd <- reducedDim(sce, params[[iSEE:::.redDimType]])
    x_range <- range(head(rd[, params[[iSEE:::.redDimXAxis]]]), 10)
    y_range <- range(head(rd[, params[[iSEE:::.redDimYAxis]]]), 10)

    # Set zoom min/max to the first two distinct values in X/Y direction
    zoom_range <- c(x_range, y_range)
    names(zoom_range) <- c("xmin","xmax","ymin","ymax")

    params[[iSEE:::.zoomData]] <- zoom_range
    p.out <- .generateOutput(params, sce, all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_identical(p.out$contents, ref$contents)
    expect_true(any(grepl("coord_cartesian.*xmin.*xmax", unlist(p.out$commands))))
    expect_false(any(grepl("coord_cartesian.*xmin.*xmax", unlist(ref$commands))))
})

########################################
# .make_colDataPlot/.violin_plot works with zoom ----

test_that(".make_colDataPlot/.violin_plot works with zoom",{
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    chosen_x <- "driver_1_s"
    cdp[[iSEE:::.colDataXAxisColData]] <- chosen_x

    ref <- .generateOutput(cdp, sce, all_memory=pObjects$memory, all_contents=pObjects$contents)

    # Identify valid values
    x_unique <- unique(as.numeric(as.factor(colData(sce)[,chosen_x])))
    chosen_y <- cdp[[iSEE:::.colDataYAxis]]
    y_range <- range(head(colData(sce)[,chosen_y], 10))

    # Set zoom min/max to the first two distinct values in X/Y direction
    zoom_range <- c(sort(head(x_unique, 2)), y_range)

    # Extend the zoom to perfectly include the min/max boxes
    zoom_range <- zoom_range + c(-0.5, 0.5, 0, 0)
    names(zoom_range) <- c("xmin","xmax","ymin","ymax")

    # Set the zoom
    cdp[[iSEE:::.zoomData]] <- zoom_range

    p.out <- .generateOutput(cdp, sce, all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_identical(p.out$contents, ref$contents)
    expect_true(any(grepl("coord_cartesian.*xmin.*xmax", unlist(p.out$commands))))
    expect_false(any(grepl("coord_cartesian.*xmin.*xmax", unlist(ref$commands))))
})

########################################
# .make_colDataPlot/.violin_plot works with horizontal zoom ----

test_that(".make_colDataPlot/.violin_plot works with zoom",{
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    chosen_x <- "NREADS"
    cdp[[iSEE:::.colDataXAxisColData]] <- chosen_x
    chosen_y <- "driver_1_s"
    cdp[[iSEE:::.colDataYAxis]] <- chosen_y

    ref <- .generateOutput(cdp, sce, all_memory=pObjects$memory, all_contents=pObjects$contents)

    # Identify valid values
    x_range <- range(head(colData(sce)[,chosen_x], 10))
    y_unique <- unique(as.numeric(as.factor(colData(sce)[,chosen_y])))

    # Set zoom min/max to the first two distinct values in X/Y direction
    zoom_range <- c(x_range, sort(head(y_unique, 2)))

    # Extend the zoom to perfectly include the min/max boxes
    zoom_range <- zoom_range + c(0, 0, -0.5, 0.5)
    names(zoom_range) <- c("xmin","xmax","ymin","ymax")

    # Set the zoom
    cdp[[iSEE:::.zoomData]] <- zoom_range

    p.out <- .generateOutput(cdp, sce, all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_identical(p.out$contents, ref$contents)
    expect_true(any(grepl("coord_flip.*xmin.*xmax", unlist(p.out$commands))))
    expect_false(any(grepl("coord_flip.*xmin.*xmax", unlist(ref$commands))))
})

########################################
# .make_colDataPlot/.square_plot works with zoom ----

test_that(".make_colDataPlot/.square_plot works with zoom",{
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    chosen_x <- "passes_qc_checks_s"
    cdp[[iSEE:::.colDataXAxisColData]] <- chosen_x
    chosen_y <- "driver_1_s"
    cdp[[iSEE:::.colDataYAxis]] <- chosen_y

    ref <- .generateOutput(cdp, sce, all_memory=pObjects$memory, all_contents=pObjects$contents)

    # Identify valid values
    x_unique <- unique(as.numeric(as.factor(colData(sce)[,chosen_x])))
    y_unique <- unique(as.numeric(as.factor(colData(sce)[,chosen_y])))

    # Set zoom min/max to the first two distinct values in X/Y direction
    zoom_range <- c(
        sort(head(x_unique, 2)),
        sort(head(y_unique, 2))
    )

    # Extend the zoom to perfectly include the min/max boxes
    zoom_range <- zoom_range + rep(c(-0.5, 0.5), times=2)
    names(zoom_range) <- c("xmin","xmax","ymin","ymax")

    # Set the zoom
    cdp[[iSEE:::.zoomData]] <- zoom_range

    p.out <- .generateOutput(cdp, sce, all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_identical(p.out$contents, ref$contents)
    expect_true(any(grepl("coord_cartesian.*xmin.*xmax", unlist(p.out$commands))))
    expect_false(any(grepl("coord_cartesian.*xmin.*xmax", unlist(ref$commands))))
})

########################################
# .define_colorby_for_column_plot ----

test_that(".define_colorby_for_column_plot handles feature selection", {
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

    color_add <- iSEE:::.colorDotPlot(params, env$plot.data$ColorBy)
    expect_match(color_add[1], "scale_color_gradientn", fixed=TRUE)
})

test_that(".define_colorby_for_column_plot handles sample selection", {
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

    color_add <- iSEE:::.colorDotPlot(params, env$plot.data$ColorBy)
    expect_identical(color_add, c(
        "scale_color_manual(values=c(`FALSE`='black', `TRUE`=\"red\"), drop=FALSE) +",
        "geom_point(aes(x=X, y=Y), data=subset(plot.data, ColorBy == 'TRUE'), col=\"red\", alpha=1, size=5*1) +"))
})

test_that(".define_colorby_for_row_plot handles sample selection", {
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

    color_add <- iSEE:::.colorDotPlot(params, env$plot.data$ColorBy)
    expect_identical(color_add, c(
        "scale_color_manual(values=c(`FALSE`='black', `TRUE`=\"red\"), drop=FALSE) +",
        "geom_point(aes(x=X, y=Y), data=subset(plot.data, ColorBy == 'TRUE'), col=\"red\", alpha=1, size=5*1) +"))
})

test_that(".define_colorby_for_row_plot handles sample selection", {
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

    color_add <- iSEE:::.colorDotPlot(params, env$plot.data$ColorBy)
    expect_match(color_add[1], "scale_color_gradientn", fixed=TRUE)
})

########################################
# define_shapeby_for_column_plot ----

test_that("define_shapeby_for_column_plot produces the expected commands", {
    params <- pObjects$memory$ReducedDimensionPlot1
    params[[iSEE:::.shapeByField]] <- iSEE:::.shapeByColDataTitle
    params[[iSEE:::.shapeByColData]] <- "driver_1_s"

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    shape_out <- iSEE:::.addDotPlotDataShape(params, env)

    expect_true(!is.null(env$plot.data$ShapeBy))
    expect_identical(shape_out$labels$ShapeBy, "driver_1_s")
    expect_match(shape_out$commands, "driver_1_s", fixed=TRUE)
})

test_that(".define_shapeby_for_row_plot produces the expected commands", {
    params <- pObjects$memory$RowDataPlot1
    params[[iSEE:::.shapeByField]] <- iSEE:::.shapeByRowDataTitle
    params[[iSEE:::.shapeByRowData]] <- "letters"

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    shape_out <- iSEE:::.addDotPlotDataShape(params, env)

    expect_true(!is.null(env$plot.data$ShapeBy))
    expect_identical(shape_out$labels$ShapeBy, "letters")
    expect_match(shape_out$commands, "letters", fixed=TRUE)
})

########################################
# define_sizeby_for_column_plot ----

test_that("define_sizeby_for_column_plot produces the expected commands", {
    params <- pObjects$memory$ReducedDimensionPlot1
    params[[iSEE:::.sizeByField]] <- iSEE:::.sizeByColDataTitle
    params[[iSEE:::.sizeByColData]] <- "NREADS"

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    size_out <- iSEE:::.addDotPlotDataSize(params, env)

    expect_true(!is.null(env$plot.data$SizeBy))
    expect_identical(size_out$labels$SizeBy, "NREADS")
    expect_match(size_out$commands, "NREADS", fixed=TRUE)
})

test_that(".define_sizeby_for_row_plot produces the expected commands", {
    params <- pObjects$memory$RowDataPlot1
    params[[iSEE:::.sizeByField]] <- iSEE:::.sizeByRowDataTitle
    params[[iSEE:::.sizeByRowData]] <- "mean_count"

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    size_out <- iSEE:::.addDotPlotDataSize(params, env)

    expect_true(!is.null(env$plot.data$SizeBy))
    expect_identical(size_out$labels$SizeBy, "mean_count")
    expect_match(size_out$commands, "mean_count", fixed=TRUE)
})

########################################
# .coerce_type handles things ----

test_that(".coerce_type handles various inputs correctly", {

    input_field <- "XYZ"
    expect_warning(
        lab_out <- iSEE:::.coerce_type(letters, input_field, as_numeric=TRUE),
        "coloring covariate has too many unique values, coercing to numeric"
    )
    expect_identical(lab_out, "plot.data$XYZ <- as.numeric(as.factor(plot.data$XYZ));")

    expect_warning(
        lab_out <- iSEE:::.coerce_type(factor(letters), input_field, as_numeric=TRUE),
        "coloring covariate has too many unique values, coercing to numeric"
    )
    expect_identical(lab_out, "plot.data$XYZ <- as.numeric(plot.data$XYZ);")

    lab_out <- iSEE:::.coerce_type(1:10, input_field, as_numeric=TRUE)
    expect_identical(lab_out, NULL)

    lab_out <- iSEE:::.coerce_type(letters, input_field, as_numeric=FALSE)
    expect_identical(lab_out, 'plot.data[["XYZ"]] <- factor(plot.data[["XYZ"]]);')

    lab_out <- iSEE:::.coerce_type(factor(letters), input_field, as_numeric=FALSE)
    expect_identical(lab_out, NULL)

    lab_out <- iSEE:::.coerce_type(1:10, input_field, as_numeric=FALSE)
    expect_identical(lab_out, 'plot.data[["XYZ"]] <- factor(plot.data[["XYZ"]]);')
})

########################################
# .create_points handles various selection effects ----

test_that(".create_points handles selection effects", {
    all_memory <- pObjects$memory
    rdp <- all_memory$ReducedDimensionPlot1
    fap <- all_memory$FeatureAssayPlot1
    fap[[iSEE:::.selectColSource]] <- .getEncodedName(rdp)

    rd <- reducedDim(sce, rdp[[iSEE:::.redDimType]])
    x_10 <- head(rd[, rdp[[iSEE:::.redDimXAxis]]], 10)
    y_10 <- head(rd[, rdp[[iSEE:::.redDimYAxis]]], 10)

    all_memory$ReducedDimensionPlot1[[iSEE:::.brushData]] <- list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction="xy", mapping=list(x="X", y="Y"),
        brushId="dummy_brush", outputId="dummy_plot"
    )

    # Trying for transparency:
    fap[[iSEE:::.selectEffect]] <- iSEE:::.selectTransTitle
    out <- .generateOutput(fap, sce, all_memory=all_memory, all_contents=pObjects$contents)

    expect_true(!is.null(out$contents$SelectBy))
    expect_true(any(grepl("geom_point.*SelectBy.*alpha", unlist(out$commands))))

    # Trying for color:
    fap[[iSEE:::.selectEffect]] <- iSEE:::.selectColorTitle
    out <- .generateOutput(fap, sce, all_memory=all_memory, all_contents=pObjects$contents)

    expect_true(!is.null(out$contents$SelectBy))
    expect_true(any(grepl("geom_point.*SelectBy.*color=\"red\"", unlist(out$commands))))

    # Trying for restriction:
    fap[[iSEE:::.selectEffect]] <- iSEE:::.selectRestrictTitle
    out <- .generateOutput(fap, sce, all_memory=all_memory, all_contents=pObjects$contents)

    expect_true(!is.null(out$contents$SelectBy))
    expect_true(any(grepl("plot.data.all", unlist(out$commands))))
    expect_true(any(grepl("subset.*SelectBy", unlist(out$commands))))
})

########################################
# .create_points handles sizing effects ----

test_that(".create_points handles sizing effects", {

    all_memory <- pObjects$memory
    rdp <- all_memory$ReducedDimensionPlot1
    rdp[[iSEE:::.sizeByField]] <- iSEE:::.sizeByColDataTitle

    out <- .generateOutput(rdp, sce, all_memory=all_memory, all_contents=pObjects$contents)

    expect_true(!is.null(out$contents$SizeBy))
    expect_true(any(grepl("geom_point.*SizeBy.*alpha", unlist(out$commands))))

})

########################################
# brush plotting works.

test_that(".self_brush_box draw multiple shiny brushes", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "NREADS"
    cdp[[iSEE:::.colDataYAxis]] <- "driver_1_s"

    brushHistory <- list(
        list(xmin=1, xmax=2, ymin=3, ymax=4),
        list(xmin=2, xmax=3, ymin=4, ymax=5)
    )
    cdp[[iSEE:::.multiSelectHistory]] <- brushHistory

    out <- iSEE:::.self_select_boxes(cdp, flip=TRUE)
    expect_length(out, 2*length(brushHistory))
    expect_type(out, "character")
    expect_match(out[1], "geom_rect", fixed=TRUE)
    expect_match(out[2], "geom_text", fixed=TRUE)
    expect_match(out[3], "geom_rect", fixed=TRUE)
    expect_match(out[4], "geom_text", fixed=TRUE)
})

test_that(".self_brush_box can flip axes", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "NREADS"
    cdp[[iSEE:::.colDataYAxis]] <- "driver_1_s"

    brushData <- list(xmin=1, xmax=2, ymin=3, ymax=4)
    cdp[[iSEE:::.brushData]] <- brushData

    out <- iSEE:::.self_select_boxes(cdp, flip=TRUE)
    expect_match(out, "aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax)", fixed=TRUE)
})

test_that(".self_brush_box flip axes when faceting on both X and Y", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "NREADS"
    cdp[[iSEE:::.colDataYAxis]] <- "driver_1_s"
    cdp[[iSEE:::.facetByRow]] <- "Core.Type"
    cdp[[iSEE:::.facetByColumn]] <- "passes_qc_checks_s"

    brushData <- list(xmin=1, xmax=2, ymin=3, ymax=4)
    cdp[[iSEE:::.brushData]] <- brushData

    out <- iSEE:::.self_select_boxes(cdp, flip=TRUE)

    # Check that row and column are flipped (to panelvar2 and panelvar1)
    expect_match(
        out,
        "list(FacetRow=all_active[['ColumnDataPlot1']][['panelvar2']], FacetColumn=all_active[['ColumnDataPlot1']][['panelvar1']])",
        fixed=TRUE)
})

########################################
# lasso construction works with single point, open, and closed paths ----

test_that(".self_lasso_path work with a single point", {
    rdp <- pObjects$memory$ReducedDimensionPlot1

    rd <- reducedDim(sce, rdp[[iSEE:::.redDimType]])
    x_10 <- head(rd[, rdp[[iSEE:::.redDimXAxis]]], 10)
    y_10 <- head(rd[, rdp[[iSEE:::.redDimYAxis]]], 10)

    new_lasso <- list(lasso=NULL, closed=FALSE, panelvar1=NULL,
        panelvar2=NULL, mapping=list(x="X", y="Y"))
    new_lasso$coord <- matrix(
        data=c(
            min(x_10), min(y_10)
        ),
        ncol=2,
        byrow=TRUE
    )

    rdp[[iSEE:::.brushData]] <- new_lasso

    lasso_cmd <- iSEE:::.self_select_boxes(rdp, flip=FALSE)
    expect_match(lasso_cmd, "geom_point", fixed=TRUE)

})

test_that(".self_lasso_path work with an open path", {
    rdp <- pObjects$memory$ReducedDimensionPlot1

    rd <- reducedDim(sce, rdp[[iSEE:::.redDimType]])
    x_10 <- head(rd[, rdp[[iSEE:::.redDimXAxis]]], 10)
    y_10 <- head(rd[, rdp[[iSEE:::.redDimYAxis]]], 10)

    new_lasso <- list(lasso=NULL, closed=FALSE, panelvar1=NULL,
        panelvar2=NULL, mapping=list(x="X", y="Y"))
    new_lasso$coord <- matrix(
        data=c(
            min(x_10), min(y_10),
            max(x_10), min(y_10),
            max(x_10), max(y_10)
        ),
        ncol=2,
        byrow=TRUE
    )

    rdp[[iSEE:::.brushData]] <- new_lasso

    lasso_cmd <- iSEE:::.self_select_boxes(rdp, flip=FALSE)
    expect_match(lasso_cmd[1], "geom_path", fixed=TRUE)
    expect_match(lasso_cmd[2], "geom_point", fixed=TRUE)
    expect_identical(lasso_cmd[3], "scale_shape_manual(values=c('TRUE'=22, 'FALSE'=20))")
    expect_identical(lasso_cmd[4], "guides(shape='none')")
})

test_that(".self_lasso_path work with an open path and a ShapeBy covariate", {
    rdp <- pObjects$memory$ReducedDimensionPlot1

    rdp[[iSEE:::.shapeByField]] <- iSEE:::.shapeByColDataTitle

    rd <- reducedDim(sce, rdp[[iSEE:::.redDimType]])
    x_10 <- head(rd[, rdp[[iSEE:::.redDimXAxis]]], 10)
    y_10 <- head(rd[, rdp[[iSEE:::.redDimYAxis]]], 10)

    new_lasso <- list(lasso=NULL, closed=FALSE, panelvar1=NULL,
        panelvar2=NULL, mapping=list(x="X", y="Y"))
    new_lasso$coord <- matrix(
        data=c(
            min(x_10), min(y_10),
            max(x_10), min(y_10),
            max(x_10), max(y_10)
        ),
        ncol=2,
        byrow=TRUE
    )

    rdp[[iSEE:::.brushData]] <- new_lasso

    lasso_cmd <- iSEE:::.self_select_boxes(rdp, flip=FALSE)
    expect_match(lasso_cmd[1], "geom_path", fixed=TRUE)
    expect_match(lasso_cmd[2], "geom_point", fixed=TRUE)
    expect_identical(lasso_cmd[3], "scale_size_manual(values=c('TRUE'=1.5, 'FALSE'=0.25))")
    expect_identical(lasso_cmd[4], "guides(size='none')")
})

test_that(".self_lasso_path work with a closed path", {
    rdp <- pObjects$memory$ReducedDimensionPlot1

    rd <- reducedDim(sce, rdp[[iSEE:::.redDimType]])
    x_10 <- head(rd[, rdp[[iSEE:::.redDimXAxis]]], 10)
    y_10 <- head(rd[, rdp[[iSEE:::.redDimYAxis]]], 10)

    new_lasso <- list(lasso=NULL, closed=TRUE, panelvar1=NULL,
        panelvar2=NULL, mapping=list(x="X", y="Y"))
    new_lasso$coord <- matrix(
        data=c(
            min(x_10), min(y_10),
            max(x_10), min(y_10),
            max(x_10), max(y_10),
            min(x_10), max(y_10),
            min(x_10), min(y_10)
        ),
        ncol=2,
        byrow=TRUE
    )

    rdp[[iSEE:::.brushData]] <- new_lasso

    lasso_cmd <- iSEE:::.self_select_boxes(rdp, flip=FALSE)
    expect_match(lasso_cmd[1], "geom_polygon", fixed=TRUE)
})

test_that(".self_lasso_path works with multiple lassos", {
    cdp <- pObjects$memory$ColumnDataPlot
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "NREADS"
    cdp[[iSEE:::.colDataYAxis]] <- "driver_1_s"

    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))

    lassoHistory <- list(LASSO_CLOSED, LASSO_CLOSED) # yeah, ok, twice the same lasso isn't elegant but hey
    cdp[[iSEE:::.multiSelectHistory]] <- lassoHistory

    lasso_cmd <- iSEE:::.self_select_boxes(cdp, flip=FALSE)
    expect_type(lasso_cmd, "character")
    expect_length(lasso_cmd, 2*length(lassoHistory)) # length=(polygon+text)*2 lassos
    expect_match(lasso_cmd[1], "geom_polygon", fixed=TRUE)
    expect_match(lasso_cmd[2], "geom_text", fixed=TRUE)
})

test_that(".self_lasso_path flip axes when faceting on both X and Y", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "NREADS"
    cdp[[iSEE:::.colDataYAxis]] <- "driver_1_s"
    cdp[[iSEE:::.facetByRow]] <- "Core.Type"
    cdp[[iSEE:::.facetByColumn]] <- "passes_qc_checks_s"

    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))

    cdp[[iSEE:::.brushData]] <- LASSO_CLOSED

    lasso_cmd <- iSEE:::.self_select_boxes(cdp, flip=FALSE)

    # Check that row and column are flipped (to panelvar2 and panelvar1)
    expect_match(
        lasso_cmd,
        "FacetRow=all_active[['ColumnDataPlot1']][['panelvar2']], FacetColumn=all_active[['ColumnDataPlot1']][['panelvar1']]",
        fixed=TRUE)
})

########################################
# Faceting utilities all work correctly. ---

test_that(".define_facetby_for_column_plot works", {
    params <- pObjects$memory$ReducedDimensionPlot1

    params[["FacetByRow"]] <- "driver_1_s"
    params[["FacetByColumn"]] <- "Core.Type"

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    facet_out <- iSEE:::.addDotPlotDataFacets(params, env)

    expect_true("FacetRow" %in% colnames(env$plot.data))
    expect_true("FacetColumn" %in% colnames(env$plot.data))
    expect_match(facet_out$commands["FacetRow"], "driver_1_s", fixed=TRUE)
    expect_match(facet_out$commands["FacetColumn"], "Core.Type", fixed=TRUE)
})

test_that(".define_facetby_for_row_plot works", {
    rowData(sce)[, "LETTERS"] <- sample(LETTERS[1:3], nrow(sce), replace=TRUE)

    params <- pObjects$memory$RowDataPlot1
    params[["FacetByRow"]] <- "letters"
    params[["FacetByColumn"]] <- "LETTERS"

    env <- new.env()
    env$se <- sce
    .generateDotPlotData(params, env)
    facet_out <- iSEE:::.addDotPlotDataFacets(params, env)

    expect_true("FacetRow" %in% colnames(env$plot.data))
    expect_true("FacetColumn" %in% colnames(env$plot.data))
    expect_match(facet_out$commands["FacetRow"], "letters", fixed=TRUE)
    expect_match(facet_out$commands["FacetColumn"], "LETTERS", fixed=TRUE)
})

test_that(".addFacets works correctly plots", {
    params <- pObjects$memory$ReducedDimensionPlot1
    out <- iSEE:::.addFacets(params)
    expect_null(out)

    params[["FacetByRow"]] <- "driver_1_s"
    params[["FacetByColumn"]] <- "Core.Type"

    out <- iSEE:::.addFacets(params)
    expect_identical(out, "facet_grid(FacetRow ~ FacetColumn)")

    params <- pObjects$memory$RowDataPlot1
    out <- iSEE:::.addFacets(params)
    expect_null(out)

    params[["FacetByRow"]] <- "letters"
    params[["FacetByColumn"]] <- iSEE:::.noSelection

    out <- iSEE:::.addFacets(params)
    expect_identical(out, "facet_grid(FacetRow ~ .)")

    params[["FacetByRow"]] <- iSEE:::.noSelection
    params[["FacetByColumn"]] <- "letters"

    out <- iSEE:::.addFacets(params)
    expect_identical(out, "facet_grid(. ~ FacetColumn)")
})

########################################
# plot set up works correctly

test_that(".choose_plot_type flips both full and restricted plot.data for horizontal violins", {
    plot.data <- data.frame(X=runif(10), Y=factor(letters[1:10]))

    envir <- new.env()
    assign("plot.data", plot.data, envir=envir)
    assign("plot.data.all", plot.data, envir=envir)
    out <- iSEE:::.choose_plot_type(envir=envir)

    expect_identical(envir$plot.data$X, plot.data$Y)
    expect_identical(envir$plot.data$Y, plot.data$X)
    expect_identical(envir$plot.data.all$X, plot.data$Y)
    expect_identical(envir$plot.data.all$Y, plot.data$X)
})

test_that("Jitter is properly performed for faceted plots", {
    # Violin setup.
    plot.data <- data.frame(Y=runif(10), X=factor(letters[1:10]),
        FacetRow=factor(letters[1:10]), FacetColumn=factor(LETTERS[1:10]))

    out <- iSEE:::.violin_setup(plot_data=plot.data, horizontal=FALSE)

    expect_match(out[3], "jitterViolinPoints")
    expect_match(out[3], "FacetRow")
    expect_match(out[3], "FacetColumn")

    # Square setup
    plot.data <- data.frame(Y=factor(letters[1:10]), X=factor(letters[1:10]),
        FacetRow=factor(letters[1:10]), FacetColumn=factor(LETTERS[1:10]))

    out <- iSEE:::.square_setup(plot_data=plot.data)

    expect_match(out, "jitterSquarePoints")
    expect_match(out, "FacetRow")
    expect_match(out, "FacetColumn")
})

########################################
# .downsample_points ----

test_that(".downsample_points produces the appropriate code for square plots", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "driver_1_s"
    cdp[[iSEE:::.colDataYAxis]] <- "passes_qc_checks_s"

    ref <- .generateOutput(cdp, sce, all_memory=all_memory, all_contents=pObjects$contents)
    expect_false(any(grepl("subsetPointsByGrid", unlist(ref$commands))))
    expect_false(any(grepl("plot.data.pre", unlist(ref$commands))))

    cdp[[iSEE:::.plotPointDownsample]] <- TRUE
    out <- .generateOutput(cdp, sce, all_memory=all_memory, all_contents=pObjects$contents)

    expect_true(any(grepl("subsetPointsByGrid.*jitteredX.*jitteredY", unlist(out$commands))))
    expect_true(any(grepl("plot.data.pre", unlist(out$commands))))
})

test_that(".downsample_points produces the appropriate code for violin plots", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "passes_qc_checks_s"
    cdp[[iSEE:::.colDataYAxis]] <- "NREADS"

    ref <- .generateOutput(cdp, sce, all_memory=all_memory, all_contents=pObjects$contents)
    expect_false(any(grepl("subsetPointsByGrid", unlist(ref$commands))))
    expect_false(any(grepl("plot.data.pre", unlist(ref$commands))))

    cdp[[iSEE:::.plotPointDownsample]] <- TRUE
    out <- .generateOutput(cdp, sce, all_memory=all_memory, all_contents=pObjects$contents)

    expect_true(any(grepl("subsetPointsByGrid.*jitteredX", unlist(out$commands))))
    expect_true(any(grepl("plot.data.pre", unlist(out$commands))))
})

test_that(".downsample_points produces the appropriate code for horizontal violin plots", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "NREADS"
    cdp[[iSEE:::.colDataYAxis]] <- "passes_qc_checks_s"

    ref <- .generateOutput(cdp, sce, all_memory=all_memory, all_contents=pObjects$contents)
    expect_false(any(grepl("subsetPointsByGrid", unlist(ref$commands))))
    expect_false(any(grepl("plot.data.pre", unlist(ref$commands))))

    cdp[[iSEE:::.plotPointDownsample]] <- TRUE
    out <- .generateOutput(cdp, sce, all_memory=all_memory, all_contents=pObjects$contents)

    expect_true(any(grepl("subsetPointsByGrid.*jitteredX", unlist(out$commands))))
    expect_true(any(grepl("plot.data.pre", unlist(out$commands))))
})

########################################
# priority-related tests.

setClass("ColumnDataPlotPrioritized", contains="ColumnDataPlot")

setMethod(".prioritizeDotPlotData", "ColumnDataPlotPrioritized", function(x, envir) {
    cmds <- c(
        ".priority <- rep(letters[1:5], length.out=ncol(se));",
        ".priority <- factor(.priority, ordered=TRUE);",
        ".rescaled <- c(a=1, b=0.5, c=2, d=3, e=1);"
    )
    eval(parse(text=cmds), envir=envir)
    list(commands=cmds, rescaled=TRUE)
})

test_that(".generateDotPlot responds to priority", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "driver_1_s"
    cdp[[iSEE:::.colDataYAxis]] <- "passes_qc_checks_s"

    ref <- .generateOutput(cdp, sce, all_memory=all_memory, all_contents=pObjects$contents)
    expect_false(any(grepl('plot.data\\[order\\(.priority\\)', unlist(ref$commands))))

    cdpp <- as(cdp, "ColumnDataPlotPrioritized")
    out <- .generateOutput(cdpp, sce, all_memory=all_memory, all_contents=pObjects$contents)
    expect_true(any(grepl('plot.data\\[order\\(.priority\\)', unlist(out$commands))))

    expect_identical(out$contents, ref$contents)
    expect_identical(sort(rownames(out$plot$data)), sort(rownames(ref$plot$data)))
    expect_false(identical(out$plot$data, ref$plot$data))
})

test_that(".downsample_points responds to priority", {
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColData
    cdp[[iSEE:::.colDataXAxisColData]] <- "driver_1_s"
    cdp[[iSEE:::.colDataYAxis]] <- "passes_qc_checks_s"

    cdp[[iSEE:::.plotPointDownsample]] <- TRUE
    cdp[[iSEE:::.plotPointSampleRes]] <- 50

    ref <- .generateOutput(cdp, sce, all_memory=all_memory, all_contents=pObjects$contents)
    expect_false(any(grepl('grouping=\\.priority', unlist(ref$commands))))
    expect_false(any(grepl('resolution=50\\*\\.rescaled', unlist(ref$commands))))

    cdpp <- as(cdp, "ColumnDataPlotPrioritized")
    out <- .generateOutput(cdpp, sce, all_memory=all_memory, all_contents=pObjects$contents)
    expect_true(any(grepl('grouping=\\.priority', unlist(out$commands))))
    expect_true(any(grepl('resolution=50\\*\\.rescaled', unlist(out$commands))))

    expect_identical(out$contents, ref$contents)
    expect_false(identical(out$plot$data, ref$plot$data))
})

########################################
# .create_plot ----

test_that(".create_plot can add faceting commands", {
    rdp <- pObjects$memory$ReducedDimensionPlot1
    rdp[[iSEE:::.facetByColumn]] <- "driver_1_s"

    out <- .generateOutput(rdp, sce, all_memory=all_memory, all_contents=pObjects$contents)
    expect_true(any(grepl("facet_grid(. ~ FacetColumn)", out$commands$plot, fixed=TRUE)))
})

test_that("2d density contours can be added to scatter plots ", {
    rdp <- pObjects$memory$ReducedDimensionPlot1
    rdp[[iSEE:::.contourAdd]] <- TRUE
    out <- .generateOutput(rdp, sce, all_memory=all_memory, all_contents=pObjects$contents)
    expect_true(any(grepl("geom_density_2d", out$commands$plot, fixed=TRUE)))
})

test_that("plots subsetted to no data contain a geom_blank command", {
    geom_blank_cmd <- "geom_blank(data=plot.data.all, inherit.aes=FALSE, aes(x=X, y=Y)) +"

    # .scatter_plot
    out <- iSEE:::.scatter_plot(
        plot_data=data.frame(), param_choices=pObjects$memory$ReducedDimensionPlot1,
        "x_lab", "y_lab", "color_lab", "shape_lab", "size_lab", "title",
        by_row=FALSE, is_subsetted=TRUE, is_downsampled=FALSE)

    expect_identical(out[["select_blank"]], geom_blank_cmd)

    # .violin_plot
    cdp <- pObjects$memory$ColumnDataPlot1
    cdp[[iSEE:::.colDataXAxis]] <- iSEE:::.colDataXAxisColDataTitle
    cdp[[iSEE:::.colDataXAxisColData]] <- "driver_1_s"

    out <- iSEE:::.violin_plot(
        plot_data=data.frame(), param_choices=cdp,
        "x_lab", "y_lab", "color_lab", "shape_lab", "size_lab", "title",
        by_row=FALSE, is_subsetted=TRUE, is_downsampled=FALSE)

    expect_identical(out[["select_blank"]], geom_blank_cmd)

    # .square_plot
    cdp[[iSEE:::.colDataYAxis]] <- "dissection_s"

    out <- iSEE:::.square_plot(
        plot_data=data.frame(), param_choices=cdp,
        "x_lab", "y_lab", "color_lab", "shape_lab", "size_lab", "title",
        by_row=FALSE, is_subsetted=TRUE)

    expect_identical(out[["select_blank"]], geom_blank_cmd)
})

########################################
# .buildLabs ----

test_that(".buildLabs returns NULL for NULL inputs", {
    expect_null(iSEE:::.buildLabs())
})

########################################
# .add_selectby_column considers NAs ----

test_that(".add_selectby_column handles NAs correctly", {
    rdp <- pObjects$memory$ReducedDimensionPlot1 # any plot will do here.
    env <- new.env()

    env$plot.data <- data.frame(X=1, Y=2, FacetRow=1, FacetColumn=2)
    out <- iSEE:::.add_selectby_column(rdp, env)
    expect_false(any(grepl("subset.*is.na", unlist(out))))
    expect_identical(nrow(env$plot.data), 1L)

    env$plot.data <- data.frame(X=1, Y=NA_real_)
    out <- iSEE:::.add_selectby_column(rdp, env)
    expect_true(any(grepl("subset.*is.na", unlist(out))))
    expect_identical(nrow(env$plot.data), 0L)

    env$plot.data <- data.frame(X=1, Y=1, FacetRow=NA_real_)
    out <- iSEE:::.add_selectby_column(rdp, env)
    expect_true(any(grepl("subset.*is.na", unlist(out))))
    expect_identical(nrow(env$plot.data), 0L)
})


test_that(".create_guides_command produces a command when expected", {

    x <- ReducedDimensionPlot(PointSize = 1, LegendPointSize = 2)

    out <- iSEE:::.create_guides_command(x, factor(sce$driver_1_s))
    expect_identical(
        out,
        "guides(colour = guide_legend(override.aes = list(size=2)), fill = guide_legend(override.aes = list(size=2))) +"
    )

    # Same point size in plot and legend returns NULL
    x <- ReducedDimensionPlot(LegendPointSize = 2, PointSize = 2)
    out <- iSEE:::.create_guides_command(x, factor(sce$driver_1_s))
    expect_null(out)

    # Continuous coloring covariate returns NULL, no matter the point size requested
    x <- ReducedDimensionPlot(PointSize = 1, LegendPointSize = 2)

    out <- iSEE:::.create_guides_command(x, sce$NREADS)
    expect_null(out)

})
