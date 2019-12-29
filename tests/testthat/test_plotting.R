# Tests the various plotting functionality.
# library(testthat); library(iSEE); source("setup_sce.R"); source("setup_other.R"); source("test_plotting.R")

context("plotting")

memory <- list(
    RedDimPlot(),
    ColDataPlot(),
    FeatAssayPlot(),
    RowDataPlot(),
    SampAssayPlot(),
    SampAssayPlot(),
    SampAssayPlot()
)

pObjects <- mimic_live_app(sce, memory)
metadata(sce)$colormap <- ExperimentColorMap()

########################################
# .make_redDimPlot/.scatter_plot ----

test_that(".make_redDimPlot/.scatter_plot produce a valid list",{
    p.out <- .generateOutput(pObjects$memory$RedDimPlot1, sce, 
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
    rdp <- pObjects$memory$RedDimPlot1
    rdp[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle

    p.out <- .generateOutput(rdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c("X","Y","ColorBy"))
})

########################################
# .make_colDataPlot/.scatter_plot ----

test_that(".make_colDataPlot/.scatter_plot produce a valid list",{
    cdp <- pObjects$memory$ColDataPlot1
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
    cdp <- pObjects$memory$ColDataPlot1
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
    p.out <- .generateOutput(pObjects$memory$ColDataPlot1, sce, 
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
    cdp <- pObjects$memory$ColDataPlot1
    cdp[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle

    p.out <- .generateOutput(cdp, sce, 
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c("Y","X","ColorBy","GroupBy","jitteredX"))
})

########################################
# .make_colDataPlot/.square_plot ----

test_that(".make_colDataPlot/.square_plot produce a valid list",{
    cdp <- pObjects$memory$ColDataPlot1
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
    cdp <- pObjects$memory$ColDataPlot1
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
    rowData(sce)[, "letters"] <- sample(letters[1:5], nrow(sce), replace=TRUE)
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
    rowData(sce)[, "letters"] <- sample(letters[1:5], nrow(sce), replace=TRUE)
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
    fdp <- pObjects$memory$FeatAssayPlot1
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
    fdp <- pObjects$memory$FeatAssayPlot1
    fdp[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle

    p.out <- .generateOutput(fdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_named(p.out$contents, c("Y","X","ColorBy","GroupBy","jitteredX"))
})

test_that(".make_featAssayPlot works for XAxis set to Column data", {
    fdp <- pObjects$memory$FeatAssayPlot1
    fdp[[iSEE:::.featAssayXAxis]] <- iSEE:::.featAssayXAxisColDataTitle
    fdp[[iSEE:::.featAssayXAxisColData]] <- "dissection_s"

    p.out <- .generateOutput(fdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_true(any(grepl("dissection_s", unlist(p.out$commands))))
})

test_that(".make_featAssayPlot works for XAxis set to a character feature name", {
    selected_gene <- "0610009B22Rik"

    fdp <- pObjects$memory$FeatAssayPlot1
    fdp[[iSEE:::.featAssayXAxis]] <- iSEE:::.featAssayXAxisFeatNameTitle
    fdp[[iSEE:::.featAssayXAxisFeatName]] <- selected_gene

    p.out <- .generateOutput(fdp, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_true(any(grepl(selected_gene, unlist(p.out$commands))))
})

test_that(".make_featAssayPlot works for groupable colour covariate", {
    selected_coldata <- "dissection_s"

    fdp <- pObjects$memory$FeatAssayPlot1
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
    sap <- pObjects$memory$SampAssayPlot1
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

    sap <- pObjects$memory$SampAssayPlot1
    sap[[iSEE:::.rowDataXAxis]] <- iSEE:::.sampAssayXAxisRowDataTitle
    sap[[iSEE:::.rowDataXAxisRowData]] <- selected_rowdata

    p.out <- .generateOutput(sap, sce,
        all_memory=pObjects$memory, all_contents=pObjects$contents)

    expect_true(any(grepl(selected_rowdata, unlist(p.out$commands))))
})

test_that(".make_sampAssayPlot works with X variable set to Sample name", {
    selected_sample <- colnames(sce)[2]

    sap <- pObjects$memory$SampAssayPlot1
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

    cdp <- pObjects$memory$ColDataPlot
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
    params <- pObjects$memory$RedDimPlot1
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
    cdp <- pObjects$memory$ColDataPlot1
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
    cdp <- pObjects$memory$ColDataPlot1
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
    cdp <- pObjects$memory$ColDataPlot1
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
    params <- pObjects$memory$RedDimPlot1
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
    expect_match(color_out$labels$ColorBy, assayNames(sce)[1], fixed=TRUE)

    color_add <- iSEE:::.colorDotPlot(params, env$plot.data$ColorBy)
    expect_match(color_add[1], "scale_color_gradientn", fixed=TRUE)
})

test_that(".define_colorby_for_column_plot handles sample selection", {
    params <- pObjects$memory$RedDimPlot1
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
    expect_match(color_out$labels$ColorBy, assayNames(sce)[1], fixed=TRUE)

    color_add <- iSEE:::.colorDotPlot(params, env$plot.data$ColorBy)
    expect_match(color_add[1], "scale_color_gradientn", fixed=TRUE)
})

########################################
# define_shapeby_for_column_plot ----

test_that("define_shapeby_for_column_plot produces the expected commands", {
    params <- pObjects$memory$RedDimPlot1
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
    params <- pObjects$memory$RedDimPlot1
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
    expect_identical(lab_out, "plot.data$XYZ <- factor(plot.data$XYZ);")

    lab_out <- iSEE:::.coerce_type(factor(letters), input_field, as_numeric=FALSE)
    expect_identical(lab_out, NULL)

    lab_out <- iSEE:::.coerce_type(1:10, input_field, as_numeric=FALSE)
    expect_identical(lab_out, "plot.data$XYZ <- factor(plot.data$XYZ);")
})

# .process_selectby_choice ----

test_that(".process_selectby_choice works when sender is another plot", {

    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$featAssayPlot[1, iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1, ]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]],
        10)
    all_memory$redDimPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction="xy", mapping=list(x="X", y="Y"),
        brushId="dummy_brush", outputId="dummy_plot"
    ))

    select_cmd <- iSEE:::.process_selectby_choice(all_memory$featAssayPlot, all_memory)

    # check the source of the selected data
    expect_match(
        select_cmd$cmds[1],
        "shiny::brushedPoints(all_coordinates",
        fixed=TRUE
    )

    # check the source plot type
    expect_match(
        select_cmd$cmds[1],
        sourcePlotType,
        fixed=TRUE
    )

    # check that the second (hard-coded) command is present
    expect_match(
        select_cmd$cmds[2],
        "plot.data$SelectBy",
        fixed=TRUE
    )

    # union of multiple selections
    # set up selection history in the transmitter plot
    all_memory$redDimPlot[[iSEE:::.multiSelectHistory]][[1]] <- list(
        all_memory$redDimPlot[[iSEE:::.brushData]][[1]]
    )
    # request union of selection history
    all_memory$featAssayPlot[1, iSEE:::.selectMultiType] <- iSEE:::.selectMultiUnionTitle
    select_cmd <- iSEE:::.process_selectby_choice(all_memory$featAssayPlot, all_memory)
    expect_match(
        select_cmd$cmds["brush1"],
        "union",
        fixed=TRUE
    )

    # saved selection with none selected (0)
    all_memory$featAssayPlot[1, iSEE:::.selectMultiType] <- iSEE:::.selectMultiSavedTitle
    all_memory$featAssayPlot[1, iSEE:::.selectMultiSaved] <- 0L
    select_cmd <- iSEE:::.process_selectby_choice(all_memory$featAssayPlot, all_memory)
    expect_null(select_cmd$cmds)

})

test_that(".process_selectby_choice works when sender is self plot", {

    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$redDimPlot[1, iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1, ]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]],
        10)
    all_memory$redDimPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction="xy", mapping=list(x="X", y="Y"),
        brushId="dummy_brush", outputId="dummy_plot"
    ))

    select_cmd <- iSEE:::.process_selectby_choice(all_memory$redDimPlot, all_memory)

    # check the source of the selected data
    expect_match(
        select_cmd$cmds[1],
        "shiny::brushedPoints(plot.data",
        fixed=TRUE
    )
    # check that the second (hard-coded) command is present
    expect_match(
        select_cmd$cmds[2],
        "plot.data$SelectBy",
        fixed=TRUE
    )

})

test_that(".process_selectby_choice works with closed lasso selection", {

    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$featAssayPlot[1, iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1, ]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]],
        10)

    new_lasso <- list(lasso=NULL, closed=TRUE, panelvar1=NULL,
        panelvar2=NULL, mapping=list(x="X", y="Y"))
    new_lasso$coord <- matrix(
        data=c(
            min(x_10), min(y_10),
            min(x_10), max(y_10),
            max(x_10), max(y_10),
            max(x_10), min(y_10)
        ),
        ncol=2,
        byrow=TRUE
    )

    all_memory$redDimPlot[[iSEE:::.lassoData]][1] <- list(new_lasso)

    select_cmd <- iSEE:::.process_selectby_choice(all_memory$featAssayPlot, all_memory)

    # check the source plot type
    expect_match(
        select_cmd$cmds[1],
        sourcePlotType,
        fixed=TRUE
    )

    # check that the second (hard-coded) command is present
    expect_match(
        select_cmd$cmds[1],
        "all_lassos",
        fixed=TRUE
    )

})

# .create_points handles transparency selection effect ----

test_that(".create_points handles transparency selection effect", {

    # Implement a self-selection, for convenience
    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$redDimPlot[1, iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1, ]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]],
        10)
    all_memory$redDimPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction="xy", mapping=list(x="X", y="Y"),
        brushId="dummy_brush", outputId="dummy_plot"
    ))
    # Set up the selection effect type
    all_memory$redDimPlot[[iSEE:::.selectEffect]][1] <- iSEE:::.selectTransTitle

    p.out <- iSEE:::.make_redDimPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_named(
        p.out$commands$select,
        c("brushNA", "select")
    )
    # TODO: better tests

})

# .create_points handles coloured selection effect ----

test_that(".create_points handles coloured selection effect", {

    # Implement a self-selection, for convenience
    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$redDimPlot[1, iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1, ]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]],
        10)
    all_memory$redDimPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction="xy", mapping=list(x="X", y="Y"),
        brushId="dummy_brush", outputId="dummy_plot"
    ))
    # Set up the selection effect type
    all_memory$redDimPlot[1, iSEE:::.selectEffect] <- iSEE:::.selectColorTitle

    p.out <- iSEE:::.make_redDimPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_named(
        p.out$commands$select,
        c("brushNA", "select")
    )
    expect_match(
        p.out$cmd$plot["points.select_color"],
        all_memory$redDimPlot[1, iSEE:::.selectColor],
        fixed=TRUE
    )
    # TODO: better tests

})

# .create_points handles restrict selection effect ----

test_that(".create_points handles restrict selection effect", {

    # Implement a self-selection, for convenience
    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$redDimPlot[1, iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1, ]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]],
        10)
    all_memory$redDimPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction="xy", mapping=list(x="X", y="Y"),
        brushId="dummy_brush", outputId="dummy_plot"
    ))
    # Set up the selection effect type
    all_memory$redDimPlot[1, iSEE:::.selectEffect] <- iSEE:::.selectRestrictTitle

    p.out <- iSEE:::.make_redDimPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_named(
        p.out$commands$select,
        c("brushNA", "select", "saved", "subset")
    )
    expect_match(
        p.out$commands$plot["points.select_restrict"],
        "plot.data",
        fixed=TRUE
    )
    # TODO: better tests

})

# .self_lasso_path work with single point, open, and closed paths ----

test_that(".self_lasso_path work with a single point", {

    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1, ]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]],
        10)

    new_lasso <- list(lasso=NULL, closed=FALSE, panelvar1=NULL,
        panelvar2=NULL, mapping=list(x="X", y="Y"))
    new_lasso$coord <- matrix(
        data=c(
            min(x_10), min(y_10)
        ),
        ncol=2,
        byrow=TRUE
    )

    all_memory$redDimPlot[[iSEE:::.lassoData]][1] <- list(new_lasso)

    lasso_cmd <- iSEE:::.self_lasso_path(all_memory$redDimPlot, flip=FALSE)

    expect_match(
        lasso_cmd,
        "geom_point",
        fixed=TRUE
    )

})

test_that(".self_lasso_path work with an open path", {

    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1, ]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]],
        10)

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

    all_memory$redDimPlot[[iSEE:::.lassoData]][1] <- list(new_lasso)

    lasso_cmd <- iSEE:::.self_lasso_path(all_memory$redDimPlot, flip=FALSE)

    expect_match(
        lasso_cmd[1],
        "geom_path",
        fixed=TRUE
    )
    expect_match(
        lasso_cmd[2],
        "geom_point",
        fixed=TRUE
    )
    expect_identical(
        lasso_cmd[3],
        "scale_shape_manual(values=c('TRUE'=22, 'FALSE'=20))"
    )
    expect_identical(
        lasso_cmd[4],
        "guides(shape='none')"
    )

})

test_that(".self_lasso_path work with a closed and flipped path", {

    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1, ]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]],
        10)

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

    all_memory$redDimPlot[[iSEE:::.lassoData]][1] <- list(new_lasso)

    lasso_cmd <- iSEE:::.self_lasso_path(all_memory$redDimPlot, flip=FALSE)

    expect_match(lasso_cmd[1], "geom_polygon", fixed=TRUE)
    expect_match(lasso_cmd[2], "scale_fill_manual", fixed=TRUE)
    expect_identical(lasso_cmd[3], "guides(shape='none')")

})

# .define_facetby_for_column_plot ----

test_that(".define_facetby_for_column_plot works", {

    params <- all_memory$redDimPlot[1, ]

    # Default without faceting
    out <- iSEE:::.define_facetby_for_column_plot(params)
    expect_identical(out, c())

     # Non-default choices
    params[["FacetByRow"]] <- TRUE
    params[["FacetByColumn"]] <- TRUE
    params[["RowFacetByColData"]] <- "driver_1_s"
    params[["ColumnFacetByColData"]] <- "Core.Type"

    out <- iSEE:::.define_facetby_for_column_plot(params)
    expect_named(out, c("FacetRow", "FacetColumn"))
    expect_match(out["FacetRow"], "driver_1_s", fixed=TRUE)
    expect_match(out["FacetColumn"], "Core.Type", fixed=TRUE)
})

# .define_facetby_for_row_plot ----

test_that(".define_facetby_for_row_plot works", {

    # Set up the selected data
    params <- all_memory$rowDataPlot[1, ]

    # Default without faceting
    out <- iSEE:::.define_facetby_for_row_plot(params)
    expect_identical(out, c())

     # Non-default choices
    params[["FacetByRow"]] <- TRUE
    params[["FacetByColumn"]] <- TRUE
    params[["RowFacetByRowData"]] <- "mean_count"
    params[["ColumnFacetByRowData"]] <- "num_cells"

    out <- iSEE:::.define_facetby_for_row_plot(params)
    expect_named(out, c("FacetRow", "FacetColumn"))
    expect_match(out["FacetRow"], "mean_count", fixed=TRUE)
    expect_match(out["FacetColumn"], "num_cells", fixed=TRUE)
})


# .add_facets works for column- and row-based plots ----

test_that(".add_facets works for column data plots", {

    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1, ]

    # Default choices

    out <- iSEE:::.add_facets(params)
    expect_null(out)

    # Non-default choices
    params[["FacetByRow"]] <- TRUE
    params[["FacetByColumn"]] <- TRUE
    params[["RowFacetByColData"]] <- "driver_1_s"
    params[["ColumnFacetByColData"]] <- "Core.Type"

    out <- iSEE:::.add_facets(params)
    expect_identical(out, "facet_grid(FacetRow ~ FacetColumn)")
})

test_that(".add_facets works for row data plots", {

    # Set up the selected data
    params <- all_memory$rowDataPlot[1, ]

    # Default choices

    out <- iSEE:::.add_facets(params)
    expect_null(out)

    # Non-default choices
    params[["FacetByRow"]] <- TRUE
    params[["FacetByColumn"]] <- TRUE
    params[["RowFacetByRowData"]] <- "mean_count"
    params[["ColumnFacetByRowData"]] <- "num_cells"

    out <- iSEE:::.add_facets(params)
    expect_identical(out, "facet_grid(FacetRow ~ FacetColumn)")
})

# .choose_plot_type ----

test_that(".choose_plot_type flips both full and restricted plot.data for horizontal violins", {

    plot.data <- data.frame(X=seq_along(letters), Y=letters)

    envir <- new.env()
    assign("plot.data.all", plot.data, envir=envir)
    out <- iSEE:::.choose_plot_type(group_X=FALSE, group_Y=TRUE, envir=envir)

    expect_true(any(grepl("plot.data.all$X <- plot.data.all$Y;", out, fixed=TRUE)))
    expect_true(any(grepl("plot.data.all$Y <- tmp;", out, fixed=TRUE)))

})

# .downsample_points ----

test_that(".downsample_points produces the appropriate code", {

    # for square plots

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "driver_1_s"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "passes_qc_checks_s"
    all_memory$colDataPlot[1, iSEE:::.plotPointDownsample] <- TRUE
    all_memory$colDataPlot[1, iSEE:::.plotPointSampleRes] <- 200

    envir <- new.env()
    plotData <- data.frame(
        X=colData(sce)[, all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData]],
        Y=colData(sce)[, all_memory$colDataPlot[1, iSEE:::.colDataYAxis]],
        jitteredX=rnorm(ncol(sce)),
        jitteredY=rnorm(ncol(sce)),
        row.names=colnames(sce)
    )
    assign("plot.data", plotData, envir=envir)
    assign("plot.type", "square", envir=envir)

    out <- iSEE:::.downsample_points(all_memory$colDataPlot[1, ], envir)
    expect_identical(out, c(
        "plot.data.pre <- plot.data;",
        "# Randomize data points to avoid a data set bias during the downsampling",
        "set.seed(100);",
        "plot.data <- plot.data[sample(nrow(plot.data)), , drop=FALSE];",
        "plot.data <- subset(plot.data, subsetPointsByGrid(jitteredX, jitteredY, resolution=200));",
        ""))

    # for violin plots

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "passes_qc_checks_s"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "NREADS"
    all_memory$colDataPlot[1, iSEE:::.plotPointDownsample] <- TRUE
    all_memory$colDataPlot[1, iSEE:::.plotPointSampleRes] <- 200

    envir <- new.env()
    plotData <- data.frame(
        X=colData(sce)[, all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData]],
        Y=colData(sce)[, all_memory$colDataPlot[1, iSEE:::.colDataYAxis]],
        jitteredX=rnorm(ncol(sce)),
        row.names=colnames(sce)
    )
    assign("plot.data", plotData, envir=envir)
    assign("plot.type", "violin_horizontal", envir=envir)
    # assign("plot.type", "violin", envir=envir)

    out <- iSEE:::.downsample_points(all_memory$colDataPlot[1, ], envir)
    expect_identical(out, c(
        "plot.data.pre <- plot.data;",
        "# Randomize data points to avoid a data set bias during the downsampling",
        "set.seed(100);",
        "plot.data <- plot.data[sample(nrow(plot.data)), , drop=FALSE];",
        "plot.data <- subset(plot.data, subsetPointsByGrid(jitteredX, Y, resolution=200));",
        ""))

    # for horizontal violin plots (X and Y are flipped)

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "passes_qc_checks_s"
    all_memory$colDataPlot[1, iSEE:::.plotPointDownsample] <- TRUE
    all_memory$colDataPlot[1, iSEE:::.plotPointSampleRes] <- 200

    envir <- new.env()
    plotData <- data.frame(
        X=colData(sce)[, all_memory$colDataPlot[1, iSEE:::.colDataYAxis]],
        Y=colData(sce)[, all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData]],
        jitteredX=rnorm(ncol(sce)),
        row.names=colnames(sce)
    )
    assign("plot.data", plotData, envir=envir)
    assign("plot.type", "violin_horizontal", envir=envir)
    # assign("plot.type", "violin", envir=envir)

    out <- iSEE:::.downsample_points(all_memory$colDataPlot[1, ], envir)
    expect_identical(out, c(
        "plot.data.pre <- plot.data;",
        "# Randomize data points to avoid a data set bias during the downsampling",
        "set.seed(100);",
        "plot.data <- plot.data[sample(nrow(plot.data)), , drop=FALSE];",
        "plot.data <- subset(plot.data, subsetPointsByGrid(jitteredX, Y, resolution=200));",
        ""))

})

# .create_plot ----

test_that(".create_plot can add faceting commands", {

    all_memory$redDimPlot[1, iSEE:::.facetByRow] <- TRUE
    all_memory$redDimPlot[1, iSEE:::.facetColumnsByColData] <- "driver_1_s"

    out <- iSEE:::.make_redDimPlot(1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_true(any(grepl("facet_grid(FacetRow ~ .)", out$commands$plot, fixed=TRUE)))
})

# Contours ----

test_that("2d density contours can be added to scatter plots ", {

    all_memory$redDimPlot[1, iSEE:::.contourAddTitle] <- TRUE

    out <- iSEE:::.scatter_plot(
        plot_data=data.frame(), param_choices=all_memory$redDimPlot,
        "x_lab", "y_lab", "color_lab", "shape_lab", "size_lab", "title",
        by_row=FALSE, is_subsetted=TRUE, is_downsampled=FALSE)

    expect_identical(out[["contours"]], "geom_density_2d(aes(x=X, y=Y), plot.data, colour='blue') +")

})

# .scatter_plot ----

test_that("plots subsetted to no data contain a geom_blank command", {

    geom_blank_cmd <- "geom_blank(data=plot.data.all, inherit.aes=FALSE, aes(x=X, y=Y)) +"

    # .scatter_plot

    out <- iSEE:::.scatter_plot(
        plot_data=data.frame(), param_choices=all_memory$redDimPlot,
        "x_lab", "y_lab", "color_lab", "shape_lab", "size_lab", "title",
        by_row=FALSE, is_subsetted=TRUE, is_downsampled=FALSE)

    expect_identical(out[["select_blank"]], geom_blank_cmd)

    # .violin_plot

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColDataTitle
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "driver_1_s"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis]

    out <- iSEE:::.violin_plot(
        plot_data=data.frame(), param_choices=all_memory$colDataPlot,
        "x_lab", "y_lab", "color_lab", "shape_lab", "size_lab", "title",
        by_row=FALSE, is_subsetted=TRUE, is_downsampled=FALSE)

    expect_identical(out[["select_blank"]], geom_blank_cmd)

    # .square_plot

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColDataTitle
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "driver_1_s"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "dissection_s"

    out <- iSEE:::.square_plot(
        plot_data=data.frame(), param_choices=all_memory$colDataPlot, sce,
        "x_lab", "y_lab", "color_lab", "shape_lab", "size_lab", "title",
        by_row=FALSE, is_subsetted=TRUE)

    expect_identical(out[["select_blank"]], geom_blank_cmd)

})

# .violin_setup ----

test_that("Jitter is properly performed for faceted plots", {

    # .violin_setup

    plotData <- data.frame(
        FacetRow=letters,
        FacetColumn=LETTERS
    )

    out <- iSEE:::.violin_setup(plot_data=plotData, horizontal=FALSE)

    expect_identical(out, c(
        group="plot.data$GroupBy <- plot.data$X;",
        seed="set.seed(100);",
        calcX="plot.data$jitteredX <- iSEE::jitterViolinPoints(plot.data$X, plot.data$Y, \n    list(FacetRow=plot.data$FacetRow, FacetColumn=plot.data$FacetColumn),\n    width=0.4, varwidth=FALSE, adjust=1,\n    method='quasirandom', nbins=NULL);" ))

    # .square_setup

    plotData <- data.frame(
        FacetRow=letters,
        FacetColumn=LETTERS
    )

    out <- iSEE:::.square_setup(plot_data=plotData)

    expect_identical(out, c(
        jitter="set.seed(100);\nj.out <- iSEE:::jitterSquarePoints(plot.data$X, plot.data$Y,\n    list(FacetRow=plot.data$FacetRow, FacetColumn=plot.data$FacetColumn));\nsummary.data <- j.out$summary;\nplot.data$jitteredX <- j.out$X;\nplot.data$jitteredY <- j.out$Y;")
        )

})

# .build_labs ----

test_that(".build_labs returns NULL for NULL inputs", {

    expect_null(iSEE:::.build_labs())

})

# .self_brush_box ----

test_that(".self_brush_box draw multiple shiny brushes", {

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "driver_1_s"

    brushHistory <- list(
        list(xmin=1, xmax=2, ymin=3, ymax=4),
        list(xmin=2, xmax=3, ymin=4, ymax=5)
    )
    all_memory$colDataPlot[[iSEE:::.multiSelectHistory]][[1]] <- brushHistory

    out <- iSEE:::.self_brush_box(all_memory$colDataPlot, flip=TRUE)
    expect_length(out, 2*length(brushHistory))
    expect_type(out, "character")
    expect_match(out[1], "geom_rect", fixed=TRUE)
    expect_match(out[2], "geom_text", fixed=TRUE)
    expect_match(out[3], "geom_rect", fixed=TRUE)
    expect_match(out[4], "geom_text", fixed=TRUE)

})

test_that(".self_brush_box can flip axes", {

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "driver_1_s"

    brushData <- list(xmin=1, xmax=2, ymin=3, ymax=4)
    all_memory$colDataPlot[[iSEE:::.brushData]][[1]] <- list(brushData)

    out <- iSEE:::.self_brush_box(all_memory$colDataPlot, flip=TRUE)
    expect_match(out, "aes(xmin=ymin, xmax=ymax, ymin=xmin, ymax=xmax)", fixed=TRUE)

})

test_that(".self_brush_box flip axes when faceting on both X and Y", {

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "driver_1_s"
    all_memory$colDataPlot[1, iSEE:::.facetByRow] <- TRUE
    all_memory$colDataPlot[1, iSEE:::.facetByColumn] <- TRUE

    brushData <- list(xmin=1, xmax=2, ymin=3, ymax=4)
    all_memory$colDataPlot[[iSEE:::.brushData]][[1]] <- list(brushData)

    out <- iSEE:::.self_brush_box(all_memory$colDataPlot, flip=TRUE)

    # Check that row and column are flipped (to panelvar2 and panelvar1)
    expect_match(
        out,
        "list(FacetRow=all_brushes[['colDataPlot1']][['panelvar2']], FacetColumn=all_brushes[['colDataPlot1']][['panelvar1']])",
        fixed=TRUE)

    # Check that the faceting data is appended to the brush data
    expect_match(
        out,
        "do.call(data.frame, append(all_brushes[['colDataPlot1']][c('xmin', 'xmax', 'ymin', 'ymax')], list(FacetRow=all_brushes[['colDataPlot1']][['panelvar2']], FacetColumn=all_brushes[['colDataPlot1']][['panelvar1']])))", fixed=TRUE
    )

})

# .self_lasso_path ----

test_that(".self_lasso_path works with multiple lassos", {

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "driver_1_s"

    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))
    lassoHistory <- list(LASSO_CLOSED, LASSO_CLOSED) # yeah, ok, twice the same lasso isn't elegant but hey
    all_memory$colDataPlot[[iSEE:::.multiSelectHistory]][[1]] <- lassoHistory

    out <- iSEE:::.self_lasso_path(all_memory$colDataPlot)
    expect_type(out, "character")
    # length=(polygon+text)*2 lassos + scale_fill_manual + guides
    expect_length(out, 2*length(lassoHistory)+2)
    expect_match(out[1], "geom_polygon", fixed=TRUE)
    expect_match(out[2], "geom_text", fixed=TRUE)
    expect_match(out[3], "scale_fill_manual", fixed=TRUE)
    expect_match(out[4], "guides(shape='none')", fixed=TRUE)
})

test_that(".self_lasso_path flip axes when faceting on both X and Y", {

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "driver_1_s"
    all_memory$colDataPlot[1, iSEE:::.facetByRow] <- TRUE
    all_memory$colDataPlot[1, iSEE:::.facetByColumn] <- TRUE

    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))
    all_memory$colDataPlot[[iSEE:::.lassoData]][[1]] <- LASSO_CLOSED

    out <- iSEE:::.self_lasso_path(all_memory$colDataPlot, flip=TRUE)

    # Check that row and column are flipped (to panelvar2 and panelvar1)
    expect_match(
        out[1],
        "FacetRow=all_lassos[['colDataPlot1']][['panelvar2']], FacetColumn=all_lassos[['colDataPlot1']][['panelvar1']]",
        fixed=TRUE)

    # Check that the faceting data is appended to the brush data
    expect_match(
        out[1],
        "data.frame(X=all_lassos[['colDataPlot1']]$coord[, 1], Y=all_lassos[['colDataPlot1']]$coord[, 2], FacetRow=all_lassos[['colDataPlot1']][['panelvar2']], FacetColumn=all_lassos[['colDataPlot1']][['panelvar1']])", fixed=TRUE
    )

    expect_identical(
        out[2],
        "scale_fill_manual(values=c('TRUE'='#DB0230', 'FALSE'='#F7CCD5'), labels=NULL)")

    expect_identical(out[3], "guides(shape='none')")

})

test_that(".self_lasso_path leaves the shape legend visible if applied to data points", {

    # Note: If data points are not shaped, the shape aesthetic is used to shape
    # the waypoints of the lasso differently from the start/closing point
    # In this case, the shape legend should not be shown.

    all_memory$redDimPlot[1, iSEE:::.shapeByField] <- iSEE:::.shapeByColDataTitle
    all_memory$redDimPlot[1, iSEE:::.shapeByColData] <- "driver_1_s"

    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(1, 2, 2, 1, 1, 1, 1, 2, 2, 1), ncol=2))
    all_memory$redDimPlot[[iSEE:::.lassoData]][[1]] <- LASSO_CLOSED

    out <- iSEE:::.self_lasso_path(all_memory$redDimPlot, flip=FALSE)
    # Do not expect any call to "guides()"
    expect_false(any(grepl("guides", out)))
})

test_that(".self_lasso_path uses the size aesthetic to distinguish waypoints of an open lasso when shape is mapped to a covariate", {

    # Note: If data points are not shaped, the shape aesthetic is used to shape
    # the waypoints of the lasso differently from the start/closing point

    all_memory$redDimPlot[1, iSEE:::.shapeByField] <- iSEE:::.shapeByColDataTitle
    all_memory$redDimPlot[1, iSEE:::.shapeByColData] <- "driver_1_s"

    LASSO_OPEN <- list(
        lasso=NULL,
        closed=FALSE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(1, 2, 2, 1, 1, 1, 2, 2), ncol=2))
    all_memory$redDimPlot[[iSEE:::.lassoData]][[1]] <- LASSO_OPEN

    out <- iSEE:::.self_lasso_path(all_memory$redDimPlot, flip=FALSE)

    expect_identical(
        out,
        c(
            "geom_path(aes(x=X, y=Y),\n    data=data.frame(X=all_lassos[['redDimPlot1']]$coord[, 1], Y=all_lassos[['redDimPlot1']]$coord[, 2]),\n    inherit.aes=FALSE, alpha=1, color='#3565AA', linetype='longdash')",
            "geom_point(aes(x=X, y=Y, size=First),\n    data=data.frame(X=all_lassos[['redDimPlot1']]$coord[, 1], Y=all_lassos[['redDimPlot1']]$coord[, 2],\n        First=seq_len(nrow(all_lassos[['redDimPlot1']]$coord)) == 1L),\n    inherit.aes=FALSE, alpha=1, stroke=1, shape=22, color='#3565AA')",
            "scale_size_manual(values=c('TRUE'=1.5, 'FALSE'=0.25))",
            "guides(size='none')")
    )

})
