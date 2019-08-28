context("plotting")

# Set up plotting parameters
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 1)
featAssayArgs <- featAssayPlotDefaults(sce, 1)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
sampAssayArgs <- sampAssayPlotDefaults(sce, 3)

# Set up memory
sce <- iSEE:::.precompute_UI_info(sce, NULL, NULL)
all_memory <- iSEE:::.setup_memory(sce,
    redDimArgs=redDimArgs,
    colDataArgs=colDataArgs,
    featAssayArgs=featAssayArgs,
    rowStatArgs=NULL,
    rowDataArgs=rowDataArgs,
    sampAssayArgs=sampAssayArgs,
    colStatArgs=NULL,
    customDataArgs=NULL,
    customStatArgs=NULL,
    heatMapArgs=NULL,
    redDimMax=1,
    colDataMax=1,
    featAssayMax=1,
    rowStatMax=0,
    rowDataMax=1,
    sampAssayMax=0,
    colStatMax=0,
    customDataMax=0,
    customStatMax=0,
    heatMapMax=0)

all_coordinates <- list()

####################
# Tests start here #
####################

# .make_redDimPlot/.scatter_plot ----

test_that(".make_redDimPlot/.scatter_plot produce a valid list",{

    p.out <- iSEE:::.make_redDimPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    # return value is a named list
    expect_type(
        p.out,
        "list"
    )
    expect_named(
        p.out,
        c("cmd_list", "xy", "plot")
    )

    # cmd value is a named list
    expect_type(
        p.out$cmd_list,
        "list"
    )
    expect_named(
        p.out$cmd_list,
        c('data', 'select', 'setup', 'plot')
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("X","Y")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_redDimPlot/.scatter_plot produce a valid xy with color", {

    all_memory$redDimPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
    p.out <- iSEE:::.make_redDimPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c("X","Y","ColorBy")
    )

})

# .make_colDataPlot/.scatter_plot ----

test_that(".make_colDataPlot/.scatter_plot produce a valid list",{

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "NALIGNED"

    p.out <- iSEE:::.make_colDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    # return value is a named list
    expect_type(
        p.out,
        "list"
    )
    expect_named(
        p.out,
        c("cmd_list", "xy", "plot")
    )

    # cmd value is a named list
    expect_type(
        p.out$cmd_list,
        "list"
    )
    expect_named(
        p.out$cmd_list,
        c('data', 'select', 'setup', 'plot')
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("Y","X")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_colDataPlot/.scatter_plot produce a valid xy with color", {

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "NALIGNED"

    all_memory$colDataPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
    p.out <- iSEE:::.make_colDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c('Y', 'X', 'ColorBy')
    )

})

# .make_colDataPlot/.violin_plot ----

test_that(".make_colDataPlot/.violin_plot produce a valid list",{

    p.out <- iSEE:::.make_colDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    # return value is a named list
    expect_type(
        p.out,
        "list"
    )
    expect_named(
        p.out,
        c("cmd_list", "xy", "plot")
    )

    # cmd value is a named list
    expect_type(
        p.out$cmd_list,
        "list"
    )
    expect_named(
        p.out$cmd_list,
        c("data", "select", "setup", "plot")
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("Y", "X", "GroupBy", "jitteredX")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_colDataPlot/.violin_plot produce a valid xy with color", {

    all_memory$colDataPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
    p.out <- iSEE:::.make_colDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c("Y","X","ColorBy","GroupBy","jitteredX")
    )

})

# .make_colDataPlot/.square_plot ----

test_that(".make_colDataPlot/.square_plot produce a valid list",{

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "driver_1_s"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "passes_qc_checks_s"

    p.out <- iSEE:::.make_colDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    # return value is a named list
    expect_type(
        p.out,
        "list"
    )
    expect_named(
        p.out,
        c("cmd_list", "xy", "plot")
    )

    # cmd value is a named list
    expect_type(
        p.out$cmd_list,
        "list"
    )
    expect_named(
        p.out$cmd_list,
        c("data", "select", "setup", "plot")
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("Y","X","jitteredX","jitteredY")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_colDataPlot/.square_plot produce a valid xy with color", {

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "driver_1_s"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "passes_qc_checks_s"
    all_memory$colDataPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle

    p.out <- iSEE:::.make_colDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c("Y","X","ColorBy","jitteredX","jitteredY")
    )

})

# .make_rowDataPlot/.scatter_plot ----

test_that(".make_rowDataPlot/.scatter_plot produce a valid list",{

    all_memory$rowDataPlot[1, iSEE:::.rowDataXAxis] <- iSEE:::.rowDataXAxisRowData
    all_memory$rowDataPlot[1, iSEE:::.rowDataXAxisRowData] <- "num_cells"
    all_memory$rowDataPlot[1, iSEE:::.rowDataYAxis] <- "mean_count"

    p.out <- iSEE:::.make_rowDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    # return value is a named list
    expect_type(
        p.out,
        "list"
    )
    expect_named(
        p.out,
        c("cmd_list", "xy", "plot")
    )

    # cmd value is a named list
    expect_type(
        p.out$cmd_list,
        "list"
    )
    expect_named(
        p.out$cmd_list,
        c("data", "select", "setup", "plot")
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("Y","X")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_rowDataPlot/.violin_plot produce a valid xy with color", {

    all_memory$rowDataPlot[1, iSEE:::.rowDataXAxis] <- iSEE:::.rowDataXAxisRowData
    all_memory$rowDataPlot[1, iSEE:::.rowDataXAxisRowData] <- "num_cells"
    all_memory$rowDataPlot[1, iSEE:::.rowDataYAxis] <- "mean_count"

    all_memory$rowDataPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByRowDataTitle
    p.out <- iSEE:::.make_rowDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c('Y', 'X', 'ColorBy')
    )

    # Color by feature name
    all_memory$rowDataPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByFeatNameTitle

    p.out <- iSEE:::.make_rowDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

})

# .make_rowDataPlot/.violin_plot ----

test_that(".make_rowDataPlot/.violin_plot produce a valid list",{

    p.out <- iSEE:::.make_rowDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    # return value is a named list
    expect_type(
        p.out,
        "list"
    )
    expect_named(
        p.out,
        c("cmd_list", "xy", "plot")
    )

    # cmd value is a named list
    expect_type(
        p.out$cmd_list,
        "list"
    )
    expect_named(
        p.out$cmd_list,
        c("data", "select", "setup", "plot")
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("Y", "X", "GroupBy", "jitteredX")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_rowDataPlot/.violin_plot produce a valid xy with color", {

    all_memory$rowDataPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByRowDataTitle
    p.out <- iSEE:::.make_rowDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c("Y","X","ColorBy","GroupBy","jitteredX")
    )

    # Color by feature name
    all_memory$rowDataPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByFeatNameTitle

    p.out <- iSEE:::.make_rowDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

})

# .make_rowDataPlot/.square_plot ----

test_that(".make_rowDataPlot/.square_plot produce a valid list",{

    rowData(sce)[, "letters"] <- sample(letters[1:5], nrow(sce), replace=TRUE)
    rowData(sce)[, "LETTERS"] <- sample(LETTERS[1:3], nrow(sce), replace=TRUE)

    all_memory$rowDataPlot[1, iSEE:::.rowDataXAxis] <- iSEE:::.rowDataXAxisRowData
    all_memory$rowDataPlot[1, iSEE:::.rowDataXAxisRowData] <- "letters"
    all_memory$rowDataPlot[1, iSEE:::.rowDataYAxis] <- "LETTERS"

    p.out <- iSEE:::.make_rowDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    # return value is a named list
    expect_type(
        p.out,
        "list"
    )
    expect_named(
        p.out,
        c("cmd_list", "xy", "plot")
    )

    # cmd value is a named list
    expect_type(
        p.out$cmd_list,
        "list"
    )
    expect_named(
        p.out$cmd_list,
        c("data", "select", "setup", "plot")
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c('Y', 'X', 'jitteredX', 'jitteredY')
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_rowDataPlot/.square_plot produce a valid xy with color",{

    rowData(sce)[, "letters"] <- sample(letters[1:5], nrow(sce), replace=TRUE)
    rowData(sce)[, "LETTERS"] <- sample(LETTERS[1:3], nrow(sce), replace=TRUE)

    all_memory$rowDataPlot[1, iSEE:::.rowDataXAxis] <- iSEE:::.rowDataXAxisRowData
    all_memory$rowDataPlot[1, iSEE:::.rowDataXAxisRowData] <- "letters"
    all_memory$rowDataPlot[1, iSEE:::.rowDataYAxis] <- "LETTERS"

    all_memory$rowDataPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByRowDataTitle

    p.out <- iSEE:::.make_rowDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    # return value is a named list
    expect_type(
        p.out,
        "list"
    )
    expect_named(
        p.out,
        c("cmd_list", "xy", "plot")
    )

    # cmd value is a named list
    expect_type(
        p.out$cmd_list,
        "list"
    )
    expect_named(
        p.out$cmd_list,
        c("data", "select", "setup", "plot")
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c('Y', 'X', 'ColorBy', 'jitteredX', 'jitteredY')
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

    # Color by feature name
    all_memory$rowDataPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByFeatNameTitle

    p.out <- iSEE:::.make_rowDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

})

# .make_featAssayPlot/.scatter_plot ----

test_that(".make_featAssayPlot/.scatter_plot produce a valid list",{

    all_memory$featAssayPlot[1, iSEE:::.featAssayYAxisRowTable] <- "Row statistics table 1"
    all_memory$featAssayPlot[1, iSEE:::.featAssayYAxisRowTable] <- "Row statistics table 1"

    p.out <- iSEE:::.make_featAssayPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    # return value is a named list
    expect_type(
        p.out,
        "list"
    )
    expect_named(
        p.out,
        c("cmd_list", "xy", "plot")
    )

    # cmd value is a named list
    expect_type(
        p.out$cmd_list,
        "list"
    )
    expect_named(
        p.out$cmd_list,
        c("data", "select", "setup", "plot")
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("Y", "X", "GroupBy", "jitteredX")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_featAssayPlot/.scatter_plot produce a valid xy with color", {

    all_memory$featAssayPlot[1, iSEE:::.featAssayYAxisRowTable] <- "Row statistics table 1"
    all_memory$featAssayPlot[1, iSEE:::.featAssayYAxisRowTable] <- "Row statistics table 1"
    all_memory$featAssayPlot[1, iSEE:::.colorByRowTable] <- "Row statistics table 1"
    all_memory$featAssayPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByFeatNameTitle

    p.out <- iSEE:::.make_featAssayPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c("Y","X","ColorBy","GroupBy","jitteredX")
    )

})

test_that(".make_featAssayPlot fails for YAxisFeatName set to a character value", {
    # change the value locally for the specific test
    selected_gene <- "0610009B22Rik"

    all_memory$featAssayPlot[1,  iSEE:::.featAssayYAxisFeatName] <- selected_gene

    expect_error(
        iSEE:::.make_featAssayPlot(id=1, all_memory, all_coordinates, sce, ExperimentColorMap()),
        "invalid format '%i'; use format %s for character objects",
        fixed=TRUE)

})

test_that(".make_featAssayPlot works for XAxis set to Column data", {
    # change the value locally for the specific test
    all_memory$featAssayPlot[1, iSEE:::.featAssayXAxis] <- iSEE:::.featAssayXAxisColDataTitle
    all_memory$featAssayPlot[1, iSEE:::.featAssayXAxisColData] <- "dissection_s"

    p.out <- iSEE:::.make_featAssayPlot(id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_match(
        p.out$cmd_list$data['x'],
        "dissection_s",
        fixed=TRUE
    )

})

test_that(".make_featAssayPlot fails for XAxis set to a character feature name", {
    selected_gene <- "0610009B22Rik"

    # change the value locally for the specific test
    all_memory$featAssayPlot[1, iSEE:::.featAssayXAxis] <- iSEE:::.featAssayXAxisFeatNameTitle
    all_memory$featAssayPlot[1, iSEE:::.featAssayXAxisFeatName] <- selected_gene

    expect_error(
        iSEE:::.make_featAssayPlot(id=1, all_memory, all_coordinates, sce, ExperimentColorMap()),
        "invalid format '%i'; use format %s for character objects",
        fixed=TRUE)

})

test_that(".make_featAssayPlot works for groupable colour covariate", {
    selected_coldata <- "dissection_s"

    # change the value locally for the specific test
    all_memory$featAssayPlot[1, iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
    all_memory$featAssayPlot[1, iSEE:::.colorByColData] <- selected_coldata

    p.out <- iSEE:::.make_featAssayPlot(id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_match(
        p.out$cmd_list$data['color'],
        selected_coldata,
        fixed=TRUE
    )

    expect_identical(p.out$cmd_list$data[['more_color']], "plot.data$ColorBy <- factor(plot.data$ColorBy);")

    expect_match(
        p.out$cmd_list$plot["scale_color1"],
        "^scale_color_manual"
    )
    expect_match(
        p.out$cmd_list$plot["scale_color1"],
        selected_coldata,
        fixed=TRUE
    )

    expect_match(
        p.out$cmd_list$plot["scale_color2"],
        "^scale_fill_manual"
    )
    expect_match(
        p.out$cmd_list$plot["scale_color2"],
        selected_coldata,
        fixed=TRUE
    )

})

# .make_sampAssayPlot ----

test_that(".make_sampAssayPlot works with X covariate set to None", {

    p.out <- iSEE:::.make_sampAssayPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    # return value is a named list
    expect_type(
        p.out,
        "list"
    )
    expect_named(
        p.out,
        c("cmd_list", "xy", "plot")
    )

    # cmd value is a named list
    expect_type(
        p.out$cmd_list,
        "list"
    )
    expect_named(
        p.out$cmd_list,
        c("data", "select", "setup", "plot")
    )

    # x cmd should create a single value
    expect_identical(
        p.out$cmd_list$data[["x"]],
        "plot.data$X <- factor(character(nrow(se)));"
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("Y", "X", "GroupBy", "jitteredX")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_sampAssayPlot works with X variable set to Row data", {

    selected_rowdata <- "num_cells"

    # change the value locally for the specific test
    all_memory$sampAssayPlot[1, iSEE:::.rowDataXAxis] <- iSEE:::.sampAssayXAxisRowDataTitle
    all_memory$sampAssayPlot[1, iSEE:::.rowDataXAxisRowData] <- selected_rowdata

    p.out <- iSEE:::.make_sampAssayPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_match(p.out$cmd_list$data[["x"]], selected_rowdata, fixed=TRUE)
    expect_match(p.out$cmd_list$plot[["labs"]], selected_rowdata, fixed=TRUE)

})

test_that(".make_sampAssayPlot works with X variable set to Sample name", {

    selected_sample <- 2L

    # change the value locally for the specific test
    all_memory$sampAssayPlot[1, iSEE:::.rowDataXAxis] <- iSEE:::.sampAssayXAxisSampNameTitle
    all_memory$sampAssayPlot[1, iSEE:::.sampAssayXAxisSampName] <- selected_sample

    p.out <- iSEE:::.make_sampAssayPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_match(
        p.out$cmd_list$data[["x"]],
        sprintf("plot.data$X <- assay(se, 2, withDimnames=FALSE)[, %i];", selected_sample),
        fixed=TRUE)

    expect_match(p.out$cmd_list$plot[["labs"]], colnames(sce)[selected_sample], fixed=TRUE)
})

# .make_colDataPlot/.create_plot horizontal violin plots ----

test_that(".make_colDataPlot/.create_plot can produce horizontal violins", {
    selected_coldataX <- "NREADS"
    selected_coldataY <- "driver_1_s"

    # change the value locally for the specific test
    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colorByColDataTitle
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- selected_coldataX

    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- selected_coldataY

    all_memory$colDataPlot[1, iSEE:::.colorByField] <- iSEE:::.colDataXAxisNothingTitle

    p.out <- iSEE:::.make_colDataPlot(id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_match(
        p.out$cmd_list$data['y'],
        selected_coldataY,
        fixed=TRUE
    )

    expect_match(
        p.out$cmd_list$data['x'],
        selected_coldataX,
        fixed=TRUE
    )

    expect_named(
        p.out$xy,
        c("Y", "X", "GroupBy", "jitteredX")
    )

})

# .scatter_plot plot with zoom ----

test_that(".scatter_plot works with zoom",{

    # Identify range of data
    params <- all_memory$redDimPlot[1, ]
    x_range <- range(head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]]
    ), 10)
    y_range <- range(head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]]
    ), 10)
    # Set zoom min/max to the first two distinct values in X/Y direction
    zoom_range <- c(x_range, y_range)
    names(zoom_range) <- c("xmin","xmax","ymin","ymax")
    # Set the zoom
    all_memory$redDimPlot[[iSEE:::.zoomData]][1] <- list(zoom_range)

    p.out <- iSEE:::.make_redDimPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    params <- all_memory$redDimPlot[1, ]
    expected_xy <- data.frame(
        X=reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimXAxis]]],
        Y=reducedDim(sce, params[[iSEE:::.redDimType]])[, params[[iSEE:::.redDimYAxis]]],
        row.names=colnames(sce)
    )

    expect_identical(p.out$xy, expected_xy)

})

# .make_colDataPlot/.violin_plot works with zoom ----

test_that(".make_colDataPlot/.violin_plot works with zoom",{

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "driver_1_s"

    # Identify valid values
    x_unique <- unique(as.numeric(as.factor(colData(sce)[,
        all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData]
        ])))
    y_range <- range(head(colData(sce)[,
        all_memory$colDataPlot[1, iSEE:::.colDataYAxis]
        ]), 10)
    # Set zoom min/max to the first two distinct values in X/Y direction
    zoom_range <- c(
        sort(head(x_unique, 2)),
        y_range
    )
    # Extend the zoom to perfectly include the min/max boxes
    zoom_range <- zoom_range + c(-0.5, 0.5, 0, 0)
    names(zoom_range) <- c("xmin","xmax","ymin","ymax")
    # Set the zoom
    all_memory$colDataPlot[[iSEE:::.zoomData]][1] <- list(zoom_range)

    p.out <- iSEE:::.make_colDataPlot(id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    params <- all_memory$colDataPlot[1, ]
    expected_xy <- data.frame(
        Y=colData(sce)[, params[[iSEE:::.colDataYAxis]]],
        X=colData(sce)[, params[[iSEE:::.colDataXAxisColData]]],
        row.names=colnames(sce)
    )

    expect_identical(p.out$xy[, c("Y", "X")], expected_xy)

})

# .make_colDataPlot/.violin_plot works with horizontal zoom ----

test_that(".make_colDataPlot/.violin_plot works with zoom",{

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "driver_1_s"

    # Identify valid values
    x_range <- range(head(colData(sce)[,
        all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData]
        ]), 10)
    y_unique <- unique(as.numeric(as.factor(colData(sce)[,
        all_memory$colDataPlot[1, iSEE:::.colDataYAxis]
        ])))
    # Set zoom min/max to the first two distinct values in X/Y direction
    zoom_range <- c(
        x_range,
        sort(head(y_unique, 2))
    )
    # Extend the zoom to perfectly include the min/max boxes
    zoom_range <- zoom_range + c(0, 0, -0.5, 0.5)
    names(zoom_range) <- c("xmin","xmax","ymin","ymax")
    # Set the zoom
    all_memory$colDataPlot[[iSEE:::.zoomData]][1] <- list(zoom_range)

    p.out <- iSEE:::.make_colDataPlot(id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    params <- all_memory$colDataPlot[1, ]
    # This requires some finesse to deal with horizontal plots
    # where the X and Y coordinates are flipped to draw the violins
    expected_xy <- data.frame(
        Y=colData(sce)[, params[[iSEE:::.colDataXAxisColData]]], # Y/X switch
        X=colData(sce)[, params[[iSEE:::.colDataYAxis]]], # X/Y switch
        row.names=colnames(sce)
    )

    expect_identical(p.out$xy[, c("Y","X")], expected_xy)

})

# .make_colDataPlot/.square_plot works with zoom ----

test_that(".make_colDataPlot/.square_plot works with zoom",{

    all_memory$colDataPlot[1, iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData] <- "driver_1_s"
    all_memory$colDataPlot[1, iSEE:::.colDataYAxis] <- "passes_qc_checks_s"

    # Identify valid values
    x_unique <- unique(as.numeric(as.factor(colData(sce)[,
        all_memory$colDataPlot[1, iSEE:::.colDataXAxisColData]
        ])))
    y_unique <- unique(as.numeric(as.factor(colData(sce)[,
        all_memory$colDataPlot[1, iSEE:::.colDataYAxis]
        ])))
    # Set zoom min/max to the first two distinct values in X/Y direction
    zoom_range <- c(
        sort(head(x_unique, 2)),
        sort(head(y_unique, 2))
    )
    # Extend the zoom to perfectly include the min/max boxes
    zoom_range <- zoom_range + rep(c(-0.5, 0.5), times=2)
    names(zoom_range) <- c("xmin","xmax","ymin","ymax")
    # Set the zoom
    all_memory$colDataPlot[[iSEE:::.zoomData]][1] <- list(zoom_range)

    p.out <- iSEE:::.make_colDataPlot(
        id=1, all_memory, all_coordinates, sce, ExperimentColorMap())

    params <- all_memory$colDataPlot[1, ]
    expected_xy <- data.frame(
        Y=colData(sce)[, params[[iSEE:::.colDataYAxis]]],
        X=colData(sce)[, params[[iSEE:::.colDataXAxisColData]]],
        row.names=colnames(sce)
    )

    expect_identical(p.out$xy[, c("Y","X")], expected_xy)

})

# .define_colorby_for_column_plot ----

test_that(".define_colorby_for_column_plot handles feature selection", {
    params <- all_memory$redDimPlot[1, ]
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByFeatNameTitle
    params[[iSEE:::.colorByFeatName]] <- 1L

    color_out <- iSEE:::.define_colorby_for_column_plot(params, sce)
    expect_match(color_out$cmds, "assay(se, 2, withDimnames=FALSE)[1, ]", fixed=TRUE)

    expect_match(
        color_out$label,
        rownames(sce)[1],
        fixed=TRUE
    )

    expect_match(
        color_out$label,
        assayNames(sce)[params[[iSEE:::.colorByFeatNameAssay]]],
        fixed=TRUE
    )

    color_add <- iSEE:::.add_color_to_column_plot(assay(sce)[1, ], params)

    expect_match(
        color_add[1],
        "scale_color_gradientn",
        fixed=TRUE
    )

})

test_that(".define_colorby_for_column_plot handles sample selection", {
    params <- all_memory$redDimPlot[1, ]
    params[[iSEE:::.colorByField]] <- iSEE:::.colorBySampNameTitle
    params[1, iSEE:::.colorBySampName] <- 3L

    color_out <- iSEE:::.define_colorby_for_column_plot(params, sce)
    expect_identical(color_out, list(
        label="SRR2140055",
        cmds="plot.data$ColorBy <- logical(nrow(plot.data));\nplot.data[3L, 'ColorBy'] <- TRUE;"))

    color_add <- iSEE:::.add_color_to_column_plot(assay(sce)[1, ], params)

    expect_identical(color_add, c(
        "scale_color_manual(values=c(`FALSE`='black', `TRUE`=\"red\"), drop=FALSE) +",
        "geom_point(aes(x=X, y=Y), data=subset(plot.data, ColorBy == 'TRUE'), col=\"red\", alpha=1, size=5*1) +"))
})

# define_shapeby_for_column_plot ----

test_that("define_shapeby_for_column_plot produces the expected commands", {
    params <- all_memory$redDimPlot[1, ]
    params[[iSEE:::.shapeByField]] <- iSEE:::.shapeByColDataTitle
    params[[iSEE:::.shapeByColData]] <- "driver_1_s"

    color_out <- iSEE:::.define_shapeby_for_column_plot(params, sce)
    expect_identical(color_out, list(
        label="driver_1_s",
        cmds="plot.data$ShapeBy <- colData(se)[, \"driver_1_s\"];"))

})

# .define_shapeby_for_row_plot ----

test_that(".define_shapeby_for_row_plot produces the expected commands", {
    params <- all_memory$rowDataPlot[1, ]
    params[[iSEE:::.shapeByField]] <- iSEE:::.shapeByRowDataTitle
    params[[iSEE:::.shapeByRowData]] <- "letters"

    color_out <- iSEE:::.define_shapeby_for_row_plot(params, sce)
    expect_identical(color_out, list(
        label="letters",
        cmds="plot.data$ShapeBy <- rowData(se)[, \"letters\"];"))

})

# define_sizeby_for_column_plot ----

test_that("define_sizeby_for_column_plot produces the expected commands", {
    params <- all_memory$redDimPlot[1, ]
    params[[iSEE:::.sizeByField]] <- iSEE:::.sizeByColDataTitle
    params[[iSEE:::.sizeByColData]] <- "NREADS"

    color_out <- iSEE:::.define_sizeby_for_column_plot(params, sce)
    expect_identical(color_out, list(
        label="NREADS",
        cmds="plot.data$SizeBy <- colData(se)[, \"NREADS\"];"))

    all_memory_sb <- all_memory
    all_memory_sb$redDimPlot[1, ] <- params
    p.out <- iSEE:::.make_redDimPlot(
        id=1, all_memory_sb, all_coordinates, sce, ExperimentColorMap())
    expect_equivalent(p.out$cmd_list$plot["points.point"],
                      "geom_point(aes(x=X, y=Y, size=SizeBy), alpha=1, plot.data, color='black') +")
})

# .define_sizeby_for_row_plot ----

test_that(".define_sizeby_for_row_plot produces the expected commands", {
    params <- all_memory$rowDataPlot[1, ]
    params[[iSEE:::.sizeByField]] <- iSEE:::.sizeByRowDataTitle
    params[[iSEE:::.sizeByRowData]] <- "mean_count"

    color_out <- iSEE:::.define_sizeby_for_row_plot(params, sce)
    expect_identical(color_out, list(
        label="mean_count",
        cmds="plot.data$SizeBy <- rowData(se)[, \"mean_count\"];"))

})

# .define_colorby_for_row_plot  ----

test_that(".define_colorby_for_row_plot handles sample selection", {
    params <- all_memory$rowDataPlot[1, ]
    params[[iSEE:::.colorByField]] <- iSEE:::.colorBySampNameTitle
    params[1, iSEE:::.colorBySampName] <- 3L

    color_out <- iSEE:::.define_colorby_for_row_plot(params, sce)
    expect_identical(color_out, list(
        label="SRR2140055\n(tophat_counts)",
        cmds="plot.data$ColorBy <- assay(se, 1, withDimnames=FALSE)[, 3];"))

    color_add <- iSEE:::.add_color_to_row_plot(assay(sce)[1, ], params)

    expect_identical(
        color_add,
        "scale_color_gradientn(colors=assayColorMap(colormap, 1L, discrete=FALSE)(21), na.value='grey50', limits=range(plot.data$ColorBy, na.rm=TRUE)) +")

})

# .gene_axis_label handles NULL rownames ----

test_that(".gene_axis_label produces a valid axis label", {

    selected_gene_int <- 1L
    selected_assay <- 1L

    lab_out <- iSEE:::.feature_axis_label(
        sce, selected_gene_int, selected_assay, multiline=FALSE
    )

    expect_match(
        lab_out,
        rownames(sce)[1],
        fixed=TRUE
    )

    expect_match(
        lab_out,
        "(tophat_counts)",
        fixed=TRUE
    )

    # Handling unnamed assays.
    assayNames(sce)[] <-""

    lab_out <- iSEE:::.feature_axis_label(
        sce, selected_gene_int, selected_assay, multiline=FALSE
    )

    expect_match(
        lab_out,
        "(assay 1)",
        fixed=TRUE
    )
})

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
        p.out$cmd_list$select,
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
        p.out$cmd_list$select,
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
        p.out$cmd_list$select,
        c("brushNA", "select", "saved", "subset")
    )
    expect_match(
        p.out$cmd_list$plot["points.select_restrict"],
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

    expect_true(any(grepl("facet_grid(FacetRow ~ .)", out$cmd_list$plot, fixed=TRUE)))
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
