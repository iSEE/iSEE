
# Set up plotting parameters
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 1)
featAssayArgs <- featAssayPlotDefaults(sce, 1)
rowDataArgs <- rowDataPlotDefaults(sce, 1)

# Set up memory
sce <- iSEE:::.precompute_UI_info(sce, NULL, NULL)
all_memory <- iSEE:::.setup_memory(sce,
    redDimArgs=redDimArgs,
    colDataArgs=colDataArgs,
    featAssayArgs=featAssayArgs,
    rowStatArgs=NULL,
    rowDataArgs=rowDataArgs,
    sampAssayArgs=NULL,
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
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

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

    all_memory$redDimPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
    p.out <- iSEE:::.make_redDimPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c("X","Y","ColorBy")
    )

})

# .make_colDataPlot/.scatter_plot ----

test_that(".make_colDataPlot/.scatter_plot produce a valid list",{

    all_memory$colDataPlot[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1,iSEE:::.colDataYAxis] <- "NALIGNED"

    p.out <- iSEE:::.make_colDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

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

    all_memory$colDataPlot[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1,iSEE:::.colDataYAxis] <- "NALIGNED"

    all_memory$colDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
    p.out <- iSEE:::.make_colDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c('Y', 'X', 'ColorBy')
    )

})

# .make_colDataPlot/.violin_plot ----

test_that(".make_colDataPlot/.violin_plot produce a valid list",{

    p.out <- iSEE:::.make_colDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

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
        c("data","select","setup","plot")
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("Y","X","GroupBy","jitteredX")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_colDataPlot/.violin_plot produce a valid xy with color", {

    all_memory$colDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
    p.out <- iSEE:::.make_colDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c("Y","X","ColorBy","GroupBy","jitteredX")
    )

})

# .make_colDataPlot/.square_plot ----

test_that(".make_colDataPlot/.square_plot produce a valid list",{

    all_memory$colDataPlot[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData] <- "driver_1_s"
    all_memory$colDataPlot[1,iSEE:::.colDataYAxis] <- "passes_qc_checks_s"

    p.out <- iSEE:::.make_colDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

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
        c("data","select","setup","plot")
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

    all_memory$colDataPlot[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData] <- "driver_1_s"
    all_memory$colDataPlot[1,iSEE:::.colDataYAxis] <- "passes_qc_checks_s"
    all_memory$colDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle

    p.out <- iSEE:::.make_colDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c("Y","X","ColorBy","jitteredX","jitteredY")
    )

})

# .make_rowDataPlot/.scatter_plot ----

test_that(".make_rowDataPlot/.scatter_plot produce a valid list",{

    all_memory$rowDataPlot[1,iSEE:::.rowDataXAxis] <- iSEE:::.rowDataXAxisRowData
    all_memory$rowDataPlot[1,iSEE:::.rowDataXAxisRowData] <- "num_cells"
    all_memory$rowDataPlot[1,iSEE:::.rowDataYAxis] <- "mean_count"

    p.out <- iSEE:::.make_rowDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

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
        c("data","select","setup","plot")
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

    all_memory$rowDataPlot[1,iSEE:::.rowDataXAxis] <- iSEE:::.rowDataXAxisRowData
    all_memory$rowDataPlot[1,iSEE:::.rowDataXAxisRowData] <- "num_cells"
    all_memory$rowDataPlot[1,iSEE:::.rowDataYAxis] <- "mean_count"

    all_memory$rowDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByRowDataTitle
    p.out <- iSEE:::.make_rowDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c('Y', 'X', 'ColorBy')
    )

    # Color by feature name
    all_memory$rowDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByFeatNameTitle

    p.out <- iSEE:::.make_rowDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

})

# .make_rowDataPlot/.violin_plot ----

test_that(".make_rowDataPlot/.violin_plot produce a valid list",{

    p.out <- iSEE:::.make_rowDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

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
        c("data","select","setup","plot")
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("Y","X","GroupBy","jitteredX")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_rowDataPlot/.violin_plot produce a valid xy with color", {

    all_memory$rowDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByRowDataTitle
    p.out <- iSEE:::.make_rowDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c("Y","X","ColorBy","GroupBy","jitteredX")
    )

    # Color by feature name
    all_memory$rowDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByFeatNameTitle

    p.out <- iSEE:::.make_rowDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

})

# .make_rowDataPlot/.square_plot ----

test_that(".make_rowDataPlot/.square_plot produce a valid list",{

    rowData(sce)[,"letters"] <- sample(letters[1:5], nrow(sce), replace = TRUE)
    rowData(sce)[,"LETTERS"] <- sample(LETTERS[1:3], nrow(sce), replace = TRUE)

    all_memory$rowDataPlot[1,iSEE:::.rowDataXAxis] <- iSEE:::.rowDataXAxisRowData
    all_memory$rowDataPlot[1,iSEE:::.rowDataXAxisRowData] <- "letters"
    all_memory$rowDataPlot[1,iSEE:::.rowDataYAxis] <- "LETTERS"

    p.out <- iSEE:::.make_rowDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

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
        c("data","select","setup","plot")
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

    rowData(sce)[,"letters"] <- sample(letters[1:5], nrow(sce), replace = TRUE)
    rowData(sce)[,"LETTERS"] <- sample(LETTERS[1:3], nrow(sce), replace = TRUE)

    all_memory$rowDataPlot[1,iSEE:::.rowDataXAxis] <- iSEE:::.rowDataXAxisRowData
    all_memory$rowDataPlot[1,iSEE:::.rowDataXAxisRowData] <- "letters"
    all_memory$rowDataPlot[1,iSEE:::.rowDataYAxis] <- "LETTERS"

    all_memory$rowDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByRowDataTitle

    p.out <- iSEE:::.make_rowDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

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
        c("data","select","setup","plot")
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
    all_memory$rowDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByFeatNameTitle

    p.out <- iSEE:::.make_rowDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

})

# .make_featAssayPlot/.scatter_plot ----

test_that(".make_featAssayPlot/.scatter_plot produce a valid list",{

    all_memory$featAssayPlot[1,iSEE:::.featAssayYAxisRowTable] <- "Row statistics table 1"
    all_memory$featAssayPlot[1,iSEE:::.featAssayYAxisRowTable] <- "Row statistics table 1"

    p.out <- iSEE:::.make_featAssayPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

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
        c("data","select","setup","plot")
    )

    # xy value is a data frame
    expect_s3_class(
        p.out$xy,
        "data.frame"
    )
    expect_named(
        p.out$xy,
        c("Y","X","GroupBy","jitteredX")
    )

    #plot
    expect_s3_class(
        p.out$plot,
        c("gg", "ggplot")
    )

})

test_that(".make_featAssayPlot/.scatter_plot produce a valid xy with color", {

    all_memory$featAssayPlot[1,iSEE:::.featAssayYAxisRowTable] <- "Row statistics table 1"
    all_memory$featAssayPlot[1,iSEE:::.featAssayYAxisRowTable] <- "Row statistics table 1"
    all_memory$featAssayPlot[1,iSEE:::.colorByRowTable] <- "Row statistics table 1"
    all_memory$featAssayPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByFeatNameTitle

    p.out <- iSEE:::.make_featAssayPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
    expect_named(
        p.out$xy,
        c("Y","X","ColorBy","GroupBy","jitteredX")
    )

})

test_that(".make_featAssayPlot fails for YAxisFeatName set to a character value", {
    # change the value locally for the specific test
    selected_gene <- "0610009B22Rik"

    all_memory$featAssayPlot[1, iSEE:::.featAssayYAxisFeatName] <- selected_gene

    expect_error(
        iSEE:::.make_featAssayPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap()),
        "invalid format '%i'; use format %s for character objects",
        fixed=TRUE)

})

test_that(".make_featAssayPlot works for XAxis set to Column data", {
    # change the value locally for the specific test
    all_memory$featAssayPlot[1,iSEE:::.featAssayXAxis] <- iSEE:::.featAssayXAxisColDataTitle
    all_memory$featAssayPlot[1,iSEE:::.featAssayXAxisColData] <- "dissection_s"

    p.out <- iSEE:::.make_featAssayPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_match(
        p.out$cmd_list$data['x'],
        "dissection_s",
        fixed = TRUE
    )

})

test_that(".make_featAssayPlot fails for XAxis set to a character feature name", {
    selected_gene <- "0610009B22Rik"

    # change the value locally for the specific test
    all_memory$featAssayPlot[1,iSEE:::.featAssayXAxis] <- iSEE:::.featAssayXAxisFeatNameTitle
    all_memory$featAssayPlot[1,iSEE:::.featAssayXAxisFeatName] <- selected_gene

    expect_error(
        iSEE:::.make_featAssayPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap()),
        "invalid format '%i'; use format %s for character objects",
        fixed=TRUE)

})

test_that(".make_featAssayPlot works for groupable colour covariate", {
    selected_coldata <- "dissection_s"

    # change the value locally for the specific test
    all_memory$featAssayPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
    all_memory$featAssayPlot[1,iSEE:::.colorByColData] <- selected_coldata

    p.out <- iSEE:::.make_featAssayPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_match(
        p.out$cmd_list$data['color'],
        selected_coldata,
        fixed = TRUE
    )

    expect_identical(p.out$cmd_list$data[['more_color']], "plot.data$ColorBy <- factor(plot.data$ColorBy);")

    expect_match(
        p.out$cmd_list$plot["scale_color1"],
        "^scale_color_manual"
    )
    expect_match(
        p.out$cmd_list$plot["scale_color1"],
        selected_coldata,
        fixed = TRUE
    )

    expect_match(
        p.out$cmd_list$plot["scale_color2"],
        "^scale_fill_manual"
    )
    expect_match(
        p.out$cmd_list$plot["scale_color2"],
        selected_coldata,
        fixed = TRUE
    )

})

# .make_colDataPlot/.create_plot horizontal violin plots ----

test_that(".make_colDataPlot/.create_plot can produce horizontal violins", {
    selected_coldataX <- "NREADS"
    selected_coldataY <- "driver_1_s"

    # change the value locally for the specific test
    all_memory$colDataPlot[1,iSEE:::.colDataXAxis] <- iSEE:::.colorByColDataTitle
    all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData] <- selected_coldataX

    all_memory$colDataPlot[1,iSEE:::.colDataYAxis] <- selected_coldataY

    all_memory$colDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colDataXAxisNothingTitle

    p.out <- iSEE:::.make_colDataPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_match(
        p.out$cmd_list$data['y'],
        selected_coldataY,
        fixed = TRUE
    )

    expect_match(
        p.out$cmd_list$data['x'],
        selected_coldataX,
        fixed = TRUE
    )

    expect_named(
        p.out$xy,
        c("Y","X","GroupBy","jitteredX")
    )

})

# .scatter_plot plot with zoom ----

test_that(".scatter_plot works with zoom",{

    # Identify range of data
    params <- all_memory$redDimPlot[1,]
    x_range <- range(head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]]
    ), 10)
    y_range <- range(head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]]
    ), 10)
    # Set zoom min/max to the first two distinct values in X/Y direction
    zoom_range <- c(x_range, y_range)
    names(zoom_range) <- c("xmin","xmax","ymin","ymax")
    # Set the zoom
    all_memory$redDimPlot[[iSEE:::.zoomData]][1] <- list(zoom_range)

    p.out <- iSEE:::.make_redDimPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

    params <- all_memory$redDimPlot[1,]
    expected_xy <- data.frame(
        X = reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
        Y = reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
        row.names = colnames(sce)
    )

    expect_identical(p.out$xy, expected_xy)

})

# .make_colDataPlot/.violin_plot works with zoom ----

test_that(".make_colDataPlot/.violin_plot works with zoom",{

    all_memory$colDataPlot[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData] <- "driver_1_s"

    # Identify valid values
    x_unique <- unique(as.numeric(as.factor(colData(sce)[,
        all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData]
        ])))
    y_range <- range(head(colData(sce)[,
        all_memory$colDataPlot[1,iSEE:::.colDataYAxis]
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

    p.out <- iSEE:::.make_colDataPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

    params <- all_memory$colDataPlot[1,]
    expected_xy <- data.frame(
        Y = colData(sce)[,params[[iSEE:::.colDataYAxis]]],
        X = colData(sce)[,params[[iSEE:::.colDataXAxisColData]]],
        row.names = colnames(sce)
    )

    expect_identical(p.out$xy[,c("Y", "X")], expected_xy)

})

# .make_colDataPlot/.violin_plot works with horizontal zoom ----

test_that(".make_colDataPlot/.violin_plot works with zoom",{

    all_memory$colDataPlot[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData] <- "NREADS"
    all_memory$colDataPlot[1,iSEE:::.colDataYAxis] <- "driver_1_s"

    # Identify valid values
    x_range <- range(head(colData(sce)[,
        all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData]
        ]), 10)
    y_unique <- unique(as.numeric(as.factor(colData(sce)[,
        all_memory$colDataPlot[1,iSEE:::.colDataYAxis]
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

    p.out <- iSEE:::.make_colDataPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

    params <- all_memory$colDataPlot[1,]
    # This requires some finesse to deal with horizontal plots
    # where the X and Y coordinates are flipped to draw the violins
    expected_xy <- data.frame(
        Y = colData(sce)[,params[[iSEE:::.colDataXAxisColData]]], # Y/X switch
        X = colData(sce)[,params[[iSEE:::.colDataYAxis]]], # X/Y switch
        row.names = colnames(sce)
    )

    expect_identical(p.out$xy[, c("Y","X")], expected_xy)

})

# .make_colDataPlot/.square_plot works with zoom ----

test_that(".make_colDataPlot/.square_plot works with zoom",{

    all_memory$colDataPlot[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
    all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData] <- "driver_1_s"
    all_memory$colDataPlot[1,iSEE:::.colDataYAxis] <- "passes_qc_checks_s"

    # Identify valid values
    x_unique <- unique(as.numeric(as.factor(colData(sce)[,
        all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData]
        ])))
    y_unique <- unique(as.numeric(as.factor(colData(sce)[,
        all_memory$colDataPlot[1,iSEE:::.colDataYAxis]
        ])))
    # Set zoom min/max to the first two distinct values in X/Y direction
    zoom_range <- c(
        sort(head(x_unique, 2)),
        sort(head(y_unique, 2))
    )
    # Extend the zoom to perfectly include the min/max boxes
    zoom_range <- zoom_range + rep(c(-0.5, 0.5), times = 2)
    names(zoom_range) <- c("xmin","xmax","ymin","ymax")
    # Set the zoom
    all_memory$colDataPlot[[iSEE:::.zoomData]][1] <- list(zoom_range)

    p.out <- iSEE:::.make_colDataPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

    params <- all_memory$colDataPlot[1,]
    expected_xy <- data.frame(
        Y = colData(sce)[,params[[iSEE:::.colDataYAxis]]],
        X = colData(sce)[,params[[iSEE:::.colDataXAxisColData]]],
        row.names = colnames(sce)
    )

    expect_identical(p.out$xy[, c("Y","X")], expected_xy)

})

# .process_colorby_choice handles gene text input ----

test_that(".process_colorby_choice_for_column_plots handles gene text input", {
    params <- all_memory$redDimPlot[1,]
    params[[iSEE:::.colorByField]] <- iSEE:::.colorByFeatNameTitle
    params[[iSEE:::.colorByFeatName]] <- 1L

    color_out <- iSEE:::.define_colorby_for_column_plot(params, sce)
    expect_match(color_out$cmds, "assay(se, 6, withDimnames=FALSE)[1,]", fixed=TRUE)

    expect_match(
        color_out$label,
        rownames(sce)[1],
        fixed = TRUE
    )

    expect_match(
        color_out$label,
        assayNames(sce)[params[[iSEE:::.colorByFeatNameAssay]]],
        fixed = TRUE
    )

    color_add <- iSEE:::.add_color_to_column_plot(assay(sce)[1,], params)

    expect_match(
        color_add[1],
        "scale_color_gradientn",
        fixed = TRUE
    )

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
        fixed = TRUE
    )

    # Handling unnamed assays.
    assayNames(sce)[] <-""

    lab_out <- iSEE:::.feature_axis_label(
        sce, selected_gene_int, selected_assay, multiline=FALSE
    )

    expect_match(
        lab_out,
        "(assay 1)",
        fixed = TRUE
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
    all_memory$featAssayPlot[1,iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1,]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
        10)
    all_memory$redDimPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction = "xy", mapping = list(x="X", y="Y"),
        brushId = "dummy_brush", outputId = "dummy_plot"
    ))

    select_cmd <- iSEE:::.process_selectby_choice(all_memory$featAssayPlot, all_memory)

    # check the source of the selected data
    expect_match(
        select_cmd$cmds[1],
        "shiny::brushedPoints(all_coordinates",
        fixed = TRUE
    )

    # check the source plot type
    expect_match(
        select_cmd$cmds[1],
        sourcePlotType,
        fixed = TRUE
    )

    # check that the second (hard-coded) command is present
    expect_match(
        select_cmd$cmds[2],
        "plot.data$SelectBy",
        fixed = TRUE
    )

})

test_that(".process_selectby_choice works when sender is self plot", {

    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$redDimPlot[1,iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1,]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
        10)
    all_memory$redDimPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction = "xy", mapping = list(x="X", y="Y"),
        brushId = "dummy_brush", outputId = "dummy_plot"
    ))

    select_cmd <- iSEE:::.process_selectby_choice(all_memory$redDimPlot, all_memory)

    # check the source of the selected data
    expect_match(
        select_cmd$cmds[1],
        "shiny::brushedPoints(plot.data",
        fixed = TRUE
    )
    # check that the second (hard-coded) command is present
    expect_match(
        select_cmd$cmds[2],
        "plot.data$SelectBy",
        fixed = TRUE
    )

})

test_that(".process_selectby_choice works with closed lasso selection", {

    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$featAssayPlot[1,iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1,]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
        10)

    new_lasso <- list(lasso=NULL, closed=TRUE, panelvar1=NULL,
        panelvar2=NULL, mapping=list(x="X", y="Y"))
    new_lasso$coord <- matrix(
        data = c(
            min(x_10), min(y_10),
            min(x_10), max(y_10),
            max(x_10), max(y_10),
            max(x_10), min(y_10)
        ),
        ncol = 2,
        byrow = TRUE
    )

    all_memory$redDimPlot[[iSEE:::.lassoData]][1] <- list(new_lasso)

    select_cmd <- iSEE:::.process_selectby_choice(all_memory$featAssayPlot, all_memory)

    # check the source plot type
    expect_match(
        select_cmd$cmds[1],
        sourcePlotType,
        fixed = TRUE
    )

    # check that the second (hard-coded) command is present
    expect_match(
        select_cmd$cmds[1],
        "all_lassos",
        fixed = TRUE
    )

})

# .create_points handles transparency selection effect ----

test_that(".create_points handles transparency selection effect", {

    # Implement a self-selection, for convenience
    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$redDimPlot[1,iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1,]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
        10)
    all_memory$redDimPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction = "xy", mapping = list(x="X", y="Y"),
        brushId = "dummy_brush", outputId = "dummy_plot"
    ))
    # Set up the selection effect type
    all_memory$redDimPlot[[iSEE:::.selectEffect]][1] <- iSEE:::.selectTransTitle

    p.out <- iSEE:::.make_redDimPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_named(
        p.out$cmd_list$select,
        c("brush", "select")
    )
    # TODO: better tests

})

# .create_points handles coloured selection effect ----

test_that(".create_points handles coloured selection effect", {

    # Implement a self-selection, for convenience
    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$redDimPlot[1,iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1,]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
        10)
    all_memory$redDimPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction = "xy", mapping = list(x="X", y="Y"),
        brushId = "dummy_brush", outputId = "dummy_plot"
    ))
    # Set up the selection effect type
    all_memory$redDimPlot[1,iSEE:::.selectEffect] <- iSEE:::.selectColorTitle

    p.out <- iSEE:::.make_redDimPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_named(
        p.out$cmd_list$select,
        c("brush", "select")
    )
    expect_match(
        p.out$cmd$plot["points.select_color"],
        all_memory$redDimPlot[1,iSEE:::.selectColor],
        fixed = TRUE
    )
    # TODO: better tests

})

# .create_points handles restrict selection effect ----

test_that(".create_points handles restrict selection effect", {

    # Implement a self-selection, for convenience
    sourcePlotName <- "Reduced dimension plot 1"
    sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type

    # Set up the point selection link: redDim1 --> featAssay1
    all_memory$redDimPlot[1,iSEE:::.selectByPlot] <- sourcePlotName
    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1,]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
        10)
    all_memory$redDimPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
        direction = "xy", mapping = list(x="X", y="Y"),
        brushId = "dummy_brush", outputId = "dummy_plot"
    ))
    # Set up the selection effect type
    all_memory$redDimPlot[1,iSEE:::.selectEffect] <- iSEE:::.selectRestrictTitle

    p.out <- iSEE:::.make_redDimPlot(
        id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())

    expect_named(
        p.out$cmd_list$select,
        c("brush","select","saved", "subset")
    )
    expect_match(
        p.out$cmd_list$plot["points.select_restrict"],
        "plot.data",
        fixed = TRUE
    )
    # TODO: better tests

})

# .self_lasso_path work with single point, open, and closed paths ----

test_that(".self_lasso_path work with a single point", {

    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1,]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
        10)

    new_lasso <- list(lasso=NULL, closed=FALSE, panelvar1=NULL,
        panelvar2=NULL, mapping=list(x="X", y="Y"))
    new_lasso$coord <- matrix(
        data = c(
            min(x_10), min(y_10)
        ),
        ncol = 2,
        byrow = TRUE
    )

    all_memory$redDimPlot[[iSEE:::.lassoData]][1] <- list(new_lasso)

    lasso_cmd <- iSEE:::.self_lasso_path(all_memory$redDimPlot, flip=FALSE)

    expect_match(
        lasso_cmd$cmds,
        "geom_point",
        fixed = TRUE
    )

    expect_identical(
        lasso_cmd$data[[1]],
        new_lasso
    )

    expect_identical(
        nrow(lasso_cmd$data[[1]]$coord),
        1L
    )

})

test_that(".self_lasso_path work with an open path", {

    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1,]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
        10)

    new_lasso <- list(lasso=NULL, closed=FALSE, panelvar1=NULL,
        panelvar2=NULL, mapping=list(x="X", y="Y"))
    new_lasso$coord <- matrix(
        data = c(
            min(x_10), min(y_10),
            max(x_10), min(y_10),
            max(x_10), max(y_10)
        ),
        ncol = 2,
        byrow = TRUE
    )

    all_memory$redDimPlot[[iSEE:::.lassoData]][1] <- list(new_lasso)

    lasso_cmd <- iSEE:::.self_lasso_path(all_memory$redDimPlot, flip=FALSE)

    expect_match(
        lasso_cmd$cmds[1],
        "geom_path",
        fixed = TRUE
    )
    expect_match(
        lasso_cmd$cmds[2],
        "geom_point",
        fixed = TRUE
    )
    expect_identical(
        lasso_cmd$cmds[3],
        "scale_shape_manual(values = c('TRUE' = 22, 'FALSE' = 20))"
    )
    expect_identical(
        lasso_cmd$cmds[4],
        "guides(shape = 'none')"
    )

    expect_identical(
        lasso_cmd$data[[1]],
        new_lasso
    )

})

test_that(".self_lasso_path work with a closed and flipped path", {

    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1,]
    x_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
        10)
    y_10 <- head(
        reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
        10)

    new_lasso <- list(lasso=NULL, closed=TRUE, panelvar1=NULL,
        panelvar2=NULL, mapping=list(x="X", y="Y"))
    new_lasso$coord <- matrix(
        data = c(
            min(x_10), min(y_10),
            max(x_10), min(y_10),
            max(x_10), max(y_10),
            min(x_10), max(y_10),
            min(x_10), min(y_10)
        ),
        ncol = 2,
        byrow = TRUE
    )

    all_memory$redDimPlot[[iSEE:::.lassoData]][1] <- list(new_lasso)

    lasso_cmd <- iSEE:::.self_lasso_path(all_memory$redDimPlot, flip=FALSE)

    expect_match(
        lasso_cmd$cmds[1],
        "geom_polygon",
        fixed = TRUE
    )
    expect_match(
        lasso_cmd$cmds[2],
        "scale_fill_manual",
        fixed = TRUE
    )
    expect_identical(
        lasso_cmd$cmds[3],
        "guides(shape = 'none')"
    )

    expect_identical(
        lasso_cmd$data[[1]],
        new_lasso
    )

})

# .define_facetby_for_column_plot ----

test_that(".define_facetby_for_column_plot works", {

    params <- all_memory$redDimPlot[1,]

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
    expect_match(out["FacetRow"], "driver_1_s", fixed = TRUE)
    expect_match(out["FacetColumn"], "Core.Type", fixed = TRUE)
})

# .define_facetby_for_row_plot ----

test_that(".define_facetby_for_row_plot works", {

    # Set up the selected data
    params <- all_memory$rowDataPlot[1,]

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
    expect_match(out["FacetRow"], "mean_count", fixed = TRUE)
    expect_match(out["FacetColumn"], "num_cells", fixed = TRUE)
})


# .add_facets works for column- and row-based plots ----

test_that(".add_facets works for column data plots", {

    # Set up the selected data (in redDim1)
    params <- all_memory$redDimPlot[1,]

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
    params <- all_memory$rowDataPlot[1,]

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

