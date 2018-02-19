
# Set up plotting parameters
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 1)
featExprArgs <- featExprPlotDefaults(sce, 1)
rowStatArgs <- rowStatTableDefaults(sce, 1)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 1)

# Set up memory
all_memory <- iSEE:::.setup_memory(
  sce, redDimArgs, colDataArgs, featExprArgs, rowStatArgs, rowDataArgs, heatMapArgs,
  redDimMax = 1, colDataMax = 1, featExprMax = 1, rowStatMax = 1, rowDataMax = 1, heatMapMax = 1)

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
    c("cmd", "xy", "plot")
  )
  
  # cmd value is a named list
  expect_type(
    p.out$cmd,
    "list"
  )
  expect_named(
    p.out$cmd,
    c("data","lim","brush","setup","plot")
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
    c("cmd", "xy", "plot")
  )
  
  # cmd value is a named list
  expect_type(
    p.out$cmd,
    "list"
  )
  expect_named(
    p.out$cmd,
    c("data","lim","brush","setup","plot")
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

test_that(".make_colDataPlot/.violin_plot produce a valid xy with color", {
 
  all_memory$colDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
  p.out <- iSEE:::.make_colDataPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  expect_named(
    p.out$xy,
    c("Y","X","ColorBy")
  )
  
})

# .make_colDataPlot/.griddotplot ----

test_that(".make_colDataPlot/.griddotplot produce a valid list",{
  
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
    c("cmd", "xy", "plot")
  )
  
  # cmd value is a named list
  expect_type(
    p.out$cmd,
    "list"
  )
  expect_named(
    p.out$cmd,
    c("data","lim","brush","setup","plot")
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

test_that(".make_colDataPlot/.griddotplot produce a valid xy with color", {
 
  all_memory$colDataPlot[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
  all_memory$colDataPlot[1,iSEE:::.colDataXAxisColData] <- "driver_1_s"
  all_memory$colDataPlot[1,iSEE:::.colDataYAxis] <- "passes_qc_checks_s"
  all_memory$colDataPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
  
  p.out <- iSEE:::.make_colDataPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  expect_named(
    p.out$xy,
    c("Y","X","ColorBy")
  )
  
})

# .make_featExprPlot/.scatter_plot ----

test_that(".make_featExprPlot/.scatter_plot produce a valid list",{
  
  all_memory$featExprPlot[1,iSEE:::.featExprYAxisRowTable] <- "Row statistics table 1"
  all_memory$featExprPlot[1,iSEE:::.featExprYAxisRowTable] <- "Row statistics table 1"
  
  p.out <- iSEE:::.make_featExprPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  # return value is a named list
  expect_type(
    p.out,
    "list"
  )
  expect_named(
    p.out,
    c("cmd", "xy", "plot")
  )
  
  # cmd value is a named list
  expect_type(
    p.out$cmd,
    "list"
  )
  expect_named(
    p.out$cmd,
    c("data","lim","brush","setup","plot")
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

test_that(".make_featExprPlot/.scatter_plot produce a valid xy with color", {
 
  all_memory$featExprPlot[1,iSEE:::.featExprYAxisRowTable] <- "Row statistics table 1"
  all_memory$featExprPlot[1,iSEE:::.featExprYAxisRowTable] <- "Row statistics table 1"
  all_memory$featExprPlot[1,iSEE:::.colorByRowTable] <- "Row statistics table 1"
  all_memory$featExprPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByRowTableTitle
  
  p.out <- iSEE:::.make_featExprPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  expect_named(
    p.out$xy,
    c("Y","X","ColorBy")
  )
  
})

test_that(".make_featExprPlot works for YAxis set to Feature name", {
  # change the value locally for the specific test
  selected_gene <- "0610009B22Rik"
  
  all_memory$featExprPlot[1,iSEE:::.featExprYAxis] <- iSEE:::.featExprYAxisFeatNameTitle
  all_memory$featExprPlot[1,iSEE:::.featExprYAxisFeatName] <- selected_gene
  
  p.out <- iSEE:::.make_featExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_match(
    p.out$cmd$data$y,
    selected_gene,
    fixed = TRUE
  )
  
})

test_that(".make_featExprPlot works for XAxis set to Column data", {
  # change the value locally for the specific test
  all_memory$featExprPlot[1,iSEE:::.featExprXAxis] <- iSEE:::.featExprXAxisColDataTitle
  all_memory$featExprPlot[1,iSEE:::.featExprXAxisColData] <- "dissection_s"
  
  p.out <- iSEE:::.make_featExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_match(
    p.out$cmd$data$x,
    "dissection_s",
    fixed = TRUE
  )
  
})

test_that(".make_featExprPlot works for XAxis set to Row table", {
  # change the value locally for the specific test
  all_memory$featExprPlot[1,iSEE:::.featExprXAxis] <- iSEE:::.featExprXAxisRowTableTitle
  all_memory$featExprPlot[1,iSEE:::.featExprXAxisRowTable] <- "Row statistics table 1"
  
  p.out <- iSEE:::.make_featExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_match(
    p.out$cmd$data$x,
    "plot.data$X <- assay",
    fixed = TRUE
  )
  
})

test_that(".make_featExprPlot works for XAxis set to Feature name", {
  selected_gene <- "0610009B22Rik"
  
  # change the value locally for the specific test
  all_memory$featExprPlot[1,iSEE:::.featExprXAxis] <- iSEE:::.featExprXAxisFeatNameTitle
  all_memory$featExprPlot[1,iSEE:::.featExprXAxisFeatName] <- selected_gene
  
  p.out <- iSEE:::.make_featExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_match(
    p.out$cmd$data$x,
    selected_gene,
    fixed = TRUE
  )
  
})

test_that(".make_featExprPlot works for groupable colour covariate", {
  selected_coldata <- "dissection_s"
  
  # change the value locally for the specific test
  all_memory$featExprPlot[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
  all_memory$featExprPlot[1,iSEE:::.colorByColData] <- selected_coldata
  
  p.out <- iSEE:::.make_featExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_match(
    p.out$cmd$data$color,
    selected_coldata,
    fixed = TRUE
  )
  
  expect_match(
    p.out$cmd$data$more_color,
    "as.factor",
    fixed = TRUE
  )
  
  expect_match(
    p.out$cmd$plot$scale_color[[1]],
    "^scale_color_manual"
  )
  expect_match(
    p.out$cmd$plot$scale_color[[1]],
    selected_coldata,
    fixed = TRUE
  )
  
  expect_match(
    p.out$cmd$plot$scale_color[[2]],
    "^scale_fill_manual"
  )
  expect_match(
    p.out$cmd$plot$scale_color[[2]],
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
    p.out$cmd$data$y,
    selected_coldataY,
    fixed = TRUE
  )
  
  expect_match(
    p.out$cmd$data$x,
    selected_coldataX,
    fixed = TRUE
  )
  
  expect_named(
    p.out$xy,
    c("Y","X")
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
  
  expect_identical(p.out$xy, expected_xy)

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
  
  expect_identical(p.out$xy, expected_xy)

})

# .make_colDataPlot/.griddotplot works with zoom ----

test_that(".make_colDataPlot/.griddotplot works with zoom",{
  
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
  
  expect_identical(p.out$xy, expected_xy)

})

# .process_colorby_choice handles gene text input ----

test_that(".process_colorby_choice_for_column_plots handles gene text input", {
  params <- all_memory$redDimPlot[1,]
  params[[iSEE:::.colorByField]] <- iSEE:::.colorByFeatNameTitle
  params[[iSEE:::.colorByFeatName]] <- rownames(sce)[1]
  
  color_out <- iSEE:::.process_colorby_choice_for_column_plots(
    params, all_memory, sce, ExperimentColorMap())
  
  expect_named(
    color_out,
    c("cmd","label","FUN")
  )
   
  expect_match(
    color_out$cmd,
    rownames(sce)[1],
    fixed = TRUE
  )
  
  expect_match(
    color_out$label,
    rownames(sce)[1],
    fixed = TRUE
  )
  
  expect_type(
     color_out$FUN,
    "closure"
  )
  
})


# .gene_axis_label handles NULL rownames ----

test_that(".gene_axis_label handles NULL rownames", {

  selected_gene_int <- 1L
  selected_assay <- 1L

  nrows <- 20; ncols <- 6
  counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
  se_nullnames <- SummarizedExperiment(assays=SimpleList(counts))

  lab_out <- iSEE:::.gene_axis_label(
    se_nullnames, selected_gene_int, selected_assay, multiline=FALSE
  )

  expect_match(
    lab_out,
    "^Feature"
  )

  expect_match(
    lab_out,
    "(assay 1)",
    fixed = TRUE
  )

})

# .coerce_to_numeric handles gene text input ----

test_that(".coerce_to_numeric handles gene text input", {

  input_values <- letters
  input_field <- "field_name"

  expect_warning(
    lab_out <- iSEE:::.coerce_to_numeric(input_values, input_field, warn=TRUE),
    "coloring covariate has too many unique values, coercing to numeric"
  )

  lab_out <- iSEE:::.coerce_to_numeric(input_values, input_field, warn=TRUE)

  expect_type(
    lab_out,
    "character"
  )

})

# .process_brushby_choice ----

test_that(".process_brushby_choice works when sender is another plot", {
  
  sourcePlotName <- "Reduced dimension plot 1"
  sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type
  
  # Set up the brush link: redDim1 --> featExpr1
  all_memory$featExprPlot[1,iSEE:::.brushByPlot] <- sourcePlotName
  # Set up the brush data (in redDim1)
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
  
  brush_cmd <- iSEE:::.process_brushby_choice(all_memory$featExprPlot, all_memory)
  
  # check the source of the brushed data
  expect_match(
    brush_cmd$cmd,
    "shiny::brushedPoints(all_coordinates",
    fixed = TRUE
  )
  # check the source plot type
  expect_match(
    brush_cmd$cmd,
    sourcePlotType,
    fixed = TRUE
  )
  # check that the second (hard-coded) command is present
  expect_match(
    brush_cmd$cmd,
    "plot.data$BrushBy",
    fixed = TRUE
  )
  
})

test_that(".process_brushby_choice works when sender is self plot", {
  
  sourcePlotName <- "Reduced dimension plot 1"
  sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type
  
  # Set up the brush link: redDim1 --> featExpr1
  all_memory$redDimPlot[1,iSEE:::.brushByPlot] <- sourcePlotName
  # Set up the brush data (in redDim1)
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
  
  brush_cmd <- iSEE:::.process_brushby_choice(all_memory$redDimPlot, all_memory)
  
  # check the source of the brushed data
  expect_match(
    brush_cmd$cmd,
    "shiny::brushedPoints(plot.data",
    fixed = TRUE
  )
  # check that the second (hard-coded) command is present
  expect_match(
    brush_cmd$cmd,
    "plot.data$BrushBy",
    fixed = TRUE
  )
  
})

# .create_points handles transparency brush ----

test_that(".create_points handles transparency brush", {
  
  # Implement a self-brush, for convenience
  sourcePlotName <- "Reduced dimension plot 1"
  sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type
  
  # Set up the brush link: redDim1 --> featExpr1
  all_memory$redDimPlot[1,iSEE:::.brushByPlot] <- sourcePlotName
  # Set up the brush data (in redDim1)
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
  all_memory$redDimPlot[[iSEE:::.brushEffect]][1] <- iSEE:::.brushTransTitle
  
  p.out <- iSEE:::.make_redDimPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_named(
    p.out$cmd$brush,
    "init"
  )
  # TODO: better tests
  
})

# .create_points handles colour brush ----

test_that(".create_points handles colour brush", {
  
  # Implement a self-brush, for convenience
  sourcePlotName <- "Reduced dimension plot 1"
  sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type
  
  # Set up the brush link: redDim1 --> featExpr1
  all_memory$redDimPlot[1,iSEE:::.brushByPlot] <- sourcePlotName
  # Set up the brush data (in redDim1)
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
  # Set up the brush type
  all_memory$redDimPlot[1,iSEE:::.brushEffect] <- iSEE:::.brushColorTitle
  
  p.out <- iSEE:::.make_redDimPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_named(
    p.out$cmd$brush,
    "init"
  )
  expect_match(
    p.out$cmd$plot$brush_color,
    all_memory$redDimPlot[1,iSEE:::.brushColor],
    fixed = TRUE
  )
  # TODO: better tests
  
})

# .create_points handles restrict brush ----

test_that(".create_points handles restrict brush", {
  
  # Implement a self-brush, for convenience
  sourcePlotName <- "Reduced dimension plot 1"
  sourcePlotType <- iSEE:::.encode_panel_name(sourcePlotName)$Type
  
  # Set up the brush link: redDim1 --> featExpr1
  all_memory$redDimPlot[1,iSEE:::.brushByPlot] <- sourcePlotName
  # Set up the brush data (in redDim1)
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
  # Set up the brush type
  all_memory$redDimPlot[1,iSEE:::.brushEffect] <- iSEE:::.brushRestrictTitle
  
  p.out <- iSEE:::.make_redDimPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_named(
    p.out$cmd$brush,
    c("init","subset")
  )
  expect_match(
    p.out$cmd$plot$brush_restrict,
    "plot.data",
    fixed = TRUE
  )
  # TODO: better tests
  
})
