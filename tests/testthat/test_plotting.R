
# Set up plotting parameters
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 1)
geneExprArgs <- geneExprPlotDefaults(sce, 1)
geneStatArgs <- geneStatTableDefaults(sce, 1)

# Set up memory
all_memory <- iSEE:::.setup_memory(
  sce, redDimArgs, colDataArgs, geneExprArgs, geneStatArgs,
  redDimMax = 1, colDataMax = 1, geneExprMax = 1, geneStatMax = 1)

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
 
  all_memory$redDim[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
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
 
  all_memory$colData[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
  p.out <- iSEE:::.make_colDataPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  expect_named(
    p.out$xy,
    c("Y","X","ColorBy")
  )
  
})

# .make_colDataPlot/.griddotplot ----

test_that(".make_colDataPlot/.griddotplot produce a valid list",{
  
  all_memory$colData[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
  all_memory$colData[1,iSEE:::.colDataXAxisColData] <- "driver_1_s"
  all_memory$colData[1,iSEE:::.colDataYAxis] <- "passes_qc_checks_s"
  
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
 
  all_memory$colData[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
  all_memory$colData[1,iSEE:::.colDataXAxisColData] <- "driver_1_s"
  all_memory$colData[1,iSEE:::.colDataYAxis] <- "passes_qc_checks_s"
  all_memory$colData[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
  
  p.out <- iSEE:::.make_colDataPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  expect_named(
    p.out$xy,
    c("Y","X","ColorBy")
  )
  
})

# .make_geneExprPlot/.scatter_plot ----

test_that(".make_geneExprPlot/.scatter_plot produce a valid list",{
  
  all_memory$geneExpr[1,iSEE:::.geneExprYAxisGeneTable] <- "Gene statistics table 1"
  all_memory$geneExpr[1,iSEE:::.geneExprYAxisGeneTable] <- "Gene statistics table 1"
  
  p.out <- iSEE:::.make_geneExprPlot(
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

test_that(".make_geneExprPlot/.scatter_plot produce a valid xy with color", {
 
  all_memory$geneExpr[1,iSEE:::.geneExprYAxisGeneTable] <- "Gene statistics table 1"
  all_memory$geneExpr[1,iSEE:::.geneExprYAxisGeneTable] <- "Gene statistics table 1"
  all_memory$geneExpr[1,iSEE:::.colorByGeneTable] <- "Gene statistics table 1"
  all_memory$geneExpr[1,iSEE:::.colorByField] <- iSEE:::.colorByGeneTableTitle
  
  p.out <- iSEE:::.make_geneExprPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  expect_named(
    p.out$xy,
    c("Y","X","ColorBy")
  )
  
})

test_that(".make_geneExprPlot works for YAxis set to Gene text", {
  # change the value locally for the specific test
  selected_gene <- "0610009B22Rik"
  
  all_memory$geneExpr[1,iSEE:::.geneExprYAxis] <- iSEE:::.geneExprYAxisGeneTextTitle
  all_memory$geneExpr[1,iSEE:::.geneExprYAxisGeneText] <- selected_gene
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_match(
    p.out$cmd$data$y,
    selected_gene,
    fixed = TRUE
  )
  
})

test_that(".make_geneExprPlot works for XAxis set to Column data", {
  # change the value locally for the specific test
  all_memory$geneExpr[1,iSEE:::.geneExprXAxis] <- iSEE:::.geneExprXAxisColDataTitle
  all_memory$geneExpr[1,iSEE:::.geneExprXAxisColData] <- "dissection_s"
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_match(
    p.out$cmd$data$x,
    "dissection_s",
    fixed = TRUE
  )
  
})

test_that(".make_geneExprPlot works for XAxis set to Gene table", {
  # change the value locally for the specific test
  all_memory$geneExpr[1,iSEE:::.geneExprXAxis] <- iSEE:::.geneExprXAxisGeneTableTitle
  all_memory$geneExpr[1,iSEE:::.geneExprXAxisGeneTable] <- "Gene statistics table 1"
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_match(
    p.out$cmd$data$x,
    "plot.data$X <- assay",
    fixed = TRUE
  )
  
})

test_that(".make_geneExprPlot works for XAxis set to Gene text", {
  selected_gene <- "0610009B22Rik"
  
  # change the value locally for the specific test
  all_memory$geneExpr[1,iSEE:::.geneExprXAxis] <- iSEE:::.geneExprXAxisGeneTextTitle
  all_memory$geneExpr[1,iSEE:::.geneExprXAxisGeneText] <- selected_gene
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  expect_match(
    p.out$cmd$data$x,
    selected_gene,
    fixed = TRUE
  )
  
})

test_that(".make_geneExprPlot works for groupable colour covariate", {
  selected_coldata <- "dissection_s"
  
  # change the value locally for the specific test
  all_memory$geneExpr[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
  all_memory$geneExpr[1,iSEE:::.colorByColData] <- selected_coldata
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
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
  all_memory$colData[1,iSEE:::.colDataXAxis] <- iSEE:::.colorByColDataTitle
  all_memory$colData[1,iSEE:::.colDataXAxisColData] <- selected_coldataX
  
  all_memory$colData[1,iSEE:::.colDataYAxis] <- selected_coldataY
  
  all_memory$colData[1,iSEE:::.colorByField] <- iSEE:::.colDataXAxisNothingTitle
  
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
  
  all_memory$redDim[[iSEE:::.zoomData]][1] <- list(NULL) # TODO
  
  p.out <- iSEE:::.make_redDimPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  params <- all_memory$redDim[1,]
  expected_xy <- data.frame(
    X = reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
    Y = reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
    row.names = colnames(sce)
  )
  
  expect_identical(p.out$xy, expected_xy)

})

# .make_colDataPlot/.violin_plot works with zoom ----

test_that(".make_colDataPlot/.violin_plot works with zoom",{
  
  all_memory$colData[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
  all_memory$colData[1,iSEE:::.colDataXAxisColData] <- "driver_1_s"
  
  # Identify valid values
  x_unique <- unique(as.numeric(as.factor(colData(sce)[,
    all_memory$colData[1,iSEE:::.colDataXAxisColData]
  ])))
  y_range <- range(head(colData(sce)[,
    all_memory$colData[1,iSEE:::.colDataYAxis]
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
  all_memory$colData[[iSEE:::.zoomData]][1] <- list(zoom_range)
  
  p.out <- iSEE:::.make_colDataPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  params <- all_memory$colData[1,]
  expected_xy <- data.frame(
    Y = colData(sce)[,params[[iSEE:::.colDataYAxis]]],
    X = colData(sce)[,params[[iSEE:::.colDataXAxisColData]]],
    row.names = colnames(sce)
  )
  
  expect_identical(p.out$xy, expected_xy)

})

# .make_colDataPlot/.griddotplot works with zoom ----

test_that(".make_colDataPlot/.griddotplot works with zoom",{
  
  all_memory$colData[1,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
  all_memory$colData[1,iSEE:::.colDataXAxisColData] <- "driver_1_s"
  all_memory$colData[1,iSEE:::.colDataYAxis] <- "passes_qc_checks_s"
  
  # Identify valid values
  x_unique <- unique(as.numeric(as.factor(colData(sce)[,
    all_memory$colData[1,iSEE:::.colDataXAxisColData]
  ])))
  y_unique <- unique(as.numeric(as.factor(colData(sce)[,
    all_memory$colData[1,iSEE:::.colDataYAxis]
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
  all_memory$colData[[iSEE:::.zoomData]][1] <- list(zoom_range)
  
  p.out <- iSEE:::.make_colDataPlot(
    id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  params <- all_memory$colData[1,]
  expected_xy <- data.frame(
    Y = colData(sce)[,params[[iSEE:::.colDataYAxis]]],
    X = colData(sce)[,params[[iSEE:::.colDataXAxisColData]]],
    row.names = colnames(sce)
  )
  
  expect_identical(p.out$xy, expected_xy)

})

# .process_colorby_choice handles gene text input ----

test_that(".process_colorby_choice handles gene text input", {
  params <- all_memory$redDim[1,]
  params[[iSEE:::.colorByField]] <- iSEE:::.colorByGeneTextTitle
  params[[iSEE:::.colorByGeneText]] <- rownames(sce)[1]
  
  color_out <- iSEE:::.process_colorby_choice(
    params, all_memory, sce, ExperimentColorMap())
  
  expect_named(
    color_out,
    c("cmd","label","FUN")
  )
   
  expect_match(
    color_out$cmd,
    rownames(sce)[1]
  )
  
  expect_match(
    color_out$label,
    rownames(sce)[1]
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
