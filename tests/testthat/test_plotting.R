
# Set up plotting arguments ----

redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
geneExprArgs <- geneExprPlotDefaults(sce, 1)
geneStatArgs <- geneStatTableDefaults(sce, 1)
redDimMax <- 1 
colDataMax <- 1
geneExprMax <- 1
geneStatMax <- 1

# Change a few default settings ----

redDimArgs[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle

colDataArgs[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle

colDataArgs[2,iSEE:::.colDataXAxis] <- iSEE:::.colDataXAxisColData
colDataArgs[2,iSEE:::.colDataXAxisColData] <- "driver_1_s" # dynamic value?
colDataArgs[2,iSEE:::.colDataYAxis] <- "passes_qc_checks_s" # dynamic value?
colDataArgs[2,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle

geneExprArgs[1,iSEE:::.geneExprYAxisGeneTable] <- "Gene statistics table 1" # dynamic value?
geneExprArgs[1,iSEE:::.geneExprYAxisGeneTable] <- "Gene statistics table 1" # dynamic value?
geneExprArgs[1,iSEE:::.colorByField] <- iSEE:::.colorByGeneTableTitle
geneExprArgs[1,iSEE:::.colorByGeneTable] <- "Gene statistics table 1" # dynamic value?

# Set up memory ----

all_memory <- iSEE:::.setup_memory(sce, redDimArgs, colDataArgs, geneExprArgs, geneStatArgs,
                          redDimMax, colDataMax, geneExprMax, geneStatMax)


# Fill in all_coordinates form a copy-paste of an allen demo session ----

all_coordinates <- list()

################################################################################
## Reduced dimension plot 1 ----
################################################################################

red.dim <- reducedDim(sce, 1);
plot.data <- data.frame(X = red.dim[, 1], Y = red.dim[, 2], row.names=colnames(sce));

# Defining plot limits
xbounds <- range(plot.data$X, na.rm = TRUE);
ybounds <- range(plot.data$Y, na.rm = TRUE);

# Saving data for transmission
all_coordinates[['redDimPlot1']] <- plot.data

zoom_range <- c(range(head(plot.data$X, 10)), range(head(plot.data$Y, 10)))
names(zoom_range) <- c("xmin","xmax","ymin","ymax")
all_memory$redDim[[iSEE:::.zoomData]][[1]] <- zoom_range

################################################################################
## Column data plot 1 ----
################################################################################

plot.data <- data.frame(Y = colData(sce)[,"NREADS"], row.names=colnames(sce));
plot.data$X <- factor(character(ncol(sce)))
plot.data$X <- as.factor(plot.data$X);
plot.data <- subset(plot.data, !is.na(X) & !is.na(Y));

# Defining plot limits
ybounds <- range(plot.data$Y, na.rm = TRUE);

# Saving data for transmission
all_coordinates[['colDataPlot1']] <- plot.data

# Setting up plot aesthetics
plot.data$GroupBy <- plot.data$X;
set.seed(100);
plot.data$jitteredX <- vipor::offsetX(plot.data$Y,
    x=plot.data$X, width=0.4, varwidth=FALSE, adjust=1,
    method='quasirandom', nbins=NULL) + as.integer(plot.data$X);

zoom_range <- c(range(head(as.numeric(plot.data$X), 10)), range(head(plot.data$Y, 10)))
names(zoom_range) <- c("xmin","xmax","ymin","ymax")
all_memory$colData[[iSEE:::.zoomData]][[1]] <- zoom_range

################################################################################
## Column data plot 2 ----
################################################################################

plot.data <- data.frame(Y = colData(sce)[,"passes_qc_checks_s"], row.names=colnames(sce));
plot.data$X <- colData(sce)[,"driver_1_s"];
plot.data$X <- as.factor(plot.data$X);
plot.data$Y <- as.factor(plot.data$Y);

# Saving data for transmission
all_coordinates[['colDataPlot2']] <- plot.data

zoom_range <- c(
  range(head(as.numeric(plot.data$X), 10)),
  range(head(as.numeric(plot.data$Y), 10))
)
names(zoom_range) <- c("xmin","xmax","ymin","ymax")
all_memory$colData[[iSEE:::.zoomData]][[2]] <- zoom_range

################################################################################
## Gene expression plot 1 ----
################################################################################

plot.data <- data.frame(Y=assay(sce, 6)[1L,], row.names = colnames(sce))
plot.data$X <- factor(character(ncol(sce)))
plot.data$X <- as.factor(plot.data$X);
plot.data <- subset(plot.data, !is.na(X) & !is.na(Y));

# Defining plot limits
ybounds <- range(plot.data$Y, na.rm = TRUE);

# Saving data for transmission
all_coordinates[['geneExprPlot1']] <- plot.data

# Setting up plot aesthetics
plot.data$GroupBy <- plot.data$X;
set.seed(100);
plot.data$jitteredX <- vipor::offsetX(plot.data$Y,
    x=plot.data$X, width=0.4, varwidth=FALSE, adjust=1,
    method='quasirandom', nbins=NULL) + as.integer(plot.data$X)

zoom_range <- c(range(head(as.numeric(plot.data$X), 10)), range(head(plot.data$Y, 10)))
names(zoom_range) <- c("xmin","xmax","ymin","ymax")
all_memory$geneExpr[[iSEE:::.zoomData]][[1]] <- zoom_range

####################
# Tests start here #
####################

# .make_redDimPlot/.scatter_plot ----

test_that(".make_redDimPlot/.scatter_plot produce a valid list",{
  
  p.out <- iSEE:::.make_redDimPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
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
    c("X","Y","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})

# .make_colDataPlot/.violin_plot ----

test_that(".make_colDataPlot/.violin_plot produce a valid list",{
  
  p.out <- iSEE:::.make_colDataPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
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
    c("Y","X","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})

# .make_colDataPlot/.griddotplot ----

test_that(".make_colDataPlot/.griddotplot produce a valid list",{
  
  p.out <- iSEE:::.make_colDataPlot(id = 2, all_memory, all_coordinates, sce, ExperimentColorMap())
  
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
    c("Y","X","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})

# .make_geneExprPlot/.scatter_plot ----

test_that(".make_geneExprPlot/.scatter_plot produce a valid list",{
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
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
    c("Y","X","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
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
  all_memory$geneExpr[1,iSEE:::.geneExprXAxisGeneTable] <- "Gene statistics table 1" # dynamic value?
  
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

# .scatter_plot plot without zoom ----

test_that(".scatter_plot works without zoom",{
  
  all_memory$redDim[[iSEE:::.zoomData]][1] <- list(NULL)
  
  p.out <- iSEE:::.make_redDimPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  params <- all_memory$redDim[1,]
  expected_xy <- data.frame(
    X = reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimXAxis]]],
    Y = reducedDim(sce, params[[iSEE:::.redDimType]])[,params[[iSEE:::.redDimYAxis]]],
    ColorBy = colData(sce)[,params[[iSEE:::.colorByColData]]],
    row.names = colnames(sce)
  )
  
  expect_identical(p.out$xy, expected_xy)

})

test_that(".scatter_plot works without zoom",{
  
  all_memory$colData[[iSEE:::.zoomData]][1] <- list(NULL)
  
  p.out <- iSEE:::.make_colDataPlot(id = 1, all_memory, all_coordinates, sce, ExperimentColorMap())
  
  params <- all_memory$colData[1,]
  expected_xy <- data.frame(
    Y = colData(sce)[,params[[iSEE:::.colDataYAxis]]],
    X = factor(rep("", ncol(sce))),
    ColorBy = colData(sce)[,params[[iSEE:::.colorByColData]]],
    row.names = colnames(sce)
  )
  
  expect_identical(p.out$xy, expected_xy)

})

# .process_colorby_choice handles gene text input ----

test_that(".process_colorby_choice handles gene text input", {
  params <- all_memory$redDim[1,]
  params[[iSEE:::.colorByField]] <- iSEE:::.colorByGeneTextTitle
  params[[iSEE:::.colorByGeneText]] <- rownames(sce)[1]
  
  color_out <- iSEE:::.process_colorby_choice(params, all_memory, sce, ExperimentColorMap())
  
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
