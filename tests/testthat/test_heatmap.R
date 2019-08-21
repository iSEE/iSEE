context("heatmap")

# Set up plotting parameters
redDimArgs <- redDimPlotDefaults(sce, 1)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 1)

redDimArgs$Type <- 2L
redDimArgs[[iSEE:::.colorByField]] <- iSEE:::.colorByColDataTitle
redDimArgs[[iSEE:::.colorByColData]] <- "driver_1_s"
redDimArgs$BrushData <- list(list(xmin=-5.8936302389057, xmax=2.3755515915654, ymin=4.3811474670046, ymax=12.602031654675,
    mapping=list(x="X", y="Y"), domain=list(left=-9.30648713007475, right=11.286214260173,
        bottom=-9.17816645795467, top=13.1270784675571), range=list(left=35.3889661815069,
        right=386.520547945205, bottom=466.050486943493, top=23.7921069615253),
    log=list(x=NULL, y=NULL), direction="xy", brushId="redDimPlot1_Brush",
    outputId="redDimPlot1"))

heatMapArgs[[iSEE:::.heatMapFeatName]][[1]] <- head(which(rowData(sce)[, "num_cells"] == ncol(sce)), 100)
heatMapArgs[[iSEE:::.heatMapColData]][[1]] <- c("driver_1_s", "NREADS")
heatMapArgs[[iSEE:::.heatMapCenterScale]][[1]] <- c(iSEE:::.heatMapCenterTitle, iSEE:::.heatMapScaleTitle)
heatMapArgs[[iSEE:::.zoomData]][[1]] <- seq_len(min(10, nrow(sce)))

# Set up memory
sce <- iSEE:::.precompute_UI_info(sce, NULL, NULL)
all_memory <- iSEE:::.setup_memory(sce,
    redDimArgs=redDimArgs,
    colDataArgs=NULL,
    featAssayArgs=NULL,
    rowStatArgs=NULL,
    rowDataArgs=rowDataArgs,
    sampAssayArgs=NULL,
    colStatArgs=NULL,
    customDataArgs=NULL,
    customStatArgs=NULL,
    heatMapArgs=heatMapArgs,
    redDimMax=1,
    colDataMax=0,
    featAssayMax=0,
    rowStatMax=0,
    rowDataMax=1,
    sampAssayMax=0,
    colStatMax=0,
    customDataMax=0,
    customStatMax=0,
    heatMapMax=1)

all_coordinates <- list()

p.out <- iSEE:::.make_redDimPlot(id=1, all_memory, all_coordinates, sce, ExperimentColorMap())
all_coordinates[["redDimPlot1"]] <- p.out$xy[, intersect(iSEE:::.allCoordinatesNames, colnames(p.out$xy))]

# .make_heatMapPlot ----

test_that(".make_heatMapPlot with groupable and non-groupable colData", {

    out <- iSEE:::.make_heatMapPlot(1, all_memory, all_coordinates, se=sce, colormap=ExperimentColorMap())

    expect_named(out, c("cmd_list", "xy", "plot", "legends"))
    expect_true(any(grepl("driver_1_s", out$cmd_list$data)))
    expect_true(any(grepl("NREADS", out$cmd_list$data)))

    # Check that zoom was applied
    expect_true(
        grepl(
            "plot.data <- subset(plot.data, Y %in% rownames(value.mat)[c(1,2,3,4,5,6,7,8,9,10)]);",
            out$cmd_list$zoom, fixed=TRUE
        )
    )

})

test_that(".make_heatMapPlot works with incoming selection", {

    ## Transparency effect for selection

    all_memory$heatMapPlot$SelectByPlot <- "Reduced dimension plot 1"
    all_memory$heatMapPlot$SelectEffect <- iSEE:::.selectTransTitle

    out <- iSEE:::.make_heatMapPlot(1, all_memory, all_coordinates, se=sce, colormap=ExperimentColorMap())

    expect_named(out, c("cmd_list", "xy", "plot", "legends"))
    expect_true(any(grepl("driver_1_s", out$cmd_list$data)))
    expect_true(any(grepl("NREADS", out$cmd_list$data)))
    expect_true(any(grepl("geom_raster(aes(fill=OrderBy1, alpha=SelectBy)) +", out$cmd_list$annot, fixed=TRUE)))

    ## Color effect for selection

    all_memory$heatMapPlot$SelectEffect <- iSEE:::.selectColorTitle

    out <- iSEE:::.make_heatMapPlot(1, all_memory, all_coordinates, se=sce, colormap=ExperimentColorMap())

    expect_true(any(grepl("geom_raster(aes(fill=SelectBy))", out$cmd_list$annot, fixed=TRUE)))

})

# .get_colorscale_limits ----

test_that(".get_colorscale_limits produces expected color scale limits", {

    # No bounds defined
    min.value <- -1
    max.value <- 1
    lower.bound <- -Inf
    upper.bound <- Inf

    out <- iSEE:::.get_colorscale_limits(min.value, max.value, lower.bound, upper.bound, include.zero=TRUE)
    expect_identical(out, c(min.value, 0, max.value))

    out <- iSEE:::.get_colorscale_limits(min.value, max.value, lower.bound, upper.bound, include.zero=FALSE)
    expect_identical(out, c(min.value, max.value))

    # Bounds defined
    lower.bound <- -0.5
    upper.bound <- 0.5

    out <- iSEE:::.get_colorscale_limits(min.value, max.value, lower.bound, upper.bound, include.zero=TRUE)
    expect_identical(out, c(lower.bound, 0, upper.bound))

    out <- iSEE:::.get_colorscale_limits(min.value, max.value, lower.bound, upper.bound, include.zero=FALSE)
    expect_identical(out, c(lower.bound, upper.bound))

})

# .cluster_genes ----

test_that(".cluster_genes clusters rows of a given matrix", {

    selectedGenes <- heatMapArgs[[iSEE:::.heatMapFeatName]][[1]]
    selectedCells <- seq_len(min(10, ncol(sce)))
    X <- assay(sce, "logcounts")[selectedGenes, selectedCells]

    # The return value should include all the rownames of the input matrix
    out <- iSEE:::.cluster_genes(X)
    expect_identical(sort(out),  sort(rownames(X)))


    # Clustering a clustered matrix should not change the order
    cluteredX <- X[out, ]

    out2 <- iSEE:::.cluster_genes(cluteredX)
    expect_identical(out, out2)

})

# .transform_global_to_local_y ----

test_that(".transform_global_to_local_y works", {

    n.genes <- 10
    n.annot <- 2

    # The maximal local value is capped to n.genes
    out <- iSEE:::.transform_global_to_local_y(2, n.annot, n.genes)
    expect_identical(out, n.genes)

    # The maximal local value is capped to 1
    out <- iSEE:::.transform_global_to_local_y(-Inf, n.annot, n.genes)
    expect_identical(out, 1)

    # Global coordinates range from 0 to 1; values between 0 and 1 return a valid gene index
    out <- iSEE:::.transform_global_to_local_y(0.5, n.annot, n.genes)
    expect_lte(out, n.genes)
    expect_gte(out, 1)

})

# .get_heatmap_fill_cmd ----

test_that(".get_heatmap_fill_cmd adapts fill limits to bounds set within the observed range", {

    # Bounds defined within the min/max observed range
    lower.bound <- -0.5
    upper.bound <- 0.5
    min.obs <- -6
    max.obs <- 6

    param_choices <- heatMapArgs
    param_choices[[iSEE:::.heatMapLower]] <- lower.bound
    param_choices[[iSEE:::.heatMapUpper]] <- upper.bound

    out <- iSEE:::.get_heatmap_fill_cmd(param_choices, ExperimentColorMap(), min.obs, max.obs)
    expectedLimits <- sprintf("limits=c(%i,%i)", min.obs, max.obs)
    expect_match(out, expectedLimits, fixed=TRUE)

})

test_that(".get_heatmap_fill_cmd adapts fill limits to bounds set within the observed range when centred", {

    # If there is only one value in the matrix (since the data is centered, the value is 0)
    min.obs <- 0
    max.obs <- 0

    param_choices <- heatMapArgs

    out <- iSEE:::.get_heatmap_fill_cmd(param_choices, ExperimentColorMap(), min.obs, max.obs)
    expect_match(out, "scale_fill_gradientn(colors=c('black'),
                                na.value='grey50') +", fixed=TRUE)

})

test_that(".get_heatmap_fill_cmd works when not centred", {

    param_choices <- heatMapArgs
    param_choices[[iSEE:::.heatMapCenterScale]][[1]] <- "Scaled"

    # Bounds defined within the min/max observed range
    lower.bound <- -0.5
    upper.bound <- 0.5
    min.obs <- -6
    max.obs <- 6

    param_choices[[iSEE:::.heatMapLower]] <- lower.bound
    param_choices[[iSEE:::.heatMapUpper]] <- upper.bound

    out <- iSEE:::.get_heatmap_fill_cmd(param_choices, ExperimentColorMap(), min.obs, max.obs)
    expect_match(
        out,
        "colors=c('#440154FF', assayColorMap(colormap, '2', discrete=FALSE)(21L), '#FDE725FF')",
        fixed=TRUE)
    expect_match(
        out,
        "values=scales::rescale(c(-6, seq(-0.5, 0.5, length.out=21L), 6), to=c(0, 1), from=c(-6, 6))",
        fixed=TRUE)

    # Disabling the upper limit only adds an additional lower color and break (to the left)
    lower.bound <- -0.5
    upper.bound <- Inf
    param_choices[[iSEE:::.heatMapLower]] <- lower.bound
    param_choices[[iSEE:::.heatMapUpper]] <- upper.bound

    out <- iSEE:::.get_heatmap_fill_cmd(param_choices, ExperimentColorMap(), min.obs, max.obs)
    expect_match(
        out,
        "colors=c('#440154FF', assayColorMap(colormap, '2', discrete=FALSE)(21L))",
        fixed=TRUE)
    expect_match(
        out,
        "values=scales::rescale(c(-6, seq(-0.5, 6, length.out=21L)), to=c(0, 1), from=c(-6, 6))",
        fixed=TRUE)

    # Disabling the lower limit only adds an additional upper color and break (to the right)
    lower.bound <- -Inf
    upper.bound <- 0.5
    param_choices[[iSEE:::.heatMapLower]] <- lower.bound
    param_choices[[iSEE:::.heatMapUpper]] <- upper.bound

    out <- iSEE:::.get_heatmap_fill_cmd(param_choices, ExperimentColorMap(), min.obs, max.obs)
    expect_match(
        out,
        "colors=c(assayColorMap(colormap, '2', discrete=FALSE)(21L), '#FDE725FF')",
        fixed=TRUE)
    expect_match(
        out,
        "values=scales::rescale(c(seq(-6, 0.5, length.out=21L), 6), to=c(0, 1), from=c(-6, 6))",
        fixed=TRUE)

})
