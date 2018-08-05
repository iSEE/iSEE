# Set up plotting parameters
redDimArgs <- redDimPlotDefaults(sce, 1)
customColArgs <- customDataPlotDefaults(sce, 1)

# Set up alternative object.
CUSTOM_PCA <- function(se, rows, columns, colour_by=NULL, scale_columns=TRUE) {
    if (!is.null(columns)) {
        kept <- se[,columns]
    } else {
        return(ggplot())
    }

    scale_columns <- as.logical(scale_columns)
    kept <- runPCA(kept, feature_set=rows, scale_columns=scale_columns)
	plotPCA(kept, colour_by=colour_by)
}

sceX <- iSEE:::.precompute_UI_info(sce, list(PCA2=CUSTOM_PCA), list())
customColArgs$Function <- "PCA2"

# Set up memory
all_memory <- iSEE:::.setup_memory(
    se = sceX, 
    redDimArgs=redDimArgs, 
    colDataArgs=NULL, 
    featAssayArgs=NULL,
    sampAssayArgs=NULL,
    rowStatArgs=NULL,
    rowDataArgs=NULL,
    customDataArgs = customColArgs,
    customStatArgs = NULL, # TODO: add an example
    heatMapArgs=NULL,
    redDimMax=1, colDataMax=0, featAssayMax=0, sampAssayMax=0, rowStatMax=0,
    rowDataMax=0, heatMapMax=0, customDataMax = 1,
    customStatMax = 0) # TODO: add an example

all_coordinates <- list()
p.out <- iSEE:::.make_customDataPlot(id = 1, all_memory, all_coordinates, sceX)

####################
# Tests start here #
####################

test_that("getting and setting of custom column functions", {
     expect_error(iSEE:::.get_internal_info(sce, "custom_data_fun"), "no internal")
     expect_identical(iSEE:::.get_internal_info(sceX, "custom_data_fun"), list(PCA2=CUSTOM))
     
    sceX2 <- iSEE:::.precompute_UI_info(sce, NULL)
    expect_identical(iSEE:::.get_internal_info(sceX2, "custom_data_fun"), NULL)
})

test_that(".make_customColPlot produces a valid list", {
    expect_named(p.out, c("cmd_list", "xy", "plot", "cached"))

    expect_named(p.out$cmd_list, c("data", "select", "setup", "plot"))
    expect_match(p.out$cmd_list$setup[1], "PCA2")

    expect_s3_class(p.out$plot, "ggplot")

    expect_identical(p.out$cached, CUSTOM(sceX, colnames(sceX)))
  
    expect_s3_class(p.out$xy, "data.frame")
    expect_named(p.out$xy, c("X","Y"))

    # Re-using the cache.
    p.out2 <- iSEE:::.make_customColPlot(id = 1, all_memory, all_coordinates, sceX, ExperimentColorMap(), cached=p.out$cached)
    expect_equal(p.out, p.out2)
})
  
test_that(".make_customColPlot works when no function is specified", {
    all_memory$customColPlot$Function <- iSEE:::.noSelection
    p.out <- iSEE:::.make_customColPlot(id = 1, all_memory, all_coordinates, sceX, ExperimentColorMap(), cached=NULL)

    expect_named(p.out, c("cmd_list", "xy", "plot", "cached"))
    expect_identical(p.out$cmd_list, NULL)

    expect_s3_class(p.out$plot, "ggplot")

    expect_identical(p.out$cached, NULL)
  
    expect_s3_class(p.out$xy, "data.frame")
    expect_identical(nrow(p.out$xy), 0L)
})

test_that(".make_customColPlot responds to a transmitted receiver", {
    all_memory$customColPlot$SelectByPlot <- "Reduced dimension plot 1"
    all_memory$customColPlot$SelectEffect <- "Restrict"
    all_memory$redDimPlot$BrushData[[1]] <- list(xmin = -11.514034644046, xmax = 9.423465477988, 
         ymin = -10.767314578073, ymax = -1.6587346435671, 
         mapping = list(x = "X", y = "Y"), 
         domain = list(left = -14.0531423894257, 
                       right = 10.9153021971093, 
                       bottom = -12.0002389472417, 
                       top = 16.4373197678157), 
         range = list(left = 39.0740047089041, 
                      right = 382.520547945205, 
                      bottom = 468.220917166096, 
                      top = 24.8879973724842), 
         log = list(x = NULL, y = NULL), direction = "xy", 
         brushId = "redDimPlot1_Brush", 
         outputId = "redDimPlot1")

    r.out <- iSEE:::.make_redDimPlot(id =1, all_memory, all_coordinates, sceX, ExperimentColorMap())    
    all_coordinates[["redDimPlot1"]] <- r.out$xy

    p.out2 <- iSEE:::.make_customColPlot(id = 1, all_memory, all_coordinates, sceX, ExperimentColorMap(), cached=NULL)
    
    # Testing equality:
    expect_named(p.out2, c("cmd_list", "xy", "plot", "cached"))
    expect_match(p.out2$cmd_list$select[1], "redDimPlot1")
    expect_match(p.out2$cmd_list$setup[1], "PCA2")

    expect_false(any(grepl("plot.data.all", unlist(p.out2$cmd_list)))) # There should be no plot.data.all!

    expect_s3_class(p.out2$plot, "ggplot")

    expect_s3_class(p.out2$xy, "data.frame")
    expect_named(p.out2$xy, c("SelectBy", "X","Y"))
    expect_false(nrow(p.out2$xy)==ncol(sceX))

    kept <- rownames(shiny:::brushedPoints(all_coordinates[["redDimPlot1"]], all_memory$redDimPlot$BrushData[[1]]))
    expect_identical(kept, rownames(p.out2$cached$coordinates))
    expect_identical(p.out2$cached, CUSTOM(sceX, kept))

    # Checking that the cache is ignored or used properly.
    p.out3 <- iSEE:::.make_customColPlot(id = 1, all_memory, all_coordinates, sceX, ExperimentColorMap(), cached=p.out$cached)
    expect_equal(p.out2, p.out3)

    p.out4 <- iSEE:::.make_customColPlot(id = 1, all_memory, all_coordinates, sceX, ExperimentColorMap(), cached=p.out2$cached)
    expect_equal(p.out2, p.out4)

    # Still valid when no function is specified.
    all_memory$customColPlot$Function <- iSEE:::.noSelection
    p.out5 <- iSEE:::.make_customColPlot(id = 1, all_memory, all_coordinates, sceX, ExperimentColorMap(), cached=NULL)
    expect_named(p.out5, c("cmd_list", "xy", "plot", "cached"))
    expect_identical(p.out5$cmd_list, NULL)
    expect_identical(p.out5$cached, NULL)
    expect_identical(nrow(p.out5$xy), 0L)
})

test_that(".make_customColPlot responds to colour selection", {
    all_memory$customColPlot$ColorBy <- "Column data"
    all_memory$customColPlot$ColorByColData <- "NALIGNED"
    p.out2 <- iSEE:::.make_customColPlot(id = 1, all_memory, all_coordinates, sceX, ExperimentColorMap(), cached=p.out$cached)

    expect_named(p.out2, c("cmd_list", "xy", "plot", "cached"))

    expect_named(p.out2$cmd_list, c("data", "select", "setup", "plot"))
    expect_match(p.out2$cmd_list$data[2], "NALIGNED")
    expect_match(p.out2$cmd_list$setup[1], "PCA2")

    expect_s3_class(p.out2$plot, "ggplot")

    expect_identical(p.out$cached, p.out2$cached)
  
    expect_s3_class(p.out2$xy, "data.frame")
    expect_named(p.out2$xy, c("ColorBy", "X","Y"))
})
  
test_that(".make_customColPlot responds to downsampling", {
    all_memory$customColPlot$Downsample <- TRUE
    p.out2 <- iSEE:::.make_customColPlot(id = 1, all_memory, all_coordinates, sceX, ExperimentColorMap(), cached=p.out$cached)

    expect_named(p.out2, c("cmd_list", "xy", "plot", "cached"))

    expect_named(p.out2$cmd_list, c("data", "select", "setup", "plot"))
    expect_match(p.out2$cmd_list$plot[1], "^plot.data.pre <-")
    expect_match(p.out2$cmd_list$plot[2], "subsetPointsByGrid")
    expect_match(p.out2$cmd_list$plot['coord'], "plot.data.pre")
})
