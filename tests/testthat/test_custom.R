context("custom")

# This script tests the code related to the creation of custom panels.
# library(iSEE); library(testthat); source("setup_sce.R"); source("test_custom.R")

# Set up plotting parameters
customDataArgs <- customDataPlotDefaults(sce, 1)
customStatArgs <- customStatTableDefaults(sce, 1)

# Set up alternative object.
CUSTOM_DATA <- function(se, rows, columns, colour_by=NULL, scale_columns=TRUE) {

    if (length(columns)) { # either NULL (no receiver) or character(0) (empty selection)
        kept <- se[, columns]
    } else {
        return(ggplot())
    }

    scale_columns <- as.logical(scale_columns)
    kept <- runPCA(kept, subset_row=rows, scale=scale_columns)
	plotPCA(kept, colour_by=colour_by)
}

CUSTOM_STAT <- function(se, rows, columns, colour_by=NULL, scale_columns=TRUE) {
    if (!is.null(columns)) {
        kept <- se[,columns]
    } else {
        return(data.frame())
    }

    data.frame(present=TRUE)
}

sceX <- iSEE:::.precompute_UI_info(sce, list(PCA2=CUSTOM_DATA), list(PRESENT=CUSTOM_STAT))
customDataArgs[1, iSEE:::.customFun] <- "PCA2"
customDataArgs[1, iSEE:::.customArgs] <- "colour_by Nanog"
customStatArgs[1, iSEE:::.customFun] <- "PRESENT"

# Set up memory
all_memory <- iSEE:::.setup_memory(
    se=sceX,
    redDimArgs=NULL,
    colDataArgs=NULL,
    featAssayArgs=NULL,
    rowStatArgs=NULL,
    rowDataArgs=NULL,
    sampAssayArgs=NULL,
    colStatArgs=NULL,
    customDataArgs=customDataArgs,
    customStatArgs=customStatArgs,
    heatMapArgs=NULL,
    redDimMax=1, colDataMax=0, featAssayMax=0, rowStatMax=0, rowDataMax=1,
    sampAssayMax=0, colStatMax=0, customDataMax=1, customStatMax=1, heatMapMax=0)

all_coordinates <- list()
p.out <- iSEE:::.make_customDataPlot(id=1, all_memory, all_coordinates, sceX)

####################
# Tests start here #
####################

test_that("getting and setting of custom column functions", {
    expect_error(iSEE:::.get_internal_info(sce, "custom_data_fun"), "no internal")
    expect_identical(iSEE:::.get_internal_info(sceX, "custom_data_fun"), list(PCA2=CUSTOM_DATA))

    expect_error(iSEE:::.get_internal_info(sce, "custom_stat_fun"), "no internal")
    expect_identical(iSEE:::.get_internal_info(sceX, "custom_stat_fun"), list(PRESENT=CUSTOM_STAT))

    sceX2 <- iSEE:::.precompute_UI_info(sce, NULL, NULL)
    expect_identical(iSEE:::.get_internal_info(sceX2, "custom_data_fun"), NULL)
})

test_that(".make_customDataPlot produces a valid list", {
    expect_named(p.out, c("cmd_list", "plot"))

    expect_named(p.out$cmd_list, c("select", "plot"))
    expect_match(p.out$cmd_list$plot[1], "PCA2")

    expect_s3_class(p.out$plot, "ggplot")
})

test_that(".make_customDataPlot works when no function is specified", {
    all_memory$customDataPlot$Function <- iSEE:::.noSelection
    p.out <- iSEE:::.make_customDataPlot(id=1, all_memory, all_coordinates, sceX)

    expect_named(p.out, c("cmd_list", "plot"))
    expect_named(p.out$cmd_list, c("select", "plot"))
    expect_match(p.out$cmd_list$plot[1], "ggplot")
    expect_identical(p.out$cmd_list$select, c("row.names <- NULL;", "col.names <- NULL;"))

    expect_s3_class(p.out$plot, "ggplot")
})

test_that(".make_customDataPlot responds to a transmitting column brush receiver", {
    all_memory$customDataPlot$ColumnSource <- "Reduced dimension plot 1"

    # Shiny brush
    all_memory$redDimPlot$BrushData[[1]] <- list(xmin=-11.514034644046, xmax=9.423465477988,
         ymin=-10.767314578073, ymax=-1.6587346435671,
         mapping=list(x="X", y="Y"),
         domain=list(left=-14.0531423894257,
                       right=10.9153021971093,
                       bottom=-12.0002389472417,
                       top=16.4373197678157),
         range=list(left=39.0740047089041,
                      right=382.520547945205,
                      bottom=468.220917166096,
                      top=24.8879973724842),
         log=list(x=NULL, y=NULL), direction="xy",
         brushId="redDimPlot1_Brush",
         outputId="redDimPlot1")

    r.out <- iSEE:::.make_redDimPlot(id =1, all_memory, all_coordinates, sceX, ExperimentColorMap())
    all_coordinates[["redDimPlot1"]] <- r.out$xy

    p.out2 <- iSEE:::.make_customDataPlot(id=1, all_memory, all_coordinates, sceX)

    # Testing equality:
    expect_named(p.out2, c("cmd_list", "plot"))
    expect_identical(p.out2$cmd_list$select[1], "row.names <- NULL;")
    expect_match(p.out2$cmd_list$select[2], "redDimPlot1")
    expect_match(p.out2$cmd_list$plot, "PCA2")
    expect_s3_class(p.out2$plot, "ggplot")

    # Still valid when no function is specified.
    all_memory$customDataPlot$Function <- iSEE:::.noSelection
    p.out5 <- iSEE:::.make_customDataPlot(id=1, all_memory, all_coordinates, sceX)
    expect_named(p.out5, c("cmd_list", "plot"))
    expect_length(p.out5$cmd_list, 2)
})

test_that(".make_customDataPlot responds to a transmitting column brush receiver", {
    all_memory$customDataPlot$ColumnSource <- "Reduced dimension plot 1"

    # Lasso selection
    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(
            -50, -25,
            -25, -25,
            -25, 50,
            -50, 50,
            -50, -25),
            ncol=2, byrow = TRUE))
    all_memory$redDimPlot[[iSEE:::.lassoData]][[1]] <- LASSO_CLOSED

    r.out <- iSEE:::.make_redDimPlot(id =1, all_memory, all_coordinates, sceX, ExperimentColorMap())
    all_coordinates[["redDimPlot1"]] <- r.out$xy

    p.out2 <- iSEE:::.make_customDataPlot(id=1, all_memory, all_coordinates, sceX)

    # Testing equality:
    expect_named(p.out2, c("cmd_list", "plot"))
    expect_identical(p.out2$cmd_list$select[1], "row.names <- NULL;")
    expect_match(p.out2$cmd_list$select[2], "redDimPlot1")
    expect_match(p.out2$cmd_list$plot, "PCA2")
    expect_s3_class(p.out2$plot, "ggplot")

    # Still valid when no function is specified.
    all_memory$customDataPlot$Function <- iSEE:::.noSelection
    p.out5 <- iSEE:::.make_customDataPlot(id=1, all_memory, all_coordinates, sceX)
    expect_named(p.out5, c("cmd_list", "plot"))
    expect_length(p.out5$cmd_list, 2)
})

test_that(".make_customDataPlot responds to a transmitting row receiver", {
    all_memory$customDataPlot$RowSource <- "Row data plot 1"
    all_memory$rowDataPlot$BrushData[[1]] <- list(xmin=0.81539988574393, xmax=1.1448535674541,
            ymin=234.6326316453, ymax=392.16655162581,
            mapping=list(x="X", y="Y", group="GroupBy"),
            domain=list(left=0.5, right=1.5, bottom=-18.95, top=397.95),
            range=list(left=43.1506849315069, right=504.520547945205, bottom=474.849315068493, top=24.958904109589),
            log=list(x=NULL, y=NULL),
            direction="xy",
            brushId="rowDataPlot1_Brush",
            outputId="rowDataPlot1")

    r.out <- iSEE:::.make_rowDataPlot(id =1, all_memory, all_coordinates, sceX, ExperimentColorMap())
    all_coordinates[["rowDataPlot1"]] <- r.out$xy

    p.out2 <- iSEE:::.make_customDataPlot(id=1, all_memory, all_coordinates, sceX)

    # Testing equality:
    expect_named(p.out2, c("cmd_list", "plot"))
    expect_match(p.out2$cmd_list$select[1], "rowDataPlot1")
    expect_identical(p.out2$cmd_list$select[2], "col.names <- NULL;")
    expect_match(p.out2$cmd_list$plot, "PCA2")
    expect_s3_class(p.out2$plot, "ggplot")
})

test_that(".make_customDataPlot responds to an empty transmitting column brush receiver", {
    all_memory$customDataPlot$ColumnSource <- "Reduced dimension plot 1"

    # Lasso selection
    LASSO_CLOSED <- list(
        lasso=NULL,
        closed=TRUE,
        panelvar1=NULL, panelvar2=NULL,
        mapping=list(x="X", y="Y"),
        coord=matrix(c(
            -50, -25,
            -49, -25,
            -49, -24,
            -50, -24,
            -50, -25),
            ncol=2, byrow = TRUE))
    all_memory$redDimPlot[[iSEE:::.lassoData]][[1]] <- LASSO_CLOSED

    r.out <- iSEE:::.make_redDimPlot(id =1, all_memory, all_coordinates, sceX, ExperimentColorMap())
    all_coordinates[["redDimPlot1"]] <- r.out$xy

    p.out2 <- iSEE:::.make_customDataPlot(id=1, all_memory, all_coordinates, sceX)
    expect_named(p.out2, c("cmd_list", "plot"))
    expect_match(p.out2$cmd_list$select[1], "row.names <- NULL")
    expect_match(p.out2$cmd_list$select[2], "redDimPlot1")
    expect_match(p.out2$cmd_list$plot, "PCA2")
    expect_s3_class(p.out2$plot, "ggplot")
})

test_that(".text2args works correctly", {
    out <- iSEE:::.text2args("whee blah
yay 23")
    expect_identical(out, c(whee="blah", yay="23"))

    # Handles empty arguments.
    out <- iSEE:::.text2args("whee blah
yay")
    expect_identical(out, c(whee="blah", yay=""))

    # Handles empty lines.
    out <- iSEE:::.text2args("
yay 1
")
    expect_identical(out, c(yay="1"))

    # Handles empty lines.
    out <- iSEE:::.text2args("whee blah

yay 1
")
    expect_identical(out, c(whee="blah", yay="1"))

    # Strips front space.
    out <- iSEE:::.text2args("  yay 1")
    expect_identical(out, c(yay="1"))

    # Only processes the first space.
    out <- iSEE:::.text2args("yay 1 2 3")
    expect_identical(out, c(yay="1 2 3"))

    # Handles empty inputs.
    out <- iSEE:::.text2args("
            ")
    expect_identical(unname(out), character(0L))
})
