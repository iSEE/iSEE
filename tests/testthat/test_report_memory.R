context("report_memory")

# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 2)
colDataArgs <- colDataPlotDefaults(sce, 2)
featAssayArgs <- featAssayPlotDefaults(sce, 3)
rowStatArgs <- rowStatTableDefaults(sce, 3)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
sampAssayArgs <- sampAssayPlotDefaults(sce, 2)
colStatArgs <- colStatTableDefaults(sce, 1)
customDataArgs <- customDataPlotDefaults(sce, 1)
customStatArgs <- customStatTableDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 2)

# Define brush and zoom parameters for one plot each
redDimArgs[["BrushData"]][[1]] <-
    list(xmin = -11.514034644046, xmax = 9.423465477988,
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

redDimArgs[["ZoomData"]][[2]] <-
    c(xmin = -12.351904605072, xmax = 10.614725533384,
      ymin = -12.000238947242, ymax = 0.0090335134551633
    )

test_that(".report_memory generates code that evaluates to the provided object",{
    sce <- iSEE:::.precompute_UI_info(sce, list(PCA2="WHEE"), list(DE="WHOO"))  # Adding custom functions for .setup_memory to check.

    all_memory <- iSEE:::.setup_memory(
        se=sce,
        redDimArgs=redDimArgs,
        colDataArgs=colDataArgs,
        featAssayArgs=featAssayArgs,
        rowStatArgs=rowStatArgs,
        rowDataArgs=rowDataArgs,
        sampAssayArgs=sampAssayArgs,
        colStatArgs=colStatArgs,
        customDataArgs=customDataArgs,
        customStatArgs=customStatArgs,
        heatMapArgs=heatMapArgs,
        redDimMax=3,
        colDataMax=3,
        featAssayMax=4,
        rowStatMax=4,
        rowDataMax=2,
        sampAssayMax=3,
        colStatMax=2,
        customDataMax=0,
        customStatMax=0,
        heatMapMax=3)

    inp0 <- DataFrame(Name = c(
        paste("Reduced dimension plot", 1:3),
        paste("Column data plot", 1:3),
        paste("Feature assay plot", 1:4),
        paste("Row statistics table", 1:4),
        paste("Row data plot", 1:2),
        paste("Sample assay plot", 1:3),
        paste("Column statistics table", 1:2),
        paste("Custom data plot", 1),
        paste("Custom statistics table", 1),
        paste("Heat map", 1:3)),
        Width = 4)
    inp <- iSEE:::.setup_initial(inp0, all_memory)

    # Re-evaluating the commands.
    txt <- iSEE:::.report_memory(inp, all_memory)
    eval_env <- new.env()
    eval(parse(text = txt), envir=eval_env)

    # Checking all the arguments.
    expect_identical(eval_env$redDimPlotArgs, all_memory$redDimPlot)
    expect_identical(eval_env$colDataPlotArgs, all_memory$colDataPlot)
    expect_identical(eval_env$featAssayPlotArgs, all_memory$featAssayPlot)
    expect_identical(eval_env$rowStatTableArgs, all_memory$rowStatTable)
    expect_identical(eval_env$rowDataPlotArgs, all_memory$rowDataPlot)
    expect_identical(eval_env$sampAssayPlotArgs, all_memory$sampAssayPlot)
    expect_identical(eval_env$colStatTableArgs, all_memory$colStatTable)
    expect_identical(eval_env$customDataPlotArgs, all_memory$customDataPlot)
    expect_identical(eval_env$customStatTableArgs, all_memory$customStatTable)
    expect_identical(eval_env$heatMapPlotArgs, all_memory$heatMapPlot)

    # Checking initial panels.
    expect_identical(eval_env$initialPanels$Name, inp0$Name)
})
