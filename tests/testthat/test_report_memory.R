# library(testthat); library(iSEE); source("setup_sce.R"); source("setup_mimic_live_app.R"); source("test_report_memory.R")

context("report_memory")

memory <- list(
    ReducedDimensionPlot(
        BrushData=list(xmin = -11.514034644046, xmax = 9.423465477988,
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
            brushId = "ReducedDimensionPlot1_Brush",
            outputId = "ReducedDimensionPlot1"
        )
    ),
    ReducedDimensionPlot(
        ZoomData=c(xmin = -12.351904605072, xmax = 10.614725533384,
            ymin = -12.000238947242, ymax = 0.0090335134551633
        )
    ),
    ColumnDataPlot(),
    ColumnDataPlot(),
    FeatureAssayPlot(),
    FeatureAssayPlot(),
    FeatureAssayPlot(),
    RowDataTable(),
    SampleAssayPlot(),
    RowDataPlot(),
    RowDataPlot()
)

pObjects <- mimic_live_app(sce, memory)

test_that(".report_memory generates code that evaluates to the provided object",{
    txt <- iSEE:::.report_memory(pObjects$memory)
    eval_env <- new.env()
    eval(parse(text = txt), envir=eval_env)
    expect_identical(pObjects$memory, eval_env$initial)
})
