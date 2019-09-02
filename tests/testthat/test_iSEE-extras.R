context("iSEE-extras")

stopifnot(
  require(shiny)
)

# This script tests the code related to extra functionalities including reporting of panel links.
# library(iSEE); library(testthat); source("setup_sce.R"); source("test_track_code.R")

# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
featAssayArgs <- featAssayPlotDefaults(sce, 3)
rowStatArgs <- rowStatTableDefaults(sce, 3)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
sampAssayArgs <- sampAssayPlotDefaults(sce, 3)
colStatArgs <- colStatTableDefaults(sce, 3)
customDataArgs <- customDataPlotDefaults(sce, 1)
customStatArgs <- customStatTableDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 2)

# Setting up a chain of plots.
redDimArgs[1, iSEE:::.selectByPlot] <- "---"
colDataArgs[1, iSEE:::.selectByPlot] <- "Reduced dimension plot 1"
colDataArgs[2, iSEE:::.selectByPlot] <- "Reduced dimension plot 1"
featAssayArgs[2, iSEE:::.selectByPlot] <- "Column data plot 1"
heatMapArgs[1, iSEE:::.selectByPlot] <- "Column data plot 1"
featAssayArgs[1, iSEE:::.selectByPlot] <- "Feature assay plot 1"

# Setting up additional parameters to test color linking
colDataArgs[1, iSEE:::.colorByField] <- iSEE:::.colorByFeatNameTitle
colDataArgs[1, iSEE:::.colorByRowTable] <- "Row statistics table 1"

# Setting up additional parameters to test X/Y axis linking
featAssayArgs[2, iSEE:::.featAssayYAxisRowTable] <- "Row statistics table 1"
featAssayArgs[2, iSEE:::.featAssayXAxis] <- iSEE:::.featAssayXAxisFeatNameTitle
featAssayArgs[2, iSEE:::.featAssayXAxisRowTable] <- "Row statistics table 2"

# Setting up additional parameters for table links
rowStatArgs[2, iSEE:::.selectByPlot] <- "Feature assay plot 3"
colStatArgs[1, iSEE:::.selectByPlot] <- "Column data plot 1"
sampAssayArgs[1, iSEE:::.sampAssayXAxis] <- iSEE:::.sampAssayXAxisSampNameTitle
sampAssayArgs[1, iSEE:::.sampAssayXAxisColTable] <- "Column statistics table 1"

sce <- iSEE:::.precompute_UI_info(sce, NULL, NULL)
all_memory <- iSEE:::.setup_memory(
    sce,
    redDimArgs, colDataArgs, featAssayArgs, rowStatArgs, rowDataArgs,
    sampAssayArgs, colStatArgs, customDataArgs, customStatArgs, heatMapArgs,
    redDimMax=1, colDataMax=2, featAssayMax=3, rowStatMax=3, rowDataMax=1,
    sampAssayMax=3, colStatMax=3, customDataMax=1, customStatMax=1, heatMapMax=2)

g <- iSEE:::.spawn_selection_chart(all_memory)
tl <- iSEE:::.spawn_table_links(all_memory)

all_coordinates <- list()

# nested DataFrame ----

test_that("nested DataFrame are extracted correctly", {

    df_info <- iSEE:::.extract_nested_DF(colData(sce))

    nested_colnames <- colnames(colData(sce)[, "nested"])

    expected_getters <- sprintf("[[\"nested\"]][[\"%s\"]]", nested_colnames)
    expected_setters <- sprintf("nested:%s", nested_colnames)

    expect_identical(df_info$getter, expected_getters)
    expect_identical(df_info$setter, expected_setters)
})

test_that(".sanitize_SE_input returns expected commands and object", {
    colData(sce)$nested <- DataFrame(nested=seq_len(ncol(sce)))
    rowData(sce)$nested <- DataFrame(nested=seq_len(nrow(sce)))
    sizeFactors(sce) <- runif(ncol(sce))

    sanitized_list <- iSEE:::.sanitize_SE_input(sce)
    sanitized_cmds <- sanitized_list$cmds
    sanitized_sce <- sanitized_list$object

    # Checking that the extracted values are correct.
    expect_identical(colData(sanitized_sce)[["nested:nested"]], sce$nested$nested)
    expect_identical(rowData(sanitized_sce)[["nested:nested"]], rowData(sce)$nested$nested)
    expect_identical(colData(sanitized_sce)[["sizeFactors(se)"]], sizeFactors(sce))

    # emulate the function behaviour to obtain the expected
    eval_env <- new.env()
    eval_env$se <- sce
    eval(parse(text=sanitized_cmds), envir=eval_env)
    expected_se <- eval_env$se

    for (f in colnames(colData(expected_se))) {
        cur_field <- colData(expected_se)[[f]]
        if (!is.numeric(cur_field) && !is.factor(cur_field)
            && !is.character(cur_field) && !is.logical(cur_field)) {
            colData(expected_se)[[f]] <- NULL
        }
    }

    for (f in colnames(rowData(expected_se))) {
        cur_field <- rowData(expected_se)[[f]]
        if (!is.numeric(cur_field) && !is.factor(cur_field)
            && !is.character(cur_field) && !is.logical(cur_field)) {
            rowData(expected_se)[[f]] <- NULL
        }
    }

    expect_identical(sanitized_sce, expected_se)
})

# .setup_initial ----

test_that(".setup_initial throws an error if Name column is missing", {

    initialPanels <- DataFrame(Width=rep(4, 3))
    all_memory <- list()

    expect_error(
        iSEE:::.setup_initial(initialPanels, all_memory),
        "need 'Name' field in 'initialPanels'",
        fixed=TRUE
    )

})

test_that(".setup_initial does not allow duplicated values in the Name column", {

    initialPanels <- DataFrame(Name=rep("Dummy", 2))
    all_memory <- list()

    expect_error(
        iSEE:::.setup_initial(initialPanels, all_memory),
        "duplicated values are not allowed in the 'Name' field of 'initialPanels'",
        fixed=TRUE
    )

})

test_that(".setup_initial supports 0-row initialPanels", {

    initialPanels <- data.frame(Name=character(0))
    all_memory <- list()

    out <- iSEE:::.setup_initial(initialPanels, all_memory)

    expected <- data.frame(
      Type = character(0),
      ID = integer(0),
      Width = integer(0),
      Height = integer(0),
      stringsAsFactors = FALSE
    )

    expect_identical(out, expected)

})

# .define_plot_links ----

test_that(".define_plot_links detects receiving and transmitting links", {

    # Report receiving and transmitting links
    out <- iSEE:::.define_plot_links("colDataPlot1", all_memory, g)
    expect_is(out, c("shiny.tag.list", "list"))

    # Every third element is a receiving/transmitting header
    expect_identical(out[[1]], "Receiving selection from")
    expect_identical(out[[3]], br())

    expect_identical(out[[4]], "Receiving color from")
    expect_identical(out[[6]], br())

    expect_identical(out[[7]], "Transmitting selection to")
    expect_identical(out[[9]], br())

    expect_identical(out[[10]], "Transmitting selection to")
    expect_identical(out[[12]], br())

    # Heat maps are not handled by this function
    expect_error(
        iSEE:::.define_plot_links("heatMapPlot1", all_memory, g),
        "missing value where TRUE/FALSE needed",
        fixed=TRUE
    )

})

test_that(".define_plot_links treats featAssayPlot specially", {

    # Report receiving and transmitting links
    out <- iSEE:::.define_plot_links("featAssayPlot2", all_memory, g)
    expect_is(out, c("shiny.tag.list", "list"))

})

# .define_table_links ----

test_that(".define_plot_links detects in/out links for rowStatTables", {

    # Report receiving and transmitting links
    out <- iSEE:::.define_table_links("rowStatTable2", all_memory, tl)
    expect_is(out, c("shiny.tag.list", "list"))

    # Every third element is a receiving/transmitting header
    expect_identical(out[[1]], "Receiving selection from")
    expect_identical(out[[3]], br())

    expect_identical(out[[4]], "Transmitting x-axis to")
    expect_identical(out[[6]], br())

})

test_that(".define_plot_links detects in/out links for colStatTables", {

    # Report receiving and transmitting links
    out <- iSEE:::.define_table_links("colStatTable1", all_memory, tl)
    expect_is(out, c("shiny.tag.list", "list"))

    # Every third element is a receiving/transmitting header
    expect_identical(out[[1]], "Receiving selection from")
    expect_identical(out[[3]], br())

    expect_identical(out[[4]], "Transmitting x-axis to")
    expect_identical(out[[6]], br())

})

# .sanitize_SE_input ----

test_that("ExpressionSet objects can be sanitized", {

    eset <- as(sce, "ExpressionSet")

    out <- iSEE:::.sanitize_SE_input(eset)
    expect_is(out, "list")
    expect_named(out, c("cmds", "object"))
    expect_s4_class(out$object, "SingleCellExperiment")
    expect_identical(out$cmds, c(
        "se <- as(se, \"SummarizedExperiment\")",
        "se <- as(se, \"SingleCellExperiment\")"))

})

test_that("Nameless SummarizedExperiment objects can be sanitized", {

    se <- as(sce, "SummarizedExperiment")
    colnames(se) <- NULL
    rownames(se) <- NULL

    out <- iSEE:::.sanitize_SE_input(se)
    expect_is(out, "list")
    expect_named(out, c("cmds", "object"))
    expect_s4_class(out$object, "SingleCellExperiment")
    expect_identical(
        out$cmds, c(
            "se <- as(se, \"SingleCellExperiment\")",
            "colnames(se) <- sprintf(\"SAMPLE_%i\", seq_len(ncol(se)))",
            "rownames(se) <- sprintf(\"FEATURE_%i\", seq_len(nrow(se)))",
            "colData(se)[, \"nested:nested1\"] <- colData(se)[[\"nested\"]][[\"nested1\"]]",
            "colData(se)[, \"nested:nested2\"] <- colData(se)[[\"nested\"]][[\"nested2\"]]"))

})

# .safe_field_name ----

test_that(".safe_field_name prepends _ to disambiguate colnames", {

    out <- iSEE:::.safe_field_name("test", "test")
    expect_identical(out, "_test")

    out <- iSEE:::.safe_field_name("test", c("test", "_test", "__test"))
    expect_identical(out, "___test")

})

# .get_selected_points ----

test_that(".get_selected_points works", {

    all_memory$colDataPlot[[iSEE:::.brushData]][1] <- list(NULL)

    p.out <- iSEE:::.make_redDimPlot(id=1, all_memory, all_coordinates, sce, ExperimentColorMap())
    all_coordinates[["redDimPlot1"]] <- p.out$xy[, intersect(iSEE:::.allCoordinatesNames, colnames(p.out$xy))]

    # No selection in transmitter panel
    out <- iSEE:::.get_selected_points(
        names = rownames(all_coordinates[["redDimPlot1"]]),
        transmitter = "Column data plot 1",
        all_memory, all_coordinates
        )
    expect_null(out)

    all_memory$colDataPlot[[iSEE:::.brushData]][1] <- list(list(
        xmin=0.9, xmax=1.1, ymin=2E7, ymax=4E7,
        direction="xy", mapping=list(x="X", y="Y"),
        brushId="dummy_brush", outputId="dummy_plot"
    ))
    p.out <- iSEE:::.make_colDataPlot(1, all_memory, all_coordinates, sce, ExperimentColorMap())
    all_coordinates[["colDataPlot1"]] <- p.out$xy[, intersect(iSEE:::.allCoordinatesNames, colnames(p.out$xy))]

    # A selection exists in the transmitter panel
    out <- iSEE:::.get_selected_points(
        names = rownames(all_coordinates[["redDimPlot1"]]),
        transmitter = "Column data plot 1",
        all_memory, all_coordinates
        )
    # Mimic brushedPoints to check whether the logical vector is correct
    expectedNames <- rownames(brushedPoints(
        all_coordinates[["colDataPlot1"]],
        all_memory$colDataPlot[[iSEE:::.brushData]][[1]]))
    expectedOut <- (rownames(all_coordinates[["redDimPlot1"]]) %in% expectedNames)
    expect_identical(out, expectedOut)

    # A history of selection exists in the transmitter panel and "select_all" is TRUE
    all_memory$colDataPlot[[iSEE:::.multiSelectHistory]][[1]] <- list(list(
        xmin=0.9, xmax=1.1, ymin=2E7, ymax=4E7,
        direction="xy", mapping=list(x="X", y="Y"),
        brushId="dummy_brush", outputId="dummy_plot"
    ))
    out <- iSEE:::.get_selected_points(
        names = rownames(all_coordinates[["redDimPlot1"]]),
        transmitter = "Column data plot 1",
        all_memory, all_coordinates, select_all = TRUE
    )
    expect_named(out, c("active", "saved"))
    expect_type(out$active, "logical")
    expect_length(out$saved, 1L)
    expect_type(out$active[[1]], "logical")

})

test_that(".regenerate_unselected_plot can regenerate plots manually", {

    pObjects <- new.env()
    pObjects$memory <- all_memory

    rObjects <- new.env()
    rObjects[["redDimPlot1"]] <- 1L

    input <- list()
    session <- NULL

    iSEE:::.regenerate_unselected_plot("redDimPlot", 1, pObjects, rObjects)

    expect_identical(rObjects[["redDimPlot1"]], 2L)

})

test_that(".regenerate_unselected_plot can destroy brushes, lassos, and selection history", {

    # Add dummy active and saved selections
    all_memory$redDimPlot[[iSEE:::.brushData]][[1]] <- list(a=1, b=2)
    all_memory$redDimPlot[[iSEE:::.multiSelectHistory]][[1]] <- list(list(a=1, b=2))

    pObjects <- new.env()
    pObjects$memory <- all_memory

    rObjects <- new.env()
    rObjects[["redDimPlot1"]] <- 1L
    rObjects[["redDimPlot1_reactivated"]] <- 1L
    rObjects[["redDimPlot1_resaved"]] <- 1L

    input <- list()
    # session <- NULL

    iSEE:::.regenerate_unselected_plot("redDimPlot", 1, pObjects, rObjects)

    expect_identical(rObjects[["redDimPlot1"]], 2L)
    expect_identical(rObjects[["redDimPlot1_reactivated"]], 2L)
    expect_identical(rObjects[["redDimPlot1_resaved"]], 2L)

})
