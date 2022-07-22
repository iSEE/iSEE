# This tests the multiple selection machinery.
# library(testthat); library(iSEE); source("setup_sce.R"); source("setup_mimic_live_app.R"); source("test_multi_select.R")

# Setting up a chain of plots.
memory <- list(
    ReducedDimensionPlot(ColorByFeatureSource="RowDataTable1", ColorBy="Feature name"),
    ColumnDataPlot(ColumnSelectionSource="ReducedDimensionPlot1"),
    ColumnDataPlot(ColumnSelectionSource="ReducedDimensionPlot1"),
    FeatureAssayPlot(ColumnSelectionSource="ColumnDataPlot1"),
    FeatureAssayPlot(ColumnSelectionSource="FeatureAssayPlot1", YAxisFeatureSource="RowDataTable1"),
    RowDataPlot(RowSelectionSource="SampleAssayPlot1"),
    SampleAssayPlot(),
    RowDataTable()
)

pObjects <- mimic_live_app(sce, memory)

# Set up the selected data (in redDim1)
rdp <- pObjects$memory$ReducedDimensionPlot1
rd <- reducedDim(sce, rdp[[iSEE:::.redDimType]])
x_10 <- head(rd[, rdp[[iSEE:::.redDimXAxis]]], 10)
y_10 <- head(rd[, rdp[[iSEE:::.redDimYAxis]]], 10)

###############################################

DUMMY_BRUSH <- list(
    xmin=min(x_10), xmax=max(x_10), ymin=min(y_10), ymax=max(y_10),
    direction="xy", mapping=list(x="X", y="Y"),
    brushId="dummy_brush", outputId="dummy_plot"
)

test_that(".process_selectby_choice works with a column-based brush", {
    plot_env <- new.env()
    cmds <- .processMultiSelections(pObjects$memory$ColumnDataPlot1, pObjects$memory, pObjects$contents, plot_env)
    expect_false(exists('col_selected', envir=plot_env))
    expect_identical(length(cmds), 0L)

    pObjects$memory$ReducedDimensionPlot1[[iSEE:::.brushData]] <- DUMMY_BRUSH
    plot_env <- new.env()
    cmds <- .processMultiSelections(pObjects$memory$ColumnDataPlot1, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('col_selected', envir=plot_env))
    expect_true(any(grepl("ReducedDimensionPlot1", unlist(cmds))))
    expect_true(any(grepl("shiny::brushedPoints", unlist(cmds))))

    pObjects$memory$ReducedDimensionPlot1[[iSEE:::.brushData]] <- list()
})

test_that(".process_selectby_choice works with a row-based brush", {
    plot_env <- new.env()
    cmds <- .processMultiSelections(pObjects$memory$RowDataPlot1, pObjects$memory, pObjects$contents, plot_env)
    expect_false(exists('row_selected', envir=plot_env))
    expect_identical(length(cmds), 0L)

    pObjects$memory$SampleAssayPlot1[[iSEE:::.brushData]] <- DUMMY_BRUSH
    plot_env <- new.env()
    cmds <- .processMultiSelections(pObjects$memory$RowDataPlot1, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('row_selected', envir=plot_env))
    expect_true(any(grepl("SampleAssayPlot1", unlist(cmds))))
    expect_true(any(grepl("shiny::brushedPoints", unlist(cmds))))

    pObjects$memory$SampleAssayPlot1[[iSEE:::.brushData]] <- list()
})

###############################################

DUMMY_LASSO <- list(lasso=NULL, closed=TRUE, panelvar1=NULL,
    panelvar2=NULL, mapping=list(x="X", y="Y"),
    coord=matrix(
        data=c(
            min(x_10), min(y_10),
            max(x_10), min(y_10),
            max(x_10), max(y_10),
            min(x_10), max(y_10),
            min(x_10), min(y_10)
        ),
        ncol=2,
        byrow=TRUE
    )
)

OPEN_LASSO <- DUMMY_LASSO
OPEN_LASSO$closed <- FALSE

test_that(".process_selectby_choice works with a column-based lasso", {
    plot_env <- new.env()
    pObjects$memory$ReducedDimensionPlot1[[iSEE:::.brushData]] <- OPEN_LASSO
    cmds <- .processMultiSelections(pObjects$memory$ColumnDataPlot1, pObjects$memory, pObjects$contents, plot_env)
    expect_false(exists('col_selected', envir=plot_env))
    expect_identical(length(cmds), 0L)

    pObjects$memory$ReducedDimensionPlot1[[iSEE:::.brushData]] <- DUMMY_LASSO
    plot_env <- new.env()
    cmds <- .processMultiSelections(pObjects$memory$ColumnDataPlot1, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('col_selected', envir=plot_env))
    expect_true(any(grepl("ReducedDimensionPlot1", unlist(cmds))))
    expect_true(any(grepl("iSEE::lassoPoints", unlist(cmds))))

    pObjects$memory$ReducedDimensionPlot1[[iSEE:::.brushData]] <- list()
})

test_that(".process_selectby_choice works with a row-based lasso", {
    plot_env <- new.env()
    pObjects$memory$SampleAssayPlot1[[iSEE:::.brushData]] <- OPEN_LASSO
    cmds <- .processMultiSelections(pObjects$memory$RowDataPlot1, pObjects$memory, pObjects$contents, plot_env)
    expect_false(exists('row_selected', envir=plot_env))
    expect_identical(length(cmds), 0L)

    pObjects$memory$SampleAssayPlot1[[iSEE:::.brushData]] <- DUMMY_LASSO
    plot_env <- new.env()
    cmds <- .processMultiSelections(pObjects$memory$RowDataPlot1, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('row_selected', envir=plot_env))
    expect_true(any(grepl("SampleAssayPlot1", unlist(cmds))))
    expect_true(any(grepl("iSEE::lassoPoints", unlist(cmds))))

    pObjects$memory$SampleAssayPlot1[[iSEE:::.brushData]] <- list()
})

###############################################

test_that(".process_selectby_choice works with saved column selections", {
    pObjects$memory$ReducedDimensionPlot1[[iSEE:::.multiSelectHistory]] <- list(DUMMY_BRUSH)
    cdp <- pObjects$memory$ColumnDataPlot1

    plot_env <- new.env()
    cmds <- .processMultiSelections(cdp, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('col_selected', envir=plot_env))
    expect_true(any(grepl("ReducedDimensionPlot1", unlist(cmds))))
    expect_true(any(grepl("shiny::brushedPoints", unlist(cmds))))

    pObjects$memory$ReducedDimensionPlot1[[iSEE:::.multiSelectHistory]] <- list()
})

test_that(".process_selectby_choice works with saved row selections", {
    pObjects$memory$SampleAssayPlot1[[iSEE:::.multiSelectHistory]] <- list(DUMMY_BRUSH)
    cdp <- pObjects$memory$RowDataPlot1

    plot_env <- new.env()
    cmds <- .processMultiSelections(cdp, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('row_selected', envir=plot_env))
    expect_true(any(grepl("SampleAssayPlot1", unlist(cmds))))
    expect_true(any(grepl("shiny::brushedPoints", unlist(cmds))))

    pObjects$memory$SampleAssayPlot1[[iSEE:::.multiSelectHistory]] <- list()
})

test_that(".any_saved_selection returns the appropriate value ", {
    x <- ReducedDimensionPlot()

    # Return whether there is at least one saved selection
    out <- iSEE:::.any_saved_selection(x, count = FALSE)
    expect_identical(out, FALSE)

    # Return the count of selections instead
    out <- iSEE:::.any_saved_selection(x, count = TRUE)
    expect_identical(out, 0L)
})

###############################################

test_that(".multiSelectionUpdateModes returns the appropriate values", {

    out <- .multiSelectionUpdateModes(ColumnDataPlot())
    expect_identical(out, iSEE:::.panelUpdatedSelectionColumn)

    out <- .multiSelectionUpdateModes(ColumnDataTable())
    expect_identical(out, iSEE:::.panelUpdatedSelectionColumn)

    out <- .multiSelectionUpdateModes(ComplexHeatmapPlot())
    expect_identical(out, character(0))

    out <- .multiSelectionUpdateModes(FeatureAssayPlot())
    expect_identical(out, iSEE:::.panelUpdatedSelectionColumn)

    out <- .multiSelectionUpdateModes(ReducedDimensionPlot())
    expect_identical(out, iSEE:::.panelUpdatedSelectionColumn)

    out <- .multiSelectionUpdateModes(RowDataPlot())
    expect_identical(out, iSEE:::.panelUpdatedSelectionRow)

    out <- .multiSelectionUpdateModes(RowDataTable())
    expect_identical(out, iSEE:::.panelUpdatedSelectionRow)

    out <- .multiSelectionUpdateModes(SampleAssayPlot())
    expect_identical(out, iSEE:::.panelUpdatedSelectionRow)

})

test_that(".multiSelectionRerenderModes returns the appropriate values for ComplexHeatmapPlot", {

    x <- ComplexHeatmapPlot(
        CustomRows = FALSE,             # default -> use incoming rows
        ShowColumnSelection = TRUE,     # default -> use incoming columns
        ColumnSelectionRestrict = FALSE # default
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, c(iSEE:::.panelUpdatedSelectionRow, iSEE:::.panelUpdatedSelectionColumn))

    x <- ComplexHeatmapPlot(
        CustomRows = TRUE,
        ShowColumnSelection = TRUE,     # default -> use incoming columns
        ColumnSelectionRestrict = FALSE # default
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, iSEE:::.panelUpdatedSelectionColumn)

    x <- ComplexHeatmapPlot(
        CustomRows = FALSE,             # default -> use incoming rows
        ShowColumnSelection = FALSE,
        ColumnSelectionRestrict = FALSE # default
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, iSEE:::.panelUpdatedSelectionRow)

    x <- ComplexHeatmapPlot(
        CustomRows = FALSE,             # default -> use incoming rows
        ShowColumnSelection = TRUE,     # default -> use incoming columns
        ColumnSelectionRestrict = TRUE  #         -> use incoming columns
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, c(iSEE:::.panelUpdatedSelectionRow, iSEE:::.panelUpdatedSelectionColumn))

    x <- ComplexHeatmapPlot(
        CustomRows = TRUE,
        ShowColumnSelection = FALSE,
        ColumnSelectionRestrict = FALSE
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, character(0))

})

test_that(".multiSelectionRerenderModes returns the appropriate values for ColumnDotPlot", {

    x <- ColumnDataPlot(
        ColumnSelectionRestrict = FALSE
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, character(0))

    x <- ColumnDataPlot(
        ColumnSelectionRestrict = TRUE
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, iSEE:::.panelUpdatedSelectionColumn)

})

test_that(".multiSelectionRerenderModes returns the appropriate values for RowDotPlot", {

    x <- RowDataPlot(
        RowSelectionRestrict = FALSE
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, character(0))

    x <- RowDataPlot(
        RowSelectionRestrict = TRUE
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, iSEE:::.panelUpdatedSelectionRow)

})

test_that(".multiSelectionRerenderModes returns the appropriate values for RowDataTable", {

    x <- RowDataTable(
        RowSelectionRestrict = FALSE
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, iSEE:::.panelUpdatedSelectionRow)

})

test_that(".multiSelectionRerenderModes returns the appropriate values for ColumnDataTable", {

    x <- ColumnDataTable(
        ColumnSelectionRestrict = FALSE
    )

    out <- .multiSelectionRerenderModes(x)
    expect_identical(out, iSEE:::.panelUpdatedSelectionColumn)

})
