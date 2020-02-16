# This tests the multiple selection machinery.
# library(testthat); library(iSEE); source("setup_sce.R"); source("setup_other.R"); source("test_multi_select.R")

# Setting up a chain of plots.
memory <- list(
    ReducedDimensionPlot(ColorByRowTable="RowDataTable1", ColorBy="Feature name"),
    ColumnDataPlot(SelectColSource="ReducedDimensionPlot1"),
    ColumnDataPlot(SelectColSource="ReducedDimensionPlot1"),
    RowAssayPlot(SelectColSource="ColumnDataPlot1"),
    RowAssayPlot(SelectColSource="RowAssayPlot1", YAxisRowTable="RowDataTable1"),
    RowDataPlot(SelectRowSource="ColumnAssayPlot1"),
    ColumnAssayPlot(),
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

    pObjects$memory$ColumnAssayPlot1[[iSEE:::.brushData]] <- DUMMY_BRUSH
    plot_env <- new.env()
    cmds <- .processMultiSelections(pObjects$memory$RowDataPlot1, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('row_selected', envir=plot_env))
    expect_true(any(grepl("ColumnAssayPlot1", unlist(cmds))))
    expect_true(any(grepl("shiny::brushedPoints", unlist(cmds))))

    pObjects$memory$ColumnAssayPlot1[[iSEE:::.brushData]] <- list()
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
    pObjects$memory$ColumnAssayPlot1[[iSEE:::.brushData]] <- OPEN_LASSO
    cmds <- .processMultiSelections(pObjects$memory$RowDataPlot1, pObjects$memory, pObjects$contents, plot_env)
    expect_false(exists('row_selected', envir=plot_env))
    expect_identical(length(cmds), 0L)

    pObjects$memory$ColumnAssayPlot1[[iSEE:::.brushData]] <- DUMMY_LASSO
    plot_env <- new.env()
    cmds <- .processMultiSelections(pObjects$memory$RowDataPlot1, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('row_selected', envir=plot_env))
    expect_true(any(grepl("ColumnAssayPlot1", unlist(cmds))))
    expect_true(any(grepl("iSEE::lassoPoints", unlist(cmds))))

    pObjects$memory$ColumnAssayPlot1[[iSEE:::.brushData]] <- list()
})

###############################################

test_that(".process_selectby_choice works with saved column selections", {
    pObjects$memory$ReducedDimensionPlot1[[iSEE:::.multiSelectHistory]] <- list(DUMMY_BRUSH)
    cdp <- pObjects$memory$ColumnDataPlot1

    # No response when still looking for the active brush.
    plot_env <- new.env()
    cmds <- .processMultiSelections(cdp, pObjects$memory, pObjects$contents, plot_env)
    expect_false(exists('col_selected', envir=plot_env))
    expect_identical(length(cmds), 0L)

    # Responds after asking for the union.
    cdp[[iSEE:::.selectColType]] <- iSEE:::.selectMultiUnionTitle
    plot_env <- new.env()
    cmds <- .processMultiSelections(cdp, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('col_selected', envir=plot_env))
    expect_true(any(grepl("ReducedDimensionPlot1", unlist(cmds))))
    expect_true(any(grepl("shiny::brushedPoints", unlist(cmds))))

    # No response after asking for save... until we specify which saved element we want.
    plot_env <- new.env()
    cdp[[iSEE:::.selectColType]] <- iSEE:::.selectMultiSavedTitle
    cmds <- .processMultiSelections(cdp, pObjects$memory, pObjects$contents, plot_env)
    expect_false(exists('col_selected', envir=plot_env))
    expect_identical(length(cmds), 0L)

    cdp[[iSEE:::.selectColSaved]] <- 1L
    plot_env <- new.env()
    cmds <- .processMultiSelections(cdp, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('col_selected', envir=plot_env))
    expect_true(any(grepl("ReducedDimensionPlot1", unlist(cmds))))
    expect_true(any(grepl("shiny::brushedPoints", unlist(cmds))))

    pObjects$memory$ReducedDimensionPlot1[[iSEE:::.brushData]] <- list()
})

test_that(".process_selectby_choice works with saved row selections", {
    pObjects$memory$ColumnAssayPlot1[[iSEE:::.multiSelectHistory]] <- list(DUMMY_BRUSH)
    cdp <- pObjects$memory$RowDataPlot1

    # No response when still looking for the active brush.
    plot_env <- new.env()
    cmds <- .processMultiSelections(cdp, pObjects$memory, pObjects$contents, plot_env)
    expect_false(exists('row_selected', envir=plot_env))
    expect_identical(length(cmds), 0L)

    # Responds after asking for the union.
    cdp[[iSEE:::.selectRowType]] <- iSEE:::.selectMultiUnionTitle
    plot_env <- new.env()
    cmds <- .processMultiSelections(cdp, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('row_selected', envir=plot_env))
    expect_true(any(grepl("ColumnAssayPlot1", unlist(cmds))))
    expect_true(any(grepl("shiny::brushedPoints", unlist(cmds))))

    # No response after asking for save... until we specify which saved element we want.
    plot_env <- new.env()
    cdp[[iSEE:::.selectRowType]] <- iSEE:::.selectMultiSavedTitle
    cmds <- .processMultiSelections(cdp, pObjects$memory, pObjects$contents, plot_env)
    expect_false(exists('row_selected', envir=plot_env))
    expect_identical(length(cmds), 0L)

    cdp[[iSEE:::.selectRowSaved]] <- 1L
    plot_env <- new.env()
    cmds <- .processMultiSelections(cdp, pObjects$memory, pObjects$contents, plot_env)

    expect_true(exists('row_selected', envir=plot_env))
    expect_true(any(grepl("ColumnAssayPlot1", unlist(cmds))))
    expect_true(any(grepl("shiny::brushedPoints", unlist(cmds))))

    pObjects$memory$ColumnAssayPlot1[[iSEE:::.brushData]] <- list()
})

test_that(".any_saved_selection returns the appropriate value ", {

  x <- ReducedDimensionPlot()

  # Return whether there is at least one saved selection
  out <- .any_saved_selection(x, count = FALSE)
  expect_identical(out, FALSE)

  # Return the count of selections instead
  out <- .any_saved_selection(x, count = TRUE)
  expect_identical(out, 0L)

})

