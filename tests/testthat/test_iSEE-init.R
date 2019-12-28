# This script tests the code related to initialization utilities.
# library(iSEE); library(testthat); source("setup_sce.R"); source("test_iSEE-extras.R")

context("iSEE-extras")

memory <- list(
    RedDimPlot(),
    ColDataPlot(SelectColSource="RedDimPlot1", ColorBy="Feature name", ColorByRowTable="RowStatTable1"),
    ColDataPlot(SelectColSource="RedDimPlot1"),
    FeatAssayPlot(SelectColSource="ColDataPlot1"),
    FeatAssayPlot(SelectColSource="FeatAssayPlot1", XAxis="Feature name", 
        YAxisRowTable="RowStatTable1", XAxisRowTable="RowStatTable2"),
    RowStatTable(),
    RowStatTable(),
    SampAssayPlot(XAxis="Sample name", XAxisColTable="ColStatTable1"),
    ColStatTable()
)

test_that(".prepare_SE works correctly", {
    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)

    # Stores the colormap:
    expect_identical(ExperimentColorMap(), metadata(sce)$colormap)

    # Caches common information:
    expect_true(all(c("DotPlot", "ColumnDotPlot", "RedDimPlot", "RowStatTable", "RowDotPlot", "ColStatTable")
        %in% names(metadata(sce)$iSEE)))
})

test_that(".setup_initial_state works correctly", {
    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)
    init_out <- iSEE:::.setup_initial_state(sce, memory)

    memory2 <- init_out$memory

    # Filled in ID's make sense.
    expect_false(any(duplicated(names(memory2))))
    expect_identical(names(memory2), unname(vapply(memory2, .getEncodedName, "")))

    # Actually runs the refinement.
    expect_identical(memory2[["FeatAssayPlot1"]][["YAxisFeatName"]], rownames(sce)[1])
    expect_identical(memory2[["RedDimPlot1"]][["Type"]], "PCA")
    expect_identical(memory2[["ColDataPlot1"]][["YAxis"]], colnames(colData(sce))[1])
    expect_identical(memory2[["SampAssayPlot1"]][["YAxisSampName"]], colnames(sce)[1])

    # Counter makes sense.
    tab <- table(vapply(memory2, iSEE:::.encodedName, ""))
    expect_identical(unname(init_out$counter), as.integer(tab))
    expect_identical(names(init_out$counter), names(tab))

    # Counter and IDs respect user-specified IDs.
    memory[[5]][["PanelId"]] <- 100L
    init_out <- iSEE:::.setup_initial_state(sce, memory)
    expect_identical(init_out$counter[["FeatAssayPlot"]], 101L)
    expect_identical("FeatAssayPlot100", names(init_out$memory)[5])
    expect_identical("FeatAssayPlot101", names(init_out$memory)[4])

    # Refinement tosses out panels that can't exist.
    sce2 <- sce
    reducedDims(sce2) <- NULL
    metadata(sce2) <- list()
    sce2 <- iSEE:::.prepare_SE(sce2, ExperimentColorMap(), memory)
    init_out <- iSEE:::.setup_initial_state(sce2, memory)
    expect_false("RedDimPlot" %in% names(init_out$counter))
})

test_that(".define_reservoir works correctly", {
    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)
    init_out <- iSEE:::.setup_initial_state(sce, memory)

    # When we're adding an empty reservoir.
    res_out <- iSEE:::.define_reservoir(sce, list(), init_out$memory, init_out$counter)
    expect_identical(init_out$counter, res_out$counter)

    enc <- vapply(res_out$memory, iSEE:::.encodedName, "")
    expect_identical(init_out$reservoir, res_out$memory[!duplicated(enc)])

    # Adding a new panel class.
    res_out2 <- iSEE:::.define_reservoir(sce, list(RowDataPlot()), init_out$memory, init_out$counter)
    expect_identical(res_out2$counter, c(res_out$counter, RowDataPlot=0L))
    expect_identical(res_out2$reservoir, c(res_out$reservoir, list(RowDataPlot=.refineParameters(RowDataPlot(), sce))))

    # Multiple copies don't change the outcome.
    res_out3 <- iSEE:::.define_reservoir(sce, 
        list(RowDataPlot(), RowDataPlot(PanelHeight=1000L), ColDataPlot(), RedDimPlot(Type="TSNE")),
        init_out$memory, init_out$counter)
    expect_identical(res_out3, res_out2)
})

test_that("persistent object setup works as expected", {
    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)
    init_out <- iSEE:::.setup_initial_state(sce, memory)
    res_out <- iSEE:::.define_reservoir(sce, list(), init_out$memory, init_out$counter)
    pObjects <- iSEE:::.create_persistent_objects(init_out$memory, res_out$reservoir, res_out$counter)

    expect_identical(pObjects$memory, init_out$memory)
    expect_type(pObjects$commands, "list")
    expect_true(is(pObjects$selection_links, "igraph"))
})
