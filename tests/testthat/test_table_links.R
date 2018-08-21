# Tests the table linking functions for point-based plots.
# library(iSEE); library(testthat); source("setup_sce.R"); source("test_table_links.R")

# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
featAssayArgs <- featAssayPlotDefaults(sce, 3)
rowStatArgs <- rowStatTableDefaults(sce, 3)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
sampAssayArgs <- sampAssayPlotDefaults(sce, 3)
colStatArgs <- colStatTableDefaults(sce, 3)
customColArgs <- customDataPlotDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 1)

# Creating test links for column-based plots.
redDimArgs[1,.colorByRowTable] <- "Row statistics table 1"
colDataArgs[2,.colorByRowTable] <- "Row statistics table 1"
colDataArgs[1,.colorByRowTable] <- "Row statistics table 3"

redDimArgs[1,.colorByField] <-
    colDataArgs[1,.colorByField] <-
    colDataArgs[2,.colorByField] <-
    .colorByFeatNameTitle

# Creating test links for row-based plots.
rowDataArgs[1,.colorByColTable] <- "Column statistics table 1"
sampAssayArgs[2,.colorByColTable] <- "Column statistics table 1"
rowDataArgs[1,.colorByField] <- 
    sampAssayArgs[2,.colorByField] <- 
    .colorBySampNameTitle

# Creating test links for feature assay plots.
featAssayArgs[1,.featAssayXAxisRowTable] <- "Row statistics table 2"
featAssayArgs[2,.featAssayYAxisRowTable] <- "Row statistics table 1"
featAssayArgs[3,.featAssayXAxisRowTable] <- "Row statistics table 1"
featAssayArgs[3,.featAssayYAxisRowTable] <- "Row statistics table 2"

featAssayArgs[1,.featAssayXAxis] <-
    featAssayArgs[3,.featAssayXAxis] <-
    .featAssayXAxisFeatNameTitle

# Creating test links for sample assay plots.
sampAssayArgs[1,.sampAssayXAxisColTable] <- "Column statistics table 2"
sampAssayArgs[2,.sampAssayYAxisColTable] <- "Column statistics table 1"
sampAssayArgs[3,.sampAssayXAxisColTable] <- "Column statistics table 1"
sampAssayArgs[3,.sampAssayYAxisColTable] <- "Column statistics table 2"

sampAssayArgs[1,.sampAssayXAxis] <-
    sampAssayArgs[3,.sampAssayXAxis] <-
    .sampAssayXAxisSampNameTitle

# Adding row names to mimic .setup_memory().
# We don't actually want to run that function, though,
# as the number of customColPlots will be set to zero.
rownames(redDimArgs) <- sprintf("redDimPlot%i", seq_len(nrow(redDimArgs)))
rownames(colDataArgs) <- sprintf("colDataPlot%i", seq_len(nrow(colDataArgs)))
rownames(featAssayArgs) <- sprintf("featAssayPlot%i", seq_len(nrow(featAssayArgs)))
rownames(rowStatArgs) <- sprintf("rowStatTable%i", seq_len(nrow(rowStatArgs)))
rownames(rowDataArgs) <- sprintf("rowDataPlot%i", seq_len(nrow(rowDataArgs)))
rownames(sampAssayArgs) <- sprintf("sampAssayPlot%i", seq_len(nrow(sampAssayArgs)))
rownames(colStatArgs) <- sprintf("colStatTable%i", seq_len(nrow(colStatArgs)))
rownames(customColArgs) <- sprintf("customColPlot%i", seq_len(nrow(customColArgs)))
rownames(heatMapArgs) <- sprintf("heatMapPlot%i", seq_len(nrow(heatMapArgs)))

# Setting up the memory.
memory <- list(
    redDimPlot=redDimArgs,
    colDataPlot=colDataArgs,
    featAssayPlot=featAssayArgs,
    rowStatTable=rowStatArgs,
    rowDataPlot=rowDataArgs,
    sampAssayPlot=sampAssayArgs,
    colStatTable=colStatArgs,
    customColPlot=customColArgs,
    heatMapPlot=heatMapArgs)
tabs <- .spawn_table_links(memory)

test_that("table link creation works correctly", {
    expect_identical(tabs$rowStatTable1$xaxis, "featAssayPlot3")
    expect_identical(tabs$rowStatTable1$yaxis, "featAssayPlot2")
    expect_identical(tabs$rowStatTable1$color, c("redDimPlot1", "colDataPlot2"))

    expect_identical(tabs$rowStatTable2$xaxis, "featAssayPlot1")
    expect_identical(tabs$rowStatTable2$yaxis, "featAssayPlot3")
    expect_identical(tabs$rowStatTable2$color, character(0))

    expect_identical(tabs$rowStatTable3$xaxis, character(0))
    expect_identical(tabs$rowStatTable3$yaxis, character(0))
    expect_identical(tabs$rowStatTable3$color, "colDataPlot1")

    expect_identical(tabs$colStatTable1$xaxis, "sampAssayPlot3")
    expect_identical(tabs$colStatTable1$yaxis, "sampAssayPlot2")
    expect_identical(tabs$colStatTable1$color, c("rowDataPlot1", "sampAssayPlot2"))

    expect_identical(tabs$colStatTable2$xaxis, "sampAssayPlot1")
    expect_identical(tabs$colStatTable2$yaxis, "sampAssayPlot3")
    expect_identical(tabs$colStatTable2$color, character(0))

    # Disabling of xaxis choices should have no effect, it's still linked but hidden.
    featAssayArgs2 <- featAssayArgs
    featAssayArgs2[c(1, 3),.featAssayXAxis] <- .featAssayXAxisNothingTitle
    memory <- list(
        redDimPlot=redDimArgs,
        colDataPlot=colDataArgs,
        featAssayPlot=featAssayArgs2,
        rowStatTable=rowStatArgs,
        rowDataPlot=rowDataArgs,
        sampAssayPlot=sampAssayArgs,
        colStatTable=colStatArgs,
        customColPlot=customColArgs,
        heatMapPlot=heatMapArgs)
    tab2 <- .spawn_table_links(memory)
    expect_identical(tabs, tab2)

    sampAssayArgs2 <- sampAssayArgs
    sampAssayArgs2[c(1, 3),.sampAssayXAxis] <- .sampAssayXAxisNothingTitle
    memory <- list(redDimPlot=redDimArgs, 
                   featAssayPlot=featAssayArgs, 
                   colDataPlot=colDataArgs, 
                   rowStatTable=rowStatArgs, 
                   rowDataPlot=rowDataArgs,
                   sampAssayPlot=sampAssayArgs2,
                   colStatTable=colStatArgs,
                   customColPlot=customColArgs,
                   heatMapPlot=heatMapArgs)
    tab2 <- .spawn_table_links(memory)
    expect_identical(tabs, tab2)

    # Disabling of color choices should have no effect, it's still linked but hidden.
    colDataArgs2 <- colDataArgs
    colDataArgs2[1:2,.colorByField] <- .colorByNothingTitle
    redDimArgs2 <- redDimArgs
    redDimArgs2[1,.colorByField] <- .colorByNothingTitle
    memory <- list(redDimPlot=redDimArgs2, 
                   featAssayPlot=featAssayArgs, 
                   colDataPlot=colDataArgs2, 
                   rowStatTable=rowStatArgs, 
                   rowDataPlot=rowDataArgs,
                   sampAssayPlot=sampAssayArgs,
                   colStatTable=colStatArgs,
                   customColPlot=customColArgs,
                   heatMapPlot=heatMapArgs)
    tab2 <- .spawn_table_links(memory)
    expect_identical(tabs, tab2)

    rowDataArgs2 <- rowDataArgs
    sampAssayArgs2 <- sampAssayArgs
    rowDataArgs2[1,.colorByField] <-  .colorByNothingTitle
    sampAssayArgs2[2,.colorByField] <- .colorByNothingTitle
    memory <- list(redDimPlot=redDimArgs, 
                   featAssayPlot=featAssayArgs, 
                   colDataPlot=colDataArgs, 
                   rowStatTable=rowStatArgs, 
                   rowDataPlot=rowDataArgs2,
                   sampAssayPlot=sampAssayArgs2,
                   colStatTable=colStatArgs,
                   customColPlot=customColArgs,
                   heatMapPlot=heatMapArgs)
    tab2 <- .spawn_table_links(memory)
    expect_identical(tabs, tab2)

    # yaxis has no choices to disable, so we'll just change the selection.
    featAssayArgs2 <- featAssayArgs
    featAssayArgs2[2:3,.featAssayYAxisRowTable] <- .noSelection
    memory <- list(
        redDimPlot=redDimArgs,
        colDataPlot=colDataArgs,
        featAssayPlot=featAssayArgs2,
        rowStatTable=rowStatArgs,
        rowDataPlot=rowDataArgs,
        sampAssayPlot=sampAssayArgs,
        colStatTable=colStatArgs,
        customColPlot=customColArgs,
        heatMapPlot=heatMapArgs)
    tab2 <- .spawn_table_links(memory)

    tabX <- tabs
    tabX$rowStatTable1$yaxis <- character(0)
    tabX$rowStatTable2$yaxis <- character(0)
    expect_identical(tabX, tab2)

    sampAssayArgs2 <- sampAssayArgs
    sampAssayArgs2[2:3,.sampAssayYAxisColTable] <- .noSelection
    memory <- list(redDimPlot=redDimArgs, 
                   featAssayPlot=featAssayArgs, 
                   colDataPlot=colDataArgs, 
                   rowStatTable=rowStatArgs, 
                   rowDataPlot=rowDataArgs,
                   sampAssayPlot=sampAssayArgs2,
                   colStatTable=colStatArgs,
                   customColPlot=customColArgs,
                   heatMapPlot=heatMapArgs)
    tab2 <- .spawn_table_links(memory)

    tabX <- tabs
    tabX$colStatTable1$yaxis <- character(0)
    tabX$colStatTable2$yaxis <- character(0)
    expect_identical(tabX, tab2)
})

test_that("table destruction works correctly for rows", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    # Destroying a row statistics table.
    .destroy_table(pObjects, "rowStatTable1")
    expect_identical(pObjects$table_links$rowStatTable1,
        list(color=character(0), xaxis=character(0), yaxis=character(0)))

    comp <- memory
    comp$redDimPlot[1,.colorByRowTable] <- .noSelection
    comp$colDataPlot[2,.colorByRowTable] <- .noSelection
    comp$featAssayPlot[2,.featAssayYAxisRowTable] <- .noSelection
    comp$featAssayPlot[3,.featAssayXAxisRowTable] <- .noSelection
    expect_identical(comp, pObjects$memory)

    # Destroying a simpler table.
    .destroy_table(pObjects, "rowStatTable3")
    expect_identical(pObjects$table_links$rowStatTable3,
        list(color=character(0), xaxis=character(0), yaxis=character(0)))

    comp$colDataPlot[1,.colorByRowTable] <- .noSelection
    expect_identical(comp, pObjects$memory)
})

test_that("table destruction works correctly for columns", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    # Destroying a column statistics table.
    .destroy_table(pObjects, "colStatTable1")
    expect_identical(pObjects$table_links$colStatTable1, 
        list(color=character(0), xaxis=character(0), yaxis=character(0)))

    comp <- memory
    comp$rowDataPlot[1,.colorByColTable] <- .noSelection
    comp$sampAssayPlot[2,.colorByColTable] <- .noSelection
    comp$sampAssayPlot[2,.sampAssayYAxisColTable] <- .noSelection
    comp$sampAssayPlot[3,.sampAssayXAxisColTable] <- .noSelection
    expect_identical(comp, pObjects$memory)

    # Destroying a simpler table.
    .destroy_table(pObjects, "colStatTable2")
    expect_identical(pObjects$table_links$colStatTable2, 
        list(color=character(0), xaxis=character(0), yaxis=character(0)))

    comp$sampAssayPlot[1,.sampAssayXAxisColTable] <- .noSelection
    comp$sampAssayPlot[3,.sampAssayYAxisColTable] <- .noSelection
    expect_identical(comp, pObjects$memory)
})

test_that("table modification works correctly (row/column-agnostic)", {
    # Changing colour:
    expect_true("redDimPlot1" %in% tabs$rowStatTable1$color)
    expect_false("redDimPlot1" %in% tabs$rowStatTable2$color)
    tab2 <- .modify_table_links(tabs, "redDimPlot1", "Row statistics table 2", "Row statistics table 1", mode="color")
    expect_identical(tab2$rowStatTable1$color, setdiff(tabs$rowStatTable1$color, "redDimPlot1"))
    expect_identical(tab2$rowStatTable2$color, union(tabs$rowStatTable2$color, "redDimPlot1"))

    # Changing x-axis.
    expect_true("featAssayPlot3" %in% tabs$rowStatTable1$xaxis)
    expect_false("featAssayPlot3" %in% tabs$rowStatTable2$xaxis)
    tab2 <- .modify_table_links(tabs, "featAssayPlot3", "Row statistics table 2", "Row statistics table 1", mode="xaxis")
    expect_identical(tab2$rowStatTable1$xaxis, setdiff(tab2$rowStatTable1$xaxis, "featAssayPlot3"))
    expect_identical(tab2$rowStatTable2$xaxis, union(tab2$rowStatTable2$xaxis, "featAssayPlot3"))

    # Changing y-axis.
    expect_true("featAssayPlot2" %in% tabs$rowStatTable1$yaxis)
    expect_false("featAssayPlot2" %in% tabs$rowStatTable2$yaxis)
    tab2 <- .modify_table_links(tabs, "featAssayPlot2", "Row statistics table 2", "Row statistics table 1", mode="yaxis")
    expect_identical(tab2$rowStatTable1$yaxis, setdiff(tab2$rowStatTable1$yaxis, "featAssayPlot2"))
    expect_identical(tab2$rowStatTable2$yaxis, union(tab2$rowStatTable2$yaxis, "featAssayPlot2"))

    # Destroying links.
    tab2 <- .modify_table_links(tabs, "redDimPlot1", .noSelection, "Row statistics table 1", mode="color")
    expect_identical(tab2$rowStatTable1$color, "colDataPlot2")

    tab2 <- .modify_table_links(tabs, "featAssayPlot3", .noSelection, "Row statistics table 1", mode="xaxis")
    expect_identical(tab2$rowStatTable1$xaxis, character(0))

    tab2 <- .modify_table_links(tabs, "featAssayPlot2", .noSelection, "Row statistics table 1", mode="yaxis")
    expect_identical(tab2$rowStatTable1$yaxis, character(0))

    # Destroying links that were never there has no effect.
    expect_false("featAssayPlot2" %in% tabs$rowStatTable3$color)
    tab2 <- .modify_table_links(tabs, "featAssayPlot2", .noSelection, "Row statistics table 3", mode="color")
    expect_identical(tab2, tabs)

    # Adding links.
    expect_false("featAssayPlot2" %in% tabs$rowStatTable3$color)
    tab2 <- .modify_table_links(tabs, "featAssayPlot2", "Row statistics table 3", .noSelection, mode="color")
    expect_identical(tab2$rowStatTable3$color, union(tab2$rowStatTable3$color, "featAssayPlot2"))

    # Adding links already there has no effect.
    expect_true("featAssayPlot2" %in% tabs$rowStatTable1$yaxis)
    tab2 <- .modify_table_links(tabs, "featAssayPlot2", "Row statistics table 1", .noSelection, mode="yaxis")
    expect_identical(tab2, tabs)
})

test_that("table observers work correctly (row/column-agnostic)", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    rObjects <- new.env()
    rObjects$redDimPlot1_PanelLinkInfo <- 1L
    rObjects$rowStatTable1_PanelLinkInfo <- 1L
    rObjects$rowStatTable2_PanelLinkInfo <- 1L

    input <- list(
        redDimPlot1_ColorBy=.colorByFeatNameTitle,
        redDimPlot1_ColorByRowTable="Row statistics table 2",
        redDimPlot1_ColorByFeatName=pObjects$memory$redDimPlot[1,"ColorByFeatName"])

    # Changing the table.
    out <- .setup_table_observer(
        "redDimPlot", 1, pObjects, rObjects, input=input, session=NULL,
        .colorByField, .colorByFeatNameTitle,
        .colorByFeatName, .colorByRowTable,
        select_choices=NULL, param="color")

    expect_false(out) # doesn't actually change the feature.
    expect_false("redDimPlot1" %in% pObjects$table_links$rowStatTable1$color)
    expect_true("redDimPlot1" %in% pObjects$table_links$rowStatTable2$color)

    expect_identical(.colorByFeatNameTitle, pObjects$memory$redDimPlot[1, .colorByField])
    expect_identical("Row statistics table 2", pObjects$memory$redDimPlot[1, .colorByRowTable])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 2L)
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 2L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 2L)

    # Changing the colour source.
    old_table_links <- pObjects$table_links
    old_param <- pObjects$memory$redDimPlot[1,]
    old_param[,.colorByField] <-
        input$redDimPlot1_ColorBy <-
        .colorByNothingTitle

    out <- .setup_table_observer(
        "redDimPlot", 1, pObjects, rObjects, input=input,  session=NULL,
        .colorByField, .colorByFeatNameTitle,
        .colorByFeatName, .colorByRowTable,
        select_choices=NULL, param="color")

    expect_true(out)
    expect_identical(old_table_links, pObjects$table_links)
    expect_identical(old_param, pObjects$memory$redDimPlot[1,])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 3L) # Should trigger updates.
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 3L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 2L)

    # Changing the table while the colour source is not feat name.
    input$redDimPlot1_ColorByRowTable <- "Row statistics table 1"

    out <- .setup_table_observer(
        "redDimPlot", 1, pObjects, rObjects, input=input, session=NULL,
        .colorByField, .colorByFeatNameTitle,
        .colorByFeatName, .colorByRowTable,
        select_choices=NULL, param="color")

    expect_false(out) # plotting should NOT be re-triggered.
    expect_false("redDimPlot1" %in% pObjects$table_links$rowStatTable2$color) # old link to gene table 2 is removed.
    expect_true("redDimPlot1" %in% pObjects$table_links$rowStatTable1$color)

    expect_identical(.colorByNothingTitle, pObjects$memory$redDimPlot[1, .colorByField])
    expect_identical("Row statistics table 1", pObjects$memory$redDimPlot[1, .colorByRowTable])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 3L) # Should NOT trigger updates.
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 3L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 2L)

    # Changing the feat name while the colour source is not feat name.
    old_table_links <- pObjects$table_links
    old_param <- pObjects$memory$redDimPlot[1,]
    input$redDimPlot1_ColorByFeatName <- input$redDimPlot1_ColorByFeatName + 1L

    out <- .setup_table_observer(
        "redDimPlot", 1, pObjects, rObjects, input=input, session=NULL,
        .colorByField, .colorByFeatNameTitle,
        .colorByFeatName, .colorByRowTable,
        select_choices=NULL, param="color")

    expect_false(out) # plotting should NOT be re-triggered.
    expect_identical(pObjects$table_links, old_table_links)
    expect_identical(old_param, pObjects$memory$redDimPlot[1,])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 3L) # Should NOT trigger updates.
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 3L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 2L)

    # Changing it back to gene table colouring.
    input$redDimPlot1_ColorBy <- .colorByFeatNameTitle

    out <- .setup_table_observer(
        "redDimPlot", 1, pObjects, rObjects,input=input, session=NULL,
        .colorByField, .colorByFeatNameTitle,
        .colorByFeatName, .colorByRowTable,
        select_choices=NULL, param="color")

    expect_true(out)
    expect_true("redDimPlot1" %in% pObjects$table_links$rowStatTable1$color) # correctly triggers the update.

    expect_identical(.colorByFeatNameTitle, pObjects$memory$redDimPlot[1, .colorByField])
    expect_identical("Row statistics table 1", pObjects$memory$redDimPlot[1, .colorByRowTable])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 4L)
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 3L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 3L)

    # Turning off gene table colouring.
    input$redDimPlot1_ColorByRowTable <- .noSelection

    out <- .setup_table_observer(
        "redDimPlot", 1, pObjects, rObjects, input=input, session=NULL,
        .colorByField, .colorByFeatNameTitle,
        .colorByFeatName, .colorByRowTable,
        select_choices=NULL, param="color")

    expect_false(out) # no change in the feature.
    expect_false("redDimPlot1" %in% pObjects$table_links$rowStatTable1$color) # correctly triggers the update.

    expect_identical(.colorByFeatNameTitle, pObjects$memory$redDimPlot[1, .colorByField])
    expect_identical(.noSelection, pObjects$memory$redDimPlot[1, .colorByRowTable])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 5L)
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 3L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 4L)
})

test_that("deleting table links is done correctly for row-based plots", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    # Deleting something with colours.
    expect_true("redDimPlot1" %in% tabs$rowStatTable1$color)

    .delete_table_links("redDimPlot", 1, pObjects)

    expect_false("redDimPlot1" %in% pObjects$table_links$rowStatTable1$color)
    expect_identical(pObjects$memory$redDimPlot[1, .colorByRowTable], .noSelection)

    # Deleting something with x- and y-axis links.
    expect_true("featAssayPlot3" %in% tabs$rowStatTable1$xaxis)
    expect_true("featAssayPlot3" %in% tabs$rowStatTable2$yaxis)

    .delete_table_links("featAssayPlot", 3, pObjects)

    expect_false("featAssayPlot3" %in% pObjects$table_links$rowStatTable1$xaxis)
    expect_false("featAssayPlot3" %in% pObjects$table_links$rowStatTable2$yaxis)
    expect_identical(pObjects$memory$featAssayPlot[3, .featAssayXAxisRowTable], .noSelection)
    expect_identical(pObjects$memory$featAssayPlot[3, .featAssayYAxisRowTable], .noSelection)
})

test_that("deleting table links is done correctly for column-based plots", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    # Deleting something with colours.
    expect_true("rowDataPlot1" %in% tabs$colStatTable1$color)

    .delete_table_links("rowDataPlot", 1, pObjects)

    expect_false("rowDataPlot1" %in% pObjects$table_links$colStatTable1$color)
    expect_identical(pObjects$memory$rowDataPlot[1, .colorByColTable], .noSelection) 

    # Deleting something with x- and y-axis links.
    expect_true("sampAssayPlot3" %in% tabs$colStatTable1$xaxis)
    expect_true("sampAssayPlot3" %in% tabs$colStatTable2$yaxis)

    .delete_table_links("sampAssayPlot", 3, pObjects)

    expect_false("sampAssayPlot3" %in% pObjects$table_links$colStatTable1$xaxis)
    expect_false("sampAssayPlot3" %in% pObjects$table_links$colStatTable2$yaxis)
    expect_identical(pObjects$memory$sampAssayPlot[3, .sampAssayXAxisColTable], .noSelection)
    expect_identical(pObjects$memory$sampAssayPlot[3, .sampAssayYAxisColTable], .noSelection)
})

