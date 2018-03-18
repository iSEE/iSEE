
# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
featExprArgs <- featExprPlotDefaults(sce, 3)
rowStatArgs <- rowStatTableDefaults(sce, 3)
rowDataArgs <- rowDataPlotDefaults(sce, 1)
heatMapArgs <- heatMapPlotDefaults(sce, 1)

# Creating test links
redDimArgs[1,iSEE:::.colorByRowTable] <- "Row statistics table 1"
colDataArgs[2,iSEE:::.colorByRowTable] <- "Row statistics table 1"
colDataArgs[1,iSEE:::.colorByRowTable] <- "Row statistics table 3"

redDimArgs[1,iSEE:::.colorByField] <- 
    colDataArgs[1,iSEE:::.colorByField] <- 
    colDataArgs[2,iSEE:::.colorByField] <- 
    iSEE:::.colorByFeatNameTitle

featExprArgs[1,iSEE:::.featExprXAxisRowTable] <- "Row statistics table 2"
featExprArgs[2,iSEE:::.featExprYAxisRowTable] <- "Row statistics table 1"
featExprArgs[3,iSEE:::.featExprXAxisRowTable] <- "Row statistics table 1"
featExprArgs[3,iSEE:::.featExprYAxisRowTable] <- "Row statistics table 2"

featExprArgs[1,iSEE:::.featExprXAxis] <-
    featExprArgs[3,iSEE:::.featExprXAxis] <-
    iSEE:::.featExprXAxisFeatNameTitle

memory <- iSEE:::.setup_memory(sce, redDimArgs, colDataArgs, featExprArgs, rowStatArgs, rowDataArgs, heatMapArgs, 
        nrow(redDimArgs), nrow(colDataArgs), nrow(featExprArgs), nrow(rowStatArgs), nrow(rowDataArgs), nrow(heatMapArgs))
tabs <- iSEE:::.spawn_table_links(memory)

test_that("table link creation works correctly", {
    expect_identical(tabs$rowStatTable1$xaxis, "featExprPlot3")
    expect_identical(tabs$rowStatTable1$yaxis, "featExprPlot2")
    expect_identical(tabs$rowStatTable1$color, c("redDimPlot1", "colDataPlot2"))

    expect_identical(tabs$rowStatTable2$xaxis, "featExprPlot1")
    expect_identical(tabs$rowStatTable2$yaxis, "featExprPlot3")
    expect_identical(tabs$rowStatTable2$color, character(0))

    expect_identical(tabs$rowStatTable3$xaxis, character(0))
    expect_identical(tabs$rowStatTable3$yaxis, character(0))
    expect_identical(tabs$rowStatTable3$color, "colDataPlot1")

    # Disabling of xaxis choices should have no effect.
    featExprArgs2 <- featExprArgs
    featExprArgs2[1,iSEE:::.featExprXAxis] <- featExprArgs2[3,iSEE:::.featExprXAxis] <- iSEE:::.featExprXAxisNothingTitle
    memory <- list(redDimPlot=redDimArgs, featExprPlot=featExprArgs2, colDataPlot=colDataArgs, rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs)
    tab2 <- iSEE:::.spawn_table_links(memory)
    expect_identical(tabs, tab2)

    # Disabling of color choices should have no effect.
    colDataArgs[1,iSEE:::.colorByField] <-
        colDataArgs[2,iSEE:::.colorByField] <-
        redDimArgs[1,iSEE:::.colorByField] <- 
        iSEE:::.colorByNothingTitle
    memory <- list(redDimPlot=redDimArgs, featExprPlot=featExprArgs, colDataPlot=colDataArgs, rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs)
    tab2 <- iSEE:::.spawn_table_links(memory)
    expect_identical(tabs, tab2)

    # yaxis has no choices to disable, so we'll just change the selection.
    featExprArgs2 <- featExprArgs
    featExprArgs2[2:3,iSEE:::.featExprYAxisRowTable] <- iSEE:::.noSelection
    memory <- list(redDimPlot=redDimArgs, featExprPlot=featExprArgs2, colDataPlot=colDataArgs, rowStatTable=rowStatArgs, rowDataPlot=rowDataArgs)
    tab2 <- iSEE:::.spawn_table_links(memory)

    tabX <- tabs
    tabX$rowStatTable1$yaxis <- character(0)
    tabX$rowStatTable2$yaxis <- character(0)
    expect_identical(tabX, tab2)
})

test_that("table destruction works correctly", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    iSEE:::.destroy_table(pObjects, "rowStatTable1")
    expect_identical(pObjects$table_links$rowStatTable1, 
        list(color=character(0), xaxis=character(0), yaxis=character(0)))

    comp <- memory
    comp$redDimPlot[1,iSEE:::.colorByRowTable] <- iSEE:::.noSelection
    comp$colDataPlot[2,iSEE:::.colorByRowTable] <- iSEE:::.noSelection
    comp$featExprPlot[2,iSEE:::.featExprYAxisRowTable] <- iSEE:::.noSelection
    comp$featExprPlot[3,iSEE:::.featExprXAxisRowTable] <- iSEE:::.noSelection
    expect_identical(comp, pObjects$memory)

    # Destroying a simpler table.
    iSEE:::.destroy_table(pObjects, "rowStatTable3")
    expect_identical(pObjects$table_links$rowStatTable1, 
        list(color=character(0), xaxis=character(0), yaxis=character(0)))

    comp$colDataPlot[1,iSEE:::.colorByRowTable] <- iSEE:::.noSelection
    expect_identical(comp, pObjects$memory)
})

test_that("table modification works correctly", {
    # Changing colour:
    expect_true("redDimPlot1" %in% tabs$rowStatTable1$color)
    expect_false("redDimPlot1" %in% tabs$rowStatTable2$color)
    tab2 <- iSEE:::.modify_table_links(tabs, "redDimPlot1", "Row statistics table 2", "Row statistics table 1", mode = "color")
    expect_identical(tab2$rowStatTable1$color, setdiff(tabs$rowStatTable1$color, "redDimPlot1"))
    expect_identical(tab2$rowStatTable2$color, union(tabs$rowStatTable2$color, "redDimPlot1"))
    
    # Changing x-axis.
    expect_true("featExprPlot3" %in% tabs$rowStatTable1$xaxis)
    expect_false("featExprPlot3" %in% tabs$rowStatTable2$xaxis)
    tab2 <- iSEE:::.modify_table_links(tabs, "featExprPlot3", "Row statistics table 2", "Row statistics table 1", mode = "xaxis")
    expect_identical(tab2$rowStatTable1$xaxis, setdiff(tab2$rowStatTable1$xaxis, "featExprPlot3"))
    expect_identical(tab2$rowStatTable2$xaxis, union(tab2$rowStatTable2$xaxis, "featExprPlot3"))

    # Changing y-axis.
    expect_true("featExprPlot2" %in% tabs$rowStatTable1$yaxis)
    expect_false("featExprPlot2" %in% tabs$rowStatTable2$yaxis)
    tab2 <- iSEE:::.modify_table_links(tabs, "featExprPlot2", "Row statistics table 2", "Row statistics table 1", mode = "yaxis")
    expect_identical(tab2$rowStatTable1$yaxis, setdiff(tab2$rowStatTable1$yaxis, "featExprPlot2"))
    expect_identical(tab2$rowStatTable2$yaxis, union(tab2$rowStatTable2$yaxis, "featExprPlot2"))

    # Destroying links.
    tab2 <- iSEE:::.modify_table_links(tabs, "redDimPlot1", iSEE:::.noSelection, "Row statistics table 1", mode = "color")
    expect_identical(tab2$rowStatTable1$color, "colDataPlot2")

    tab2 <- iSEE:::.modify_table_links(tabs, "featExprPlot3", iSEE:::.noSelection, "Row statistics table 1", mode = "xaxis")
    expect_identical(tab2$rowStatTable1$xaxis, character(0))

    tab2 <- iSEE:::.modify_table_links(tabs, "featExprPlot2", iSEE:::.noSelection, "Row statistics table 1", mode = "yaxis")
    expect_identical(tab2$rowStatTable1$yaxis, character(0))

    # Destroying links that were never there has no effect.
    expect_false("featExprPlot2" %in% tabs$rowStatTable3$color)
    tab2 <- iSEE:::.modify_table_links(tabs, "featExprPlot2", iSEE:::.noSelection, "Row statistics table 3", mode = "color")
    expect_identical(tab2, tabs)

    # Adding links.
    expect_false("featExprPlot2" %in% tabs$rowStatTable3$color)
    tab2 <- iSEE:::.modify_table_links(tabs, "featExprPlot2", "Row statistics table 3", iSEE:::.noSelection, mode = "color")
    expect_identical(tab2$rowStatTable3$color, union(tab2$rowStatTable3$color, "featExprPlot2"))

    # Adding links already there has no effect.
    expect_true("featExprPlot2" %in% tabs$rowStatTable1$yaxis)
    tab2 <- iSEE:::.modify_table_links(tabs, "featExprPlot2", "Row statistics table 1", iSEE:::.noSelection, mode = "yaxis")
    expect_identical(tab2, tabs)
})

test_that("table observers work correctly", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    rObjects <- new.env()
    rObjects$redDimPlot1_PanelLinkInfo <- 1L
    rObjects$rowStatTable1_PanelLinkInfo <- 1L
    rObjects$rowStatTable2_PanelLinkInfo <- 1L

    input <- list(redDimPlot1_ColorBy=iSEE:::.colorByFeatNameTitle, 
                  redDimPlot1_ColorByRowTable="Row statistics table 2",
                  redDimPlot1_ColorByFeatName=pObjects$memory$redDimPlot[1,"ColorByFeatName"])

    # Changing the table.
    out <- iSEE:::.setup_table_observer("redDimPlot", 1, pObjects, rObjects, input=input, session = NULL, 
                                        iSEE:::.colorByField, iSEE:::.colorByFeatNameTitle, 
                                        iSEE:::.colorByFeatName, iSEE:::.colorByRowTable, 
                                        feat_choices = NULL, param = "color")

    expect_false(out) # doesn't actually change the feature.
    expect_false("redDimPlot1" %in% pObjects$table_links$rowStatTable1$color)
    expect_true("redDimPlot1" %in% pObjects$table_links$rowStatTable2$color)

    expect_identical(iSEE:::.colorByFeatNameTitle, pObjects$memory$redDimPlot[1, iSEE:::.colorByField])
    expect_identical("Row statistics table 2", pObjects$memory$redDimPlot[1, iSEE:::.colorByRowTable])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 2L)
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 2L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 2L)

    # Changing the colour source.
    old_table_links <- pObjects$table_links
    old_param <- pObjects$memory$redDimPlot[1,]
    old_param[,iSEE:::.colorByField] <- input$redDimPlot1_ColorBy <- iSEE:::.colorByNothingTitle

    out <- iSEE:::.setup_table_observer("redDimPlot", 1, pObjects, rObjects, input=input,  session=NULL, 
                                        iSEE:::.colorByField, iSEE:::.colorByFeatNameTitle, 
                                        iSEE:::.colorByFeatName, iSEE:::.colorByRowTable, 
                                        feat_choices = NULL, param = "color")

    expect_true(out)
    expect_identical(old_table_links, pObjects$table_links)
    expect_identical(old_param, pObjects$memory$redDimPlot[1,])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 3L) # Should trigger updates.
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 3L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 2L)

    # Changing the table while the colour source is not feat name.
    input$redDimPlot1_ColorByRowTable <- "Row statistics table 1"

    out <- iSEE:::.setup_table_observer("redDimPlot", 1, pObjects, rObjects, input=input, session=NULL, 
                                        iSEE:::.colorByField, iSEE:::.colorByFeatNameTitle, 
                                        iSEE:::.colorByFeatName, iSEE:::.colorByRowTable, 
                                        feat_choices = NULL, param = "color")

    expect_false(out) # plotting should NOT be re-triggered.
    expect_false("redDimPlot1" %in% pObjects$table_links$rowStatTable2$color) # old link to gene table 2 is removed.
    expect_true("redDimPlot1" %in% pObjects$table_links$rowStatTable1$color) 

    expect_identical(iSEE:::.colorByNothingTitle, pObjects$memory$redDimPlot[1, iSEE:::.colorByField])
    expect_identical("Row statistics table 1", pObjects$memory$redDimPlot[1, iSEE:::.colorByRowTable])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 3L) # Should NOT trigger updates.
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 3L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 2L)

    # Changing the feat name while the colour source is not feat name.
    old_table_links <- pObjects$table_links
    old_param <- pObjects$memory$redDimPlot[1,]
    input$redDimPlot1_ColorByFeatName <- input$redDimPlot1_ColorByFeatName + 1L

    out <- iSEE:::.setup_table_observer("redDimPlot", 1, pObjects, rObjects, input=input, session=NULL, 
                                        iSEE:::.colorByField, iSEE:::.colorByFeatNameTitle, 
                                        iSEE:::.colorByFeatName, iSEE:::.colorByRowTable, 
                                        feat_choices = NULL, param = "color")

    expect_false(out) # plotting should NOT be re-triggered.
    expect_identical(pObjects$table_links, old_table_links)
    expect_identical(old_param, pObjects$memory$redDimPlot[1,])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 3L) # Should NOT trigger updates.
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 3L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 2L)

    # Changing it back to gene table colouring.
    input$redDimPlot1_ColorBy <- iSEE:::.colorByFeatNameTitle

    out <- iSEE:::.setup_table_observer("redDimPlot", 1, pObjects, rObjects,input=input, session = NULL, 
                                        iSEE:::.colorByField, iSEE:::.colorByFeatNameTitle, 
                                        iSEE:::.colorByFeatName, iSEE:::.colorByRowTable,
                                        feat_choices = NULL, param = "color")

    expect_true(out)
    expect_true("redDimPlot1" %in% pObjects$table_links$rowStatTable1$color) # correctly triggers the update.

    expect_identical(iSEE:::.colorByFeatNameTitle, pObjects$memory$redDimPlot[1, iSEE:::.colorByField])
    expect_identical("Row statistics table 1", pObjects$memory$redDimPlot[1, iSEE:::.colorByRowTable])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 4L)
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 3L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 3L)

    # Turning off gene table colouring.
    input$redDimPlot1_ColorByRowTable <- iSEE:::.noSelection

    out <- iSEE:::.setup_table_observer("redDimPlot", 1, pObjects, rObjects, input=input, session = NULL, 
                                        iSEE:::.colorByField, iSEE:::.colorByFeatNameTitle, 
                                        iSEE:::.colorByFeatName, iSEE:::.colorByRowTable,
                                        feat_choices = NULL, param = "color")

    expect_false(out) # no change in the feature.
    expect_false("redDimPlot1" %in% pObjects$table_links$rowStatTable1$color) # correctly triggers the update.

    expect_identical(iSEE:::.colorByFeatNameTitle, pObjects$memory$redDimPlot[1, iSEE:::.colorByField])
    expect_identical(iSEE:::.noSelection, pObjects$memory$redDimPlot[1, iSEE:::.colorByRowTable])

    expect_identical(rObjects$redDimPlot1_PanelLinkInfo, 5L)
    expect_identical(rObjects$rowStatTable2_PanelLinkInfo, 3L)
    expect_identical(rObjects$rowStatTable1_PanelLinkInfo, 4L)
})

test_that("deleting table links is done correctly", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    # Deleting something with colours.
    expect_true("redDimPlot1" %in% tabs$rowStatTable1$color)

    iSEE:::.delete_table_links("redDimPlot", 1, pObjects)

    expect_false("redDimPlot1" %in% pObjects$table_links$rowStatTable1$color)
    expect_identical(pObjects$memory$redDimPlot[1, iSEE:::.colorByRowTable], iSEE:::.noSelection) 

    # Deleting something with x- and y-axis links.
    expect_true("featExprPlot3" %in% tabs$rowStatTable1$xaxis)
    expect_true("featExprPlot3" %in% tabs$rowStatTable2$yaxis)

    iSEE:::.delete_table_links("featExprPlot", 3, pObjects)

    expect_false("featExprPlot3" %in% pObjects$table_links$rowStatTable1$xaxis)
    expect_false("featExprPlot3" %in% pObjects$table_links$rowStatTable2$yaxis)
    expect_identical(pObjects$memory$featExprPlot[3, iSEE:::.featExprXAxisRowTable], iSEE:::.noSelection)
    expect_identical(pObjects$memory$featExprPlot[3, iSEE:::.featExprYAxisRowTable], iSEE:::.noSelection)
})
