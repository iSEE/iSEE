
# Do NOT move to setup; re-defined here to keep tests self-contained.
redDimArgs <- redDimPlotDefaults(sce, 1)
colDataArgs <- colDataPlotDefaults(sce, 2)
geneExprArgs <- geneExprPlotDefaults(sce, 3)
geneStatArgs <- geneStatTableDefaults(sce, 3)

# Creating test links
redDimArgs[1,iSEE:::.colorByGeneTable] <- "Gene statistics table 1"
colDataArgs[2,iSEE:::.colorByGeneTable] <- "Gene statistics table 1"
colDataArgs[1,iSEE:::.colorByGeneTable] <- "Gene statistics table 3"

redDimArgs[1,iSEE:::.colorByField] <- 
    colDataArgs[1,iSEE:::.colorByField] <- 
    colDataArgs[2,iSEE:::.colorByField] <- 
    iSEE:::.colorByGeneTableTitle

geneExprArgs[1,iSEE:::.geneExprXAxisGeneTable] <- "Gene statistics table 2"
geneExprArgs[2,iSEE:::.geneExprYAxisGeneTable] <- "Gene statistics table 1"
geneExprArgs[3,iSEE:::.geneExprXAxisGeneTable] <- "Gene statistics table 1"
geneExprArgs[3,iSEE:::.geneExprYAxisGeneTable] <- "Gene statistics table 2"

geneExprArgs[1,iSEE:::.geneExprXAxis] <-
    geneExprArgs[3,iSEE:::.geneExprXAxis] <-
    iSEE:::.geneExprXAxisGeneTableTitle

memory <- iSEE:::.setup_memory(sce, redDimArgs, colDataArgs, geneExprArgs, geneStatArgs,
        nrow(redDimArgs), nrow(colDataArgs), nrow(geneExprArgs), nrow(geneStatArgs))
tabs <- iSEE:::.spawn_table_links(memory)

test_that("table link creation works correctly", {
    expect_identical(tabs$geneStatTable1$xaxis, "geneExprPlot3")
    expect_identical(tabs$geneStatTable1$yaxis, "geneExprPlot2")
    expect_identical(tabs$geneStatTable1$color, c("redDimPlot1", "colDataPlot2"))

    expect_identical(tabs$geneStatTable2$xaxis, "geneExprPlot1")
    expect_identical(tabs$geneStatTable2$yaxis, "geneExprPlot3")
    expect_identical(tabs$geneStatTable2$color, character(0))

    expect_identical(tabs$geneStatTable3$xaxis, character(0))
    expect_identical(tabs$geneStatTable3$yaxis, character(0))
    expect_identical(tabs$geneStatTable3$color, "colDataPlot1")

    # Checking what happens when xaxis choices are disabled.
    geneExprArgs2 <- geneExprArgs
    geneExprArgs2[1,iSEE:::.geneExprXAxis] <- geneExprArgs2[3,iSEE:::.geneExprXAxis] <- iSEE:::.geneExprXAxisNothingTitle
    memory <- list(redDim=redDimArgs, geneExpr=geneExprArgs2, colData=colDataArgs, geneStat=geneStatArgs)
    tab2 <- iSEE:::.spawn_table_links(memory)

    tabX <- tabs
    tabX$geneStatTable1$xaxis <- character(0)
    tabX$geneStatTable2$xaxis <- character(0)
    expect_identical(tabX, tab2)

    # Checking what happens when xaxis choices are disabled.
    geneExprArgs2 <- geneExprArgs
    geneExprArgs2[2,iSEE:::.geneExprYAxis] <- geneExprArgs2[3,iSEE:::.geneExprYAxis] <- iSEE:::.geneExprYAxisGeneTextTitle
    memory <- list(redDim=redDimArgs, geneExpr=geneExprArgs2, colData=colDataArgs, geneStat=geneStatArgs)
    tab2 <- iSEE:::.spawn_table_links(memory)

    tabX <- tabs
    tabX$geneStatTable1$yaxis <- character(0)
    tabX$geneStatTable2$yaxis <- character(0)
    expect_identical(tabX, tab2)

    # Checking what happens when y-axis choices are disabled.
    colDataArgs[1,iSEE:::.colorByField] <-
        colDataArgs[2,iSEE:::.colorByField] <-
        redDimArgs[1,iSEE:::.colorByField] <- 
        iSEE:::.colorByNothingTitle
    memory <- list(redDim=redDimArgs, geneExpr=geneExprArgs, colData=colDataArgs, geneStat=geneStatArgs)
    tab2 <- iSEE:::.spawn_table_links(memory)

    tabX <- tabs
    tabX$geneStatTable1$color <- character(0)
    tabX$geneStatTable3$color <- character(0)
    expect_identical(tabX, tab2)
})

test_that("table destruction works correctly", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    iSEE:::.destroy_table(pObjects, "geneStatTable1")
    expect_identical(pObjects$table_links$geneStatTable1, 
        list(color=character(0), xaxis=character(0), yaxis=character(0)))

    comp <- memory
    comp$redDim[1,iSEE:::.colorByGeneTable] <- ""
    comp$colData[2,iSEE:::.colorByGeneTable] <- ""
    comp$geneExpr[2,iSEE:::.geneExprYAxisGeneTable] <- ""
    comp$geneExpr[3,iSEE:::.geneExprXAxisGeneTable] <- ""
    expect_identical(comp, pObjects$memory)

    # Destroying a simpler table.
    iSEE:::.destroy_table(pObjects, "geneStatTable3")
    expect_identical(pObjects$table_links$geneStatTable1, 
        list(color=character(0), xaxis=character(0), yaxis=character(0)))

    comp$colData[1,iSEE:::.colorByGeneTable] <- ""
    expect_identical(comp, pObjects$memory)
})

test_that("table modification works correctly", {
    # Changing colour:
    expect_true("redDimPlot1" %in% tabs$geneStatTable1$color)
    expect_false("redDimPlot1" %in% tabs$geneStatTable2$color)
    tab2 <- iSEE:::.modify_table_links(tabs, "redDimPlot1", "Gene statistics table 2", "Gene statistics table 1", mode = "color")
    expect_identical(tab2$geneStatTable1$color, setdiff(tabs$geneStatTable1$color, "redDimPlot1"))
    expect_identical(tab2$geneStatTable2$color, union(tabs$geneStatTable2$color, "redDimPlot1"))
    
    # Changing x-axis.
    expect_true("geneExprPlot3" %in% tabs$geneStatTable1$xaxis)
    expect_false("geneExprPlot3" %in% tabs$geneStatTable2$xaxis)
    tab2 <- iSEE:::.modify_table_links(tabs, "geneExprPlot3", "Gene statistics table 2", "Gene statistics table 1", mode = "xaxis")
    expect_identical(tab2$geneStatTable1$xaxis, setdiff(tab2$geneStatTable1$xaxis, "geneExprPlot3"))
    expect_identical(tab2$geneStatTable2$xaxis, union(tab2$geneStatTable2$xaxis, "geneExprPlot3"))

    # Changing y-axis.
    expect_true("geneExprPlot2" %in% tabs$geneStatTable1$yaxis)
    expect_false("geneExprPlot2" %in% tabs$geneStatTable2$yaxis)
    tab2 <- iSEE:::.modify_table_links(tabs, "geneExprPlot2", "Gene statistics table 2", "Gene statistics table 1", mode = "yaxis")
    expect_identical(tab2$geneStatTable1$yaxis, setdiff(tab2$geneStatTable1$yaxis, "geneExprPlot2"))
    expect_identical(tab2$geneStatTable2$yaxis, union(tab2$geneStatTable2$yaxis, "geneExprPlot2"))

    # Destroying links.
    tab2 <- iSEE:::.modify_table_links(tabs, "redDimPlot1", "", "Gene statistics table 1", mode = "color")
    expect_identical(tab2$geneStatTable1$color, "colDataPlot2")

    tab2 <- iSEE:::.modify_table_links(tabs, "geneExprPlot3", "", "Gene statistics table 1", mode = "xaxis")
    expect_identical(tab2$geneStatTable1$xaxis, character(0))

    tab2 <- iSEE:::.modify_table_links(tabs, "geneExprPlot2", "", "Gene statistics table 1", mode = "yaxis")
    expect_identical(tab2$geneStatTable1$yaxis, character(0))

    # Destroying links that were never there has no effect.
    expect_false("geneExprPlot2" %in% tabs$geneStatTable3$color)
    tab2 <- iSEE:::.modify_table_links(tabs, "geneExprPlot2", "", "Gene statistics table 3", mode = "color")
    expect_identical(tab2, tabs)

    # Adding links.
    expect_false("geneExprPlot2" %in% tabs$geneStatTable3$color)
    tab2 <- iSEE:::.modify_table_links(tabs, "geneExprPlot2", "Gene statistics table 3", "", mode = "color")
    expect_identical(tab2$geneStatTable3$color, union(tab2$geneStatTable3$color, "geneExprPlot2"))

    # Adding links already there has no effect.
    expect_true("geneExprPlot2" %in% tabs$geneStatTable1$yaxis)
    tab2 <- iSEE:::.modify_table_links(tabs, "geneExprPlot2", "Gene statistics table 1", "", mode = "yaxis")
    expect_identical(tab2, tabs)
})

test_that("table observers work correctly", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    # Changing the table.
    out <- iSEE:::.setup_table_observer("redDim", 1, input=list(redDimColorBy1=iSEE:::.colorByGeneTableTitle, redDimColorByGeneTable1="Gene statistics table 2"),
        pObjects, iSEE:::.colorByField, iSEE:::.colorByGeneTableTitle, iSEE:::.colorByGeneTable, param = "color")
    expect_true(out)
    expect_false("redDimPlot1" %in% pObjects$table_links$geneStatTable1)
    expect_true("redDimPlot1" %in% pObjects$table_links$geneStatTable2)
    expect_identical(iSEE:::.colorByGeneTableTitle, pObjects$memory$redDim[1, iSEE:::.colorByField])
    expect_identical("Gene statistics table 2", pObjects$memory$redDim[1, iSEE:::.colorByGeneTable])

    # Changing the colour source.
    out <- iSEE:::.setup_table_observer("redDim", 1, input=list(redDimColorBy1=iSEE:::.colorByNothingTitle, redDimColorByGeneTable1="Gene statistics table 1"),
        pObjects, iSEE:::.colorByField, iSEE:::.colorByGeneTableTitle, iSEE:::.colorByGeneTable, param = "color")
    expect_true(out)
    expect_false("redDimPlot1" %in% pObjects$table_links$geneStatTable2)
    expect_false("redDimPlot1" %in% pObjects$table_links$geneStatTable1) # does NOT update.
    expect_identical(iSEE:::.colorByNothingTitle, pObjects$memory$redDim[1, iSEE:::.colorByField])
    expect_identical("Gene statistics table 1", pObjects$memory$redDim[1, iSEE:::.colorByGeneTable])
})

test_that("deleting table links is done correctly", {
    pObjects <- new.env()
    pObjects$table_links <- tabs
    pObjects$memory <- memory

    # Deleting something with colours.
    expect_true("redDimPlot1" %in% tabs$geneStatTable1$color)

    iSEE:::.delete_table_links("redDim", 1, pObjects)

    expect_false("redDimPlot1" %in% pObjects$table_links$geneStatTable1$color)
    expect_identical(pObjects$memory$redDim[1, iSEE:::.colorByGeneTable], "") 

    # Deleting something with x- and y-axis links.
    expect_true("geneExprPlot3" %in% tabs$geneStatTable1$xaxis)
    expect_true("geneExprPlot3" %in% tabs$geneStatTable2$yaxis)

    iSEE:::.delete_table_links("geneExpr", 3, pObjects)

    expect_false("geneExprPlot3" %in% pObjects$table_links$geneStatTable1$xaxis)
    expect_false("geneExprPlot3" %in% pObjects$table_links$geneStatTable2$yaxis)
    expect_identical(pObjects$memory$geneExpr[3, iSEE:::.geneExprXAxisGeneTable], "")
    expect_identical(pObjects$memory$geneExpr[3, iSEE:::.geneExprYAxisGeneTable], "")
})
