

# .make_redDimPlot/.scatter_plot ----

test_that(".make_redDimPlot/.scatter_plot produce a valid list",{
  
  p.out <- iSEE:::.make_redDimPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  # return value is a named list
  expect_type(
    p.out,
    "list"
  )
  expect_named(
    p.out,
    c("cmd", "xy", "plot")
  )
  
  # cmd value is a named list
  expect_type(
    p.out$cmd,
    "list"
  )
  expect_named(
    p.out$cmd,
    c("data","lim","brush","setup","plot")
  )
  
  # xy value is a data frame
  expect_s3_class(
    p.out$xy,
    "data.frame"
  )
  expect_named(
    p.out$xy,
    c("X","Y","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})

# .make_colDataPlot/.violin_plot ----

test_that(".make_colDataPlot/.violin_plot produce a valid list",{
  
  p.out <- iSEE:::.make_colDataPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  # return value is a named list
  expect_type(
    p.out,
    "list"
  )
  expect_named(
    p.out,
    c("cmd", "xy", "plot")
  )
  
  # cmd value is a named list
  expect_type(
    p.out$cmd,
    "list"
  )
  expect_named(
    p.out$cmd,
    c("data","lim","brush","setup","plot")
  )
  
  # xy value is a data frame
  expect_s3_class(
    p.out$xy,
    "data.frame"
  )
  expect_named(
    p.out$xy,
    c("Y","X","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})

# .make_colDataPlot/.griddotplot ----

test_that(".make_colDataPlot/.griddotplot produce a valid list",{
  
  p.out <- iSEE:::.make_colDataPlot(id = 2, all_memory, all_coordinates, sce, ecm)
  
  # return value is a named list
  expect_type(
    p.out,
    "list"
  )
  expect_named(
    p.out,
    c("cmd", "xy", "plot")
  )
  
  # cmd value is a named list
  expect_type(
    p.out$cmd,
    "list"
  )
  expect_named(
    p.out$cmd,
    c("data","lim","brush","setup","plot")
  )
  
  # xy value is a data frame
  expect_s3_class(
    p.out$xy,
    "data.frame"
  )
  expect_named(
    p.out$xy,
    c("Y","X","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})

# .make_geneExprPlot/.scatter_plot ----

test_that(".make_geneExprPlot/.scatter_plot produce a valid list",{
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  # return value is a named list
  expect_type(
    p.out,
    "list"
  )
  expect_named(
    p.out,
    c("cmd", "xy", "plot")
  )
  
  # cmd value is a named list
  expect_type(
    p.out$cmd,
    "list"
  )
  expect_named(
    p.out$cmd,
    c("data","lim","brush","setup","plot")
  )
  
  # xy value is a data frame
  expect_s3_class(
    p.out$xy,
    "data.frame"
  )
  expect_named(
    p.out$xy,
    c("Y","X","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})

test_that(".make_geneExprPlot works for YAxis set to Gene text", {
  # change the value locally for the specific test
  selected_gene <- "0610009B22Rik"
  
  all_memory$geneExpr[1,iSEE:::.geneExprYAxis] <- iSEE:::.geneExprYAxisGeneTextTitle
  all_memory$geneExpr[1,iSEE:::.geneExprYAxisGeneText] <- selected_gene
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  expect_match(
    p.out$cmd$data$y,
    selected_gene,
    fixed = TRUE
  )
  
})

test_that(".make_geneExprPlot works for XAxis set to Column data", {
  # change the value locally for the specific test
  all_memory$geneExpr[1,iSEE:::.geneExprXAxis] <- iSEE:::.geneExprXAxisColDataTitle
  all_memory$geneExpr[1,iSEE:::.geneExprXAxisColData] <- "dissection_s"
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  expect_match(
    p.out$cmd$data$x,
    "dissection_s",
    fixed = TRUE
  )
  
})

test_that(".make_geneExprPlot works for XAxis set to Gene table", {
  # change the value locally for the specific test
  all_memory$geneExpr[1,iSEE:::.geneExprXAxis] <- iSEE:::.geneExprXAxisGeneTableTitle
  all_memory$geneExpr[1,iSEE:::.geneExprXAxisGeneTable] <- "Gene statistics table 1" # dynamic value?
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  expect_match(
    p.out$cmd$data$x,
    "plot.data$X <- assay",
    fixed = TRUE
  )
  
})

test_that(".make_geneExprPlot works for XAxis set to Gene text", {
  selected_gene <- "0610009B22Rik"
  
  # change the value locally for the specific test
  all_memory$geneExpr[1,iSEE:::.geneExprXAxis] <- iSEE:::.geneExprXAxisGeneTextTitle
  all_memory$geneExpr[1,iSEE:::.geneExprXAxisGeneText] <- selected_gene
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  expect_match(
    p.out$cmd$data$x,
    selected_gene,
    fixed = TRUE
  )
  
})


test_that(".make_geneExprPlot works for groupable colour covariate", {
  selected_coldata <- "dissection_s"
  
  # change the value locally for the specific test
  all_memory$geneExpr[1,iSEE:::.colorByField] <- iSEE:::.colorByColDataTitle
  all_memory$geneExpr[1,iSEE:::.colorByColData] <- selected_coldata
  
  p.out <- iSEE:::.make_geneExprPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  expect_match(
    p.out$cmd$data$color,
    selected_coldata,
    fixed = TRUE
  )
  
  expect_match(
    p.out$cmd$data$more_color,
    "as.factor",
    fixed = TRUE
  )
  
  expect_match(
    p.out$cmd$plot$scale_color[[1]],
    "^scale_color_manual"
  )
  expect_match(
    p.out$cmd$plot$scale_color[[1]],
    selected_coldata,
    fixed = TRUE
  )
  
  expect_match(
    p.out$cmd$plot$scale_color[[2]],
    "^scale_fill_manual"
  )
  expect_match(
    p.out$cmd$plot$scale_color[[2]],
    selected_coldata,
    fixed = TRUE
  )
  
})

# .make_colDataPlot/.create_plot horizontal violin plots ----

test_that(".make_colDataPlot/.create_plot can produce horizontal violins", {
  selected_coldataX <- "NREADS"
  selected_coldataY <- "driver_1_s"
  
  # change the value locally for the specific test
  all_memory$colData[1,iSEE:::.colDataXAxis] <- iSEE:::.colorByColDataTitle
  all_memory$colData[1,iSEE:::.colDataXAxisColData] <- selected_coldataX
  
  all_memory$colData[1,iSEE:::.colDataYAxis] <- selected_coldataY
  
  all_memory$colData[1,iSEE:::.colorByField] <- iSEE:::.colDataXAxisNothingTitle
  
  p.out <- iSEE:::.make_colDataPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  expect_match(
    p.out$cmd$data$y,
    selected_coldataY,
    fixed = TRUE
  )
  
  expect_match(
    p.out$cmd$data$x,
    selected_coldataX,
    fixed = TRUE
  )
  
  expect_named(
    p.out$xy,
    c("Y","X")
  )
  
})

# scatter plot without zoom ----

test_that(".make_redDimPlot/.scatter_plot produce a valid list",{
  
  all_memory$redDim[[iSEE:::.zoomData]][1] <- list(NULL)
  
  p.out <- iSEE:::.make_redDimPlot(id = 1, all_memory, all_coordinates, sce, ecm)
  
  # return value is a named list
  expect_type(
    p.out,
    "list"
  )
  expect_named(
    p.out,
    c("cmd", "xy", "plot")
  )
  
  # cmd value is a named list
  expect_type(
    p.out$cmd,
    "list"
  )
  expect_named(
    p.out$cmd,
    c("data","lim","brush","setup","plot")
  )
  
  # xy value is a data frame
  expect_s3_class(
    p.out$xy,
    "data.frame"
  )
  expect_named(
    p.out$xy,
    c("X","Y","ColorBy")
  )
  
  #plot
  expect_s3_class(
    p.out$plot,
    c("gg", "ggplot")
  )

})
