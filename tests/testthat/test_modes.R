
####################
# Tests start here #
####################

# .make_redDimPlot/.scatter_plot ----

test_that(".make_redDimPlot/.scatter_plot produce a valid list",{
  
  plot_count <- 6
  rv <- rowVars(logcounts(sce))
  top_var <- head(order(rv, decreasing = TRUE), plot_count*2)
  top_var_genes <- rownames(sce)[top_var]
  
  plot_features <- data.frame(
      x = head(top_var_genes, plot_count),
      y = tail(top_var_genes, plot_count),
      stringsAsFactors = FALSE
   )
  
  # launch the app itself ----
  
  app <- modeGating(sce, features = plot_features, featAssayMax = 12)
    
  # return value is a named list
  expect_s3_class(
    app,
    "shiny.appobj"
  )
  
})
