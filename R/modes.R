
mode_CyTOF <- function(
  se, n_linked = 3,
  featExprMax = max(5, n_featExprPlot),
  features = sample(rownames(se), size=featExprMax*2, replace=TRUE),
  ..., plot_width = 4){
  # This mode is meaningless with fewer than two featExprPlot
  stopifnot(n_featExprPlot > 1)
  
  featExprArgs <- featExprPlotDefaults(se, featExprMax)
  # prepare featExprArgs
  featExprArgs[[iSEE:::.featExprXAxis]] <- iSEE:::.featExprXAxisFeatNameTitle
  featExprArgs[[iSEE:::.featExprYAxis]] <- iSEE:::.featExprYAxisFeatNameTitle
  # Y axes take all the odd-numbered feature names
  featExprArgs[[iSEE:::.featExprXAxisFeatName]] <- features[seq(2,nrow(featExprArgs)*2,2)]
  # X axes take all the even-numbered feature names
  featExprArgs[[iSEE:::.featExprYAxisFeatName]] <- features[seq(1,nrow(featExprArgs)*2,2)]
  featExprArgs[[iSEE:::.brushByPlot]] <- c(
    "",
    sprintf("Feature expression plot %i", seq(1,n_featExprPlot-1,1)),
    rep("", nrow(featExprArgs) - n_featExprPlot)
  )
  featExprArgs[[iSEE:::.brushEffect]] <- c(
    "",
    rep("Restrict", n_featExprPlot - 2),
    "Color",
    rep("", nrow(featExprArgs) - n_featExprPlot)
  )
  # Show only the active 
  initialPanels = DataFrame(
    Name = c(
      sprintf("Feature expression plot %i", seq(1, n_featExprPlot, 1))
    ),
    Width = c(
      rep(plot_width, n_featExprPlot)
    )
  )
  # Preconfigure an app
  app <- iSEE(
    se,
    redDimArgs = NULL, colDataArgs = NULL,
    featExprArgs = featExprArgs,
    rowStatArgs = NULL, rowDataArgs = NULL, heatMapArgs = NULL,
    redDimMax = 0, colDataMax = 0,
    featExprMax = featExprMax, # possibly larger
    rowStatMax = 0, rowDataMax = 0, heatMapMax = 0,
    initialPanels = initialPanels,
    annot.orgdb = NULL, annot.keytype = "ENTREZID", annot.keyfield = NULL,
    ...
  )
  
  return(app)
}
