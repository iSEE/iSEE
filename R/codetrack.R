.track_it_all <- function(input, rObjects, se) {
  reddimfields <- c(.redDimType,.colorByField, .colorByColData, .colorByGeneExprs, 
                    .colorByGeneExprsAssay, .brushByPlot, .redDimXAxis, .redDimYAxis)
  coldatafields <- c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData, .colorByField, 
                     .colorByColData, .colorByGeneExprs, .colorByGeneExprsAssay,
                     .brushByPlot)
  geneexprfields <- c(.geneExprID, .geneExprAssay, .geneExprXAxis, .geneExprXAxisColData,
                      .geneExprXAxisGeneExprs, .colorByField, .colorByColData, 
                      .colorByGeneExprs, .colorByGeneExprs, .brushByPlot)
  aobjs <- as.data.frame(rObjects$active_plots)
  rdobjs <- aobjs[aobjs$Type == "redDim",]
  cdobjs <- aobjs[aobjs$Type == "colData",]
  geobjs <- aobjs[aobjs$Type == "geneExpr",]
  gtobjs <- aobjs[aobjs$Type == "geneStat",]
  
  cat("\nRED DIM PARAMS\n")
  for (i in rdobjs$ID) {
    cat(length(reddimfields),"params: ")
    for (field in reddimfields)
      cat(.inputRedDim(field,i), input[[.inputRedDim(field,i)]],"; ")
    cat(rownames(se)[input[[paste0("geneStatTable",
                                   as.numeric(gsub("Gene statistics table ","",
                                                   input[[.inputRedDim(.colorByGeneExprs,i)]])),
                                   "_rows_selected")]]])
  }
  cat("\n")
  
  cat("\nCOL DATA PARAMS\n")
  for (i in cdobjs$ID) {
    cat(length(coldatafields),"params: ")
    for (field in coldatafields)
      cat(.inputColData(field,i), input[[.inputColData(field,i)]],"; ")
    cat(rownames(se)[input[[paste0("geneStatTable",
                                   as.numeric(gsub("Gene statistics table ","",
                                                   input[[.inputColData(.colorByGeneExprs,i)]])),
                                   "_rows_selected")]]])
  } 
  cat("\n")
  cat("\nGENE EXPR PARAMS\n")
  for (i in geobjs$ID) {
    cat(length(geneexprfields),"params: ")
    for (field in geneexprfields)
      cat(.inputGeneExpr(field,i), input[[.inputGeneExpr(field,i)]],"; ")
    cat(rownames(se)[input[[paste0("geneStatTable",
                                   as.numeric(gsub("Gene statistics table ","",
                                                   input[[.inputGeneExpr(.colorByGeneExprs,i)]])),
                                   "_rows_selected")]]])
  }
  cat("\n")
  
}