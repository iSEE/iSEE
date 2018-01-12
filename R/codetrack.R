.track_it_all <- function(input, rObjects, se) {
  
  reddimfields <- c(.redDimType, .redDimXAxis, .redDimYAxis, ALLEXTRAS)
  coldatafields <- c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData, ALLEXTRAS)
  geneexprfields <- c(.geneExprID, .geneExprAssay, .geneExprXAxis, .geneExprXAxisColData,
                      .geneExprXAxisGeneExprs, ALLEXTRAS)
  aobjs <- as.data.frame(rObjects$active_plots)
  rdobjs <- aobjs[aobjs$Type == "redDim",]
  cdobjs <- aobjs[aobjs$Type == "colData",]
  geobjs <- aobjs[aobjs$Type == "geneExpr",]
  gtobjs <- aobjs[aobjs$Type == "geneStat",]
  
  # cat("\nRED DIM PARAMS\n")
  # for (i in rdobjs$ID) {
  #   cat(length(reddimfields),"params: ")
  #   for (field in reddimfields)
  #     cat(.inputRedDim(field,i), input[[.inputRedDim(field,i)]],"; ")
  #   cat(rownames(se)[input[[paste0("geneStatTable",
  #                                  as.numeric(gsub("Gene statistics table ","",
  #                                                  input[[.inputRedDim(.colorByGeneExprs,i)]])),
  #                                  "_rows_selected")]]])
  # }
  # cat("\n")
  # 
  # cat("\nCOL DATA PARAMS\n")
  # for (i in cdobjs$ID) {
  #   cat(length(coldatafields),"params: ")
  #   for (field in coldatafields)
  #     cat(.inputColData(field,i), input[[.inputColData(field,i)]],"; ")
  #   cat(rownames(se)[input[[paste0("geneStatTable",
  #                                  as.numeric(gsub("Gene statistics table ","",
  #                                                  input[[.inputColData(.colorByGeneExprs,i)]])),
  #                                  "_rows_selected")]]])
  # } 
  # cat("\n")
  # cat("\nGENE EXPR PARAMS\n")
  # for (i in geobjs$ID) {
  #   cat(length(geneexprfields),"params: ")
  #   for (field in geneexprfields)
  #     cat(.inputGeneExpr(field,i), input[[.inputGeneExpr(field,i)]],"; ")
  #   cat(rownames(se)[input[[paste0("geneStatTable",
  #                                  as.numeric(gsub("Gene statistics table ","",
  #                                                  input[[.inputGeneExpr(.colorByGeneExprs,i)]])),
  #                                  "_rows_selected")]]])
  # }
  # cat("\n")
  # 
  
  # storing to a text character vector
  
  tracked_code_reddims <- c()
  tracked_code_coldata <- c()
  tracked_code_geneexp <- c()
  
  tracked_code_reddims <- c(tracked_code_reddims, "#### Reduced Dimension plots and parameters")
  for (i in rdobjs$ID) {
    tracked_code_reddims <- c(tracked_code_reddims,paste0("## red dim plot ",i))
    # cat(length(reddimfields),"params: ")
    for (field in reddimfields)
      tracked_code_reddims <- c(tracked_code_reddims, paste0(.inputRedDim(field,i)," <- ",
                                             as.character(input[[.inputRedDim(field,i)]])))
    
    gene_from_table <- rownames(se)[
      input[[paste0("geneStatTable",
                    as.numeric(gsub("Gene statistics table ","",
                                    input[[.inputRedDim(.colorByGeneTable,i)]])),
                    "_rows_selected")]]
      ]
    tracked_code_reddims <- c(tracked_code_reddims, paste0("geneFromTable",i," <- ", gene_from_table))
    
    tracked_code_reddims <- c(tracked_code_reddims, rep("# here goes the code for the func call",2))
    
    #                   
    # cat(rownames(se)[input[[paste0("geneStatTable",
    #                                as.numeric(gsub("Gene statistics table ","",
    #                                                input[[.inputRedDim(.colorByGeneExprs,i)]])),
    #                                "_rows_selected")]]])
  }
  
  
  
  tracked_code_coldata <- c(tracked_code_coldata, "#### Column Data plots and parameters")
  for (i in cdobjs$ID) {
    tracked_code_coldata <- c(tracked_code_coldata,paste0("## col data plot ",i))
    for (field in coldatafields)
      tracked_code_coldata <- c(tracked_code_coldata, paste0(.inputColData(field,i)," <- ",
                                                             as.character(input[[.inputColData(field,i)]])))
    
    gene_from_table <- rownames(se)[
      input[[paste0("geneStatTable",
                    as.numeric(gsub("Gene statistics table ","",
                                    input[[.inputColData(.colorByGeneTable,i)]])),
                    "_rows_selected")]]
      ]
    tracked_code_coldata <- c(tracked_code_coldata, paste0("geneFromTable",i," <- ", gene_from_table))
    tracked_code_coldata <- c(tracked_code_coldata, rep("# here goes the code for the func call",2))
    
  }
  
  tracked_code_geneexp <- c(tracked_code_geneexp, "#### Gene Expression plots and parameters")
  for (i in geobjs$ID) {
    tracked_code_geneexp <- c(tracked_code_geneexp,paste0("## gene expr plot ",i))
    for (field in coldatafields)
      tracked_code_geneexp <- c(tracked_code_geneexp, paste0(.inputGeneExpr(field,i)," <- ",
                                                             as.character(input[[.inputGeneExpr(field,i)]])))
    
    gene_from_table <- rownames(se)[
      input[[paste0("geneStatTable",
                    as.numeric(gsub("Gene statistics table ","",
                                    input[[.inputGeneExpr(.colorByGeneTable,i)]])),
                    "_rows_selected")]]
      ]
    tracked_code_geneexp <- c(tracked_code_geneexp, paste0("geneFromTable",i," <- ", gene_from_table))
    tracked_code_geneexp <- c(tracked_code_geneexp, rep("# here goes the code for the func call",2))
    
  }
  
  
  
  # cat("\n")
  
  # cat("\nCOL DATA PARAMS\n")
  # for (i in cdobjs$ID) {
  #   cat(length(coldatafields),"params: ")
  #   for (field in coldatafields)
  #     cat(.inputColData(field,i), input[[.inputColData(field,i)]],"; ")
  #   cat(rownames(se)[input[[paste0("geneStatTable",
  #                                  as.numeric(gsub("Gene statistics table ","",
  #                                                  input[[.inputColData(.colorByGeneExprs,i)]])),
  #                                  "_rows_selected")]]])
  # } 
  # cat("\n")
  # cat("\nGENE EXPR PARAMS\n")
  # for (i in geobjs$ID) {
  #   cat(length(geneexprfields),"params: ")
  #   for (field in geneexprfields)
  #     cat(.inputGeneExpr(field,i), input[[.inputGeneExpr(field,i)]],"; ")
  #   cat(rownames(se)[input[[paste0("geneStatTable",
  #                                  as.numeric(gsub("Gene statistics table ","",
  #                                                  input[[.inputGeneExpr(.colorByGeneExprs,i)]])),
  #                                  "_rows_selected")]]])
  # }
  # cat("\n")
  # 
  
  tracked_code_printed <- cat(paste(c(tracked_code_reddims,
                                      tracked_code_coldata,
                                      tracked_code_geneexp,
                                      ""), collapse = "\n"))
  
  return(tracked_code_printed)
  
}