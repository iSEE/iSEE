.track_it_all <- function(input, rObjects, se, pObjects) {
  
  # reddimfields <- c(.redDimType, .redDimXAxis, .redDimYAxis, ALLEXTRAS)
  # coldatafields <- c(.colDataYAxis, .colDataXAxis, .colDataXAxisColData, ALLEXTRAS)
  # geneexprfields <- c(.geneExprAssay, .geneExprXAxis, .geneExprXAxisColData,
                      # .geneExprXAxisGeneTable, .geneExprXAxisGeneText, ALLEXTRAS)
  aobjs <- as.data.frame(rObjects$active_plots)
  
  # rdobjs <- aobjs[aobjs$Type == "redDim",]
  # cdobjs <- aobjs[aobjs$Type == "colData",]
  # geobjs <- aobjs[aobjs$Type == "geneExpr",]
  # gtobjs <- aobjs[aobjs$Type == "geneStat",]
  
  # cmds are kept only for the plots and not for the tables
  
  aobjs <- aobjs[aobjs$Type!="geneStat",]

  # storing to a text character vector
  
  # tracked_code_reddims <- c()
  # tracked_code_coldata <- c()
  # tracked_code_geneexp <- c()
  
  # tracked_code_reddims <- c(tracked_code_reddims, "#### Reduced Dimension plots and parameters")
  # for (i in rdobjs$ID) {
  #   tracked_code_reddims <- c(tracked_code_reddims,paste0("## red dim plot ",i))
  #   # cat(length(reddimfields),"params: ")
  #   for (field in reddimfields)
  #     tracked_code_reddims <- c(tracked_code_reddims, paste0(.inputRedDim(field,i)," <- ",
  #                                                            as.character(input[[.inputRedDim(field,i)]])))
  #   
  #   gene_from_table <- rownames(se)[
  #     input[[paste0("geneStatTable",
  #                   as.numeric(gsub("Gene statistics table ","",
  #                                   input[[.inputRedDim(.colorByGeneTable,i)]])),
  #                   "_rows_selected")]]
  #     ]
  #   tracked_code_reddims <- c(tracked_code_reddims, paste0("geneFromTable",i," <- ", gene_from_table))
  #   
  #   tracked_code_reddims <- c(tracked_code_reddims, rep("# here goes the code for the func call",2))
  #   
  #   #                   
  #   # cat(rownames(se)[input[[paste0("geneStatTable",
  #   #                                as.numeric(gsub("Gene statistics table ","",
  #   #                                                input[[.inputRedDim(.colorByGeneExprs,i)]])),
  #   #                                "_rows_selected")]]])
  # }
  # 
  # 
  # 
  # tracked_code_coldata <- c(tracked_code_coldata, "#### Column Data plots and parameters")
  # for (i in cdobjs$ID) {
  #   tracked_code_coldata <- c(tracked_code_coldata,paste0("## col data plot ",i))
  #   for (field in coldatafields)
  #     tracked_code_coldata <- c(tracked_code_coldata, paste0(.inputColData(field,i)," <- ",
  #                                                            as.character(input[[.inputColData(field,i)]])))
  #   
  #   gene_from_table <- rownames(se)[
  #     input[[paste0("geneStatTable",
  #                   as.numeric(gsub("Gene statistics table ","",
  #                                   input[[.inputColData(.colorByGeneTable,i)]])),
  #                   "_rows_selected")]]
  #     ]
  #   tracked_code_coldata <- c(tracked_code_coldata, paste0("geneFromTable",i," <- ", gene_from_table))
  #   tracked_code_coldata <- c(tracked_code_coldata, rep("# here goes the code for the func call",2))
  #   
  # }
  # 
  # tracked_code_geneexp <- c(tracked_code_geneexp, "#### Gene Expression plots and parameters")
  # for (i in geobjs$ID) {
  #   tracked_code_geneexp <- c(tracked_code_geneexp,paste0("## gene expr plot ",i))
  #   for (field in coldatafields)
  #     tracked_code_geneexp <- c(tracked_code_geneexp, paste0(.inputGeneExpr(field,i)," <- ",
  #                                                            as.character(input[[.inputGeneExpr(field,i)]])))
  #   
  #   gene_from_table <- rownames(se)[
  #     input[[paste0("geneStatTable",
  #                   as.numeric(gsub("Gene statistics table ","",
  #                                   input[[.inputGeneExpr(.colorByGeneTable,i)]])),
  #                   "_rows_selected")]]
  #     ]
  #   tracked_code_geneexp <- c(tracked_code_geneexp, paste0("geneFromTable",i," <- ", gene_from_table))
  #   tracked_code_geneexp <- c(tracked_code_geneexp, rep("# here goes the code for the func call",2))
  #   
  # }
  # 
  # 

  # 
  # tracked_code_printed <- paste(c(tracked_code_reddims,
  #                                 tracked_code_coldata,
  #                                 tracked_code_geneexp,
  #                                 ""), collapse = "\n")
  # 
  # 
  # for (i in rdobjs$ID){
  #   tracked_code_reddims <- c(tracked_code_reddims,)
  # }
  # pObjects$commands
  
  tracked_code <- c("## Here's the list of commands to generate the plots you created using iSEE",
                    "## Just copy them in a live session of R where your SingleCellExperiment object is",
                    "## ... and if you want to adjust something, just edit the corresponding lines!",
                    "")
  for (i in seq_len(nrow(aobjs))) {
    tracked_code <- c(tracked_code, 
                      paste0("## ",paste0(aobjs$Type[i],"_",aobjs$ID[i])),
                      pObjects$commands[[aobjs$Type[i]]][aobjs$ID[i]],
                      ""
                      )
    message(paste0(aobjs$Type[i],"_",aobjs$ID[i]))
  }
    
  # 
  # tracked_code_printed <- c(pObjects$commands$redDim[1],
  #                           pObjects$commands$geneExp[1])
  
  tracked_code <- c(tracked_code,
                    "## to guarantee the reproducibility of your code, you should also",
                    "## record the output of sessionInfo()",
                    "sessionInfo()")
  
  return(tracked_code)
}