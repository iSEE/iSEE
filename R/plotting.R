.make_redDimPlot <- function(se, param_choices, input, all.coordinates) 
# Makes the dimension reduction plot.
{ 
    red.dim <- reducedDim(se, param_choices[[.redDimType]])

    color_choice <- param_choices[[.generalColorBy]]
    if (color_choice==.colorByColDataTitle) {
      covariate.name <- param_choices[[.generalColorByColData]]
      covariate <- colData(se)[,covariate.name]
      astr <- aes_string(x="Dim1", y="Dim2", color="Covariate")
    } else if (color_choice==.colorByGeneExprsTitle) {
      tab.id <- .encode_panel_name(param_choices[[.generalColorByGeneExprs]])$ID
      linked.tab <- paste0("geneStatTable", tab.id, "_rows_selected")
      covariate.name <- rownames(se)[input[[linked.tab]]]
      covariate <- assay(se, param_choices[[.generalColorByGeneExprsAssay]])[covariate.name,]
      astr <- aes_string(x="Dim1", y="Dim2", color="Covariate")
    } else {
      covariate.name <- ""
      covariate <- NULL              
      astr <- aes_string(x="Dim1", y="Dim2")
    }

    # Figuring out what to do with brushing.
    print(param_choices[[.brushByPlot]])
    brush.by <- .encode_panel_name(param_choices[[.brushByPlot]])
    brush.id <- input[[paste0(brush.by$Type, "Plot", brush.by$ID, .brushField)]]
    brushed.pts <- brushedPoints(all.coordinates[[paste0(brush.by$Type, "Plot", brush.by$ID)]], brush.id)
    print(brushed.pts)
    if (!is.null(brushed.pts) && nrow(brushed.pts)) { 
        print("YAY, brushing!")
        print(brushed.pts)
    } 

    plot.data <- data.frame(Dim1=red.dim[,param_choices[[.redDimXAxis]]],
                            Dim2=red.dim[,param_choices[[.redDimYAxis]]])
    plot.data$Covariate <- covariate
              
    out.plot <- ggplot(plot.data, astr) +
        geom_point(size=1.5) +
        labs(color=covariate.name) +
        theme_void()

    list(xy=plot.data, plot=out.plot)
}

.make_phenoDataPlot <- function(se, param_choices, input) 
# Makes a plot of column data variables.
{
    aes_args <- list(y=param_choices[[.phenoDataYAxisColData]])
#     if (param_choices[[.phenoDataXAxis]]!=.phenoDataXAxisNothingTitle) { # Currently not-quite-working as plotPhenoData needs 'x'.
        aes_args$x <- param_choices[[.phenoDataXAxisColData]]
#     }

    color_choice <- param_choices[[.generalColorBy]]
    if (color_choice==.colorByColDataTitle) {
      aes_args$color <- param_choices[[.generalColorByColData]]
    } else if (color_choice==.colorByGeneExprsTitle) {
      aes_args$color <- param_choices[[.generalColorByGeneExprs]]
    }
    aes_final <- do.call(aes_string, aes_args)
  
    # Creating the plot. 
    plotPhenoData(se, aes_final)
}

.make_geneExprPlot <- function(se, param_choices, input) 
# Makes a gene expression plot.
{
    xchoice <- param_choices[[.geneExprXAxis]]
    if (xchoice==.geneExprXAxisColDataTitle) {
      byx <- param_choices[[.geneExprXAxisColData]]
    } else if (xchoice==.geneExprXAxisGeneExprsTitle) {
      byx <- param_choices[[.geneExprXAxisGeneExprs]]
    } else {
      byx <- NULL
    }

    color_choice <- param_choices[[.generalColorBy]]
    if (color_choice==.colorByColDataTitle) {
      covariate.name <- param_choices[[.generalColorByColData]]
    } else if (color_choice==.colorByGeneExprsTitle) {
      tab.id <- .encode_panel_name(param_choices[[.generalColorByGeneExprs]])$ID
      linked.tab <- paste0("geneStatTable", tab.id, "_rows_selected")
      covariate.name <- rownames(se)[linked.tab]
    } else {
      covariate.name <- NULL
    }

    # Getting the gene choice.
    tab.id <- .encode_panel_name(param_choices[[.geneExprID]])$ID
    linked.tab <- paste0("geneStatTable", tab.id, "_rows_selected")
    cur.gene <- rownames(se)[input[[linked.tab]]]
    plotExpression(se, exprs_values=param_choices[[.geneExprAssay]],
                   x=byx,
                   features=cur.gene,
                   colour_by=covariate.name)
}

