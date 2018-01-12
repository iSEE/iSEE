.make_redDimPlot <- function(se, param_choices, input, all.coordinates) 
# Makes the dimension reduction plot.
{ 
    red.dim <- reducedDim(se, param_choices[[.redDimType]])

    color_choice <- param_choices[[.colorByField]]
    if (color_choice==.colorByColDataTitle) {
      covariate.name <- param_choices[[.colorByColData]]
      covariate <- colData(se)[,covariate.name]
      astr <- aes_string(x="Dim1", y="Dim2", color="Covariate")
    } else if (color_choice==.colorByGeneTableTitle || color_choice==.colorByGeneTextTitle) {

      if (color_choice==.colorByGeneTableTitle) {
        covariate.name <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
        assay.choice <- param_choices[[.colorByGeneTableAssay]]
      } else {
        covariate.name <- param_choices[[.colorByGeneText]]
        if (!covariate.name %in% rownames(se)) { 
          covariate.name <- NULL
        }
        assay.choice <- param_choices[[.colorByGeneTextAssay]]
      }
      if (!is.null(covariate.name)) { 
        covariate <- assay(se, assay.choice)[covariate.name,]
        astr <- aes_string(x="Dim1", y="Dim2", color="Covariate")
      } else {
        covariate.name <- ""
        covariate <- NULL              
        astr <- aes_string(x="Dim1", y="Dim2")
      }
    } else {
      covariate.name <- ""
      covariate <- NULL              
      astr <- aes_string(x="Dim1", y="Dim2")
    }

    # Figuring out what to do with brushing.
    brush.in <- param_choices[[.brushByPlot]]
    if (brush.in!="") {
        brush.by <- .encode_panel_name(brush.in)
        brush.id <- input[[paste0(brush.by$Type, .brushField, brush.by$ID)]]
        brushed.pts <- brushedPoints(all.coordinates[[paste0(brush.by$Type, "Plot", brush.by$ID)]], brush.id)
        if (!is.null(brushed.pts) && nrow(brushed.pts)) { 
            print("YAY, brushing!")
            print(brushed.pts)
        } 
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

.make_colDataPlot <- function(se, param_choices, input) 
# Makes a plot of column data variables.
{
    aes_args <- list(y=param_choices[[.colDataYAxis]])
#     if (param_choices[[.colDataXAxis]]!=.colDataXAxisNothingTitle) { # Currently not-quite-working as plotPhenoData needs 'x'.
        aes_args$x <- param_choices[[.colDataXAxisColData]]
#     }

    color_choice <- param_choices[[.colorByField]]
    if (color_choice==.colorByColDataTitle) {
      aes_args$color <- param_choices[[.colorByColData]]
    } else if (color_choice==.colorByGeneTableTitle) {
      aes_args$color <- param_choices[[.colorByGeneTable]]
    } else if (color_choice==.colorByGeneTextTitle) {
      aes_args$color <- param_choices[[.colorByGeneText]]
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

    color_choice <- param_choices[[.colorByField]]
    if (color_choice==.colorByColDataTitle) {
      covariate.name <- param_choices[[.colorByColData]]
    } else if (color_choice==.colorByGeneTableTitle) {
      covariate.name <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
    } else if (color_choice==.colorByGeneTextTitle) {
      covariate.name <- param_choices[[.colorByGeneText]]
    } else {
      covariate.name <- NULL
    }

    # Getting the gene choice.
    cur.gene <- .find_linked_gene(se, param_choices[[.geneExprID]], input)
    if (!is.null(cur.gene)) { 
        plotExpression(se, exprs_values=param_choices[[.geneExprAssay]],
                       x=byx,
                       features=cur.gene,
                       colour_by=covariate.name)
    }
}

.find_linked_gene <- function(se, link, input) 
# Convenience function to identify the selected gene from the linked table.
{ 
    if (link=="") { 
        return(NULL)
    }
    tab.id <- .encode_panel_name(link)$ID
    linked.tab <- paste0("geneStatTable", tab.id, "_rows_selected")
    rownames(se)[input[[linked.tab]]]
}
