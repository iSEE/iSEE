.make_redDimPlot <- function(se, param_choices, input, all.coordinates) 
# Makes the dimension reduction plot.
{ 
    red.dim <- reducedDim(se, param_choices[[.redDimType]])

    color_choice <- param_choices[[.colorByField]]
    if (color_choice==.colorByColDataTitle) {
      covariate.name <- param_choices[[.colorByColData]]
      covariate <- colData(se)[,covariate.name]
      astr <- "aes(x=Dim1, y=Dim2, color=Covariate)"
      cov.str <- sprintf(";\nplot.data$Covariate <- colData(se)[,'%s']", covariate.name)
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
        astr <- "aes(x=Dim1, y=Dim2, color=Covariate)"
        cov.str <- sprintf(";\nplot.data$Covariate <- assay(se, '%s')['%s',]", 
                           assay.choice, covariate.name)
      } else {
        covariate.name <- ""
        covariate <- NULL              
        astr <- "aes(x=Dim1, y=Dim2)"
        cov.str <- ""
        }
    } else {
      covariate.name <- ""
      covariate <- NULL              
      astr <- "aes(x=Dim1, y=Dim2)"
      cov.str <- ""
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

    cmd <- sprintf(
      "red.dim <- reducedDim(se, '%s'); 
plot.data <- data.frame(Dim1 = red.dim[, %s], 
Dim2 = red.dim[, %s])%s;
ggplot(data = plot.data, %s) + geom_point(size=1.5) + labs(color='%s') + theme_void()",
      param_choices[[.redDimType]],
      param_choices[[.redDimXAxis]],
      param_choices[[.redDimYAxis]],
      cov.str,
      astr,
      covariate.name)
    print(cmd)
    list(xy = plot.data, cmd = cmd, plot = eval(parse(text = cmd)))
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
      xcoord <- se[[byx]]
    } else if (xchoice==.geneExprXAxisGeneExprsTitle) {
      byx <- param_choices[[.geneExprXAxisGeneExprs]]
      xcoord <- assay(se, i = param_choices[[.geneExprAssay]])[byx,]
      show_violin <- FALSE
      show_median <- FALSE
    } else {
      byx <- NULL
      xcoord <- NULL
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
      exprs.mat <- as.matrix(assays(se, param_choices[[.geneExprAssay]]))
      evals_long <- reshape2::melt(exprs.mat, value.name = "evals")
      colnames(evals_long) <- c("Feature", "Cell", "evals")
      samples_long <- data.frame(row.names = colnames(se))
      if (!is.null(xcoord)) samples_long[[byx]] <- xcoord
      
      aesth <- aes()
      if ( is.null(byx) ) {
        aesth$x <- as.symbol("Feature")
        one_facet <- TRUE
      } else {
        aesth$x <- as.symbol(x)
        one_facet <- FALSE
      }
      aesth$y <- as.symbol("evals")
      
      object <- cbind(evals_long, samples_long)
      
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
