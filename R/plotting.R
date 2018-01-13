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

    # out.plot <- ggplot(plot.data, astr) +
    #     geom_point(size=1.5) +
    #     labs(color=covariate.name) +
    #     theme_void()

    #list(xy=plot.data, plot=out.plot)

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

    # Process Y-axis
    plot.data <- data.frame(
        Y = colData(se)[,param_choices[[.colDataYAxis]]]
    )
    cmd_y <- sprintf(
        "plot.data <- data.frame(Y = colData(se)[,'%s']);",
        param_choices[[.colDataYAxis]]
    )

    # Process X-axis
    if (param_choices[[.colDataXAxis]] == .colDataXAxisNothingTitle) {
        # TODO: allow toggling rank decreasing/increasing, note that factors cannot be negated
        plot.data$X <- rank(plot.data$Y, ties.method = "first")
        x_lab <- NULL
        cmd_x <- 'plot.data$X <- rank(plot.data$Y, ties.method = "first");'
    } else {
        plot.data$X <- colData(se)[,param_choices[[.colDataXAxisColData]]]
        x_lab <- param_choices[[.colDataXAxisColData]]
        cmd_x <- sprintf(
            "plot.data$X <- colData(se)[,'%s'];",
            param_choices[[.colDataXAxisColData]]
        )
    }

    # general case; color aes dropped later if applicable
    cmd_aes <- "aes(X, Y, color = Covariate)"
    # process the color choice if any
    color_choice <- param_choices[[.colorByField]]
    if (color_choice == .colorByColDataTitle) {
        covariate.name <- param_choices[[.colorByColData]]
        plot.data$Covariate <- colData(se)[,covariate.name]
        cmd_color <- sprintf(
            "plot.data$Covariate <- colData(se)[,'%s'];", covariate.name
        )
    } else if (color_choice == .colorByGeneTableTitle || color_choice == .colorByGeneTextTitle){
        if (color_choice == .colorByGeneTableTitle) {
            covariate.name <- .find_linked_gene(se, param_choices[[.colorByGeneTable]], input)
            assay.choice <- param_choices[[.colorByGeneTableAssay]]
        } else if (color_choice == .colorByGeneTextTitle) {
            covariate.name <- param_choices[[.colorByGeneText]]
            assay.choice <- param_choices[[.colorByGeneTextAssay]]
        } else {
            stop("Impossible color choice!")
        }
        # Set the color to the selected gene
        if (!is.null(covariate.name)){
            plot.data$Covariate <- assay(se, assay.choice)[covariate.name,]
            cmd_color <- sprintf(
                "plot.data$Covariate <- assay(se, '%s')['%s',];",
                assay.choice, covariate.name
            )
        } else {
            message("Color mode is gene expression, but none selected.")
            cmd_aes <- "aes(X, Y)"
        }
    } else {
        covariate.name <- NULL
        cmd_color <- "# No coloring data"
        cmd_aes <- "aes(X, Y)"
    }


    # Creating the plot.
    gg <- ggplot(plot.data, eval(parse(text = cmd_aes))) +
        geom_point() +
        labs(
            x = x_lab,
            y = param_choices[[.colDataYAxis]],
            color = covariate.name
        ) +
        theme_bw() +
        theme(
            legend.position = "bottom"
        )

    gg_cmd <- paste(
        sprintf("ggplot(plot.data, %s) +", cmd_aes),
        "geom_point() +",
        sprintf(
            "labs(y = '%s'%s%s) +",
            param_choices[[.colDataYAxis]],
            ifelse(is.null(x_lab), "", sprintf(", x = '%s'", x_lab)),
            ifelse(is.null(covariate.name), "", sprintf(", color = '%s', ", covariate.name))
        ),
        "theme_bw() +",
        "theme(legend.position = 'bottom')",
        sep = "\n\t"
    )

    cmd <- paste(cmd_y, cmd_x, cmd_color, gg_cmd, sep = "\n")

    message(cmd)
    # return(list(xy = plot.data, cmd = cmd, plot = eval(parse(text = cmd))))
    return(gg)
    # list(xy = plot.data, cmd = cmd, plot = eval(parse(text = cmd)))
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
