#' Makes a heatmap
#'
#' Make a heatmap with features on the Y axis and samples on the X axis.
#'
#' @param id Integer specifying the index of the heatmap plot panel
#' @param all_memory A \code{list} of \linkS4class{DataFrame}s, where each
#' \linkS4class{DataFrame} corresponds to a panel type and contains the
#' initial settings for each individual panel of that type.
#' @param all_coordinates A \code{list} of \code{data.frame}s that contain the coordinates and covariates of data points visible in each of the plots.
#' @param se A \linkS4class{SingleCellExperiment} object.
#' @param colormap An \linkS4class{ExperimentColorMap} object that defines
#' custom colormaps to apply to individual \code{assays}, \code{colData},
#' and \code{rowData} covariates.
#'
#' @return A \code{list} that includes the following elements:
#' \describe{
#'   \item{cmd}{A \code{list} of \code{list} of commands
#'   as \code{character} vectors to parse and evaluate to produce the final plot.
#'   Each list element groups functionally related commands with a common purpose
#'   that should be evaluated together, yet separately from other groups
#'   for certains tasks).
#'   }
#'   \item{xy}{A \code{data.frame} that includes coordinates and covariates of
#'   the plot.}
#'   \item{plot}{A \code{\link{ggplot}} object that results of the evaluation
#'   of \code{cmd} items.}
#'   \item{legends}{A list of \code{\link{ggplot}} objects that contains color
#'   legends for the column data annotations.}
#' }
#'
#' @author Charlotte Soneson, Aaron Lun, Kevin Rue-Albrecht
#' @rdname INTERNAL_make_heatMapPlot
#'
#' @importFrom ggplot2 scale_y_continuous geom_raster element_blank unit
#' @importFrom cowplot get_legend plot_grid
#' @importFrom reshape2 melt
#' @importFrom dplyr arrange
.make_heatMapPlot <- function(id, all_memory, all_coordinates, se, colormap) {
    param_choices <- all_memory$heatMapPlot[id,]
    eval_env <- new.env()
    eval_env$se <- se
    eval_env$colormap <- colormap
    eval_env$all_coordinates <- all_coordinates

    genes_selected_y <- param_choices[[.heatMapFeatName]][[1]]
    validate(need(
        length(genes_selected_y) > 0L,
        sprintf("At least one feature needed for heat map")
    ))
    assay_choice <- param_choices[[.heatMapAssay]]

    # Extract genes to be included and melt the matrix to long format
    data_cmds <- .initialize_cmd_store()
    data_cmds <- .add_command(data_cmds, c(
      sprintf("value.mat <- as.matrix(assay(se, %i)[%s, , drop=FALSE]);",
              assay_choice, paste(deparse(genes_selected_y), collapse="\n")),
      "plot.data <- reshape2::melt(value.mat, varnames=c('Y', 'X'));",
      "plot.data$Y <- factor(plot.data$Y, rev(rownames(value.mat)))"
    ))

    # Arrange cells according to the selected colData columns
    orderBy <- param_choices[[.heatMapColData]][[1]]
    validate(need(
        length(orderBy) > 0L,
        sprintf("At least one column annotation needed for heat map")
    ))

    data_cmds <- .add_command(data_cmds, c("",
        vapply(seq_along(orderBy), FUN=function(i) {
            lhs <- sprintf("plot.data[['OrderBy%i']]", i)
            rhs <- sprintf("colData(se)[['%s']][match(plot.data$X, rownames(colData(se)))]", orderBy[i])
            if (.is_groupable(colData(se)[[orderBy[i]]])) {
                sprintf("%s <- factor(%s);", lhs, rhs)
            } else {
                sprintf("%s <- %s;", lhs, rhs)
            }
        }, FUN.VALUE=character(1)),
        sprintf("plot.data <- dplyr::arrange(plot.data, %s);",
                paste0("OrderBy", seq_along(orderBy), collapse=",")),
        "plot.data$X <- factor(plot.data$X, levels=unique(plot.data$X));"
    ))

    data_cmds <- .evaluate_commands(data_cmds, eval_env)

    # Processing the column selection choice.
    alpha_cmd <- ""
    alpha_legend_cmd <- NULL
    select_out <- .process_selectby_choice(param_choices, all_memory)
    select_cmds <- select_out$cmds
    select_as_field <- .safe_field_name("SelectBy", colnames(colData(se)))

    if (length(select_cmds)) {
        # plot.data$X contains the gene names (as they are duplicated across plot.data, and cannot be in 'rownames'), hence the sub().
        select_cmds[["select"]] <- sub("rownames(plot.data)", "plot.data$X", select_cmds[["select"]], fixed=TRUE)
        .populate_selection_environment(all_memory[[select_out$transmitter$Type]][select_out$transmitter$ID,], eval_env)
        .text_eval(select_cmds, eval_env)

        if (param_choices[[.selectEffect]]==.selectTransTitle) {
            alpha_cmd <- ", alpha=SelectBy"
            alpha_legend_cmd <- "guides(alpha=FALSE) +"

        } else if (param_choices[[.selectEffect]]==.selectColorTitle) {
            ## Add annotation bar
            orderBy <- c(orderBy, select_as_field)
        }
    }

    validate(need(
        nrow(eval_env$plot.data) > 0L,
        sprintf("At least one observation needed for heat map")
    ))

    # Deciding whether to center and scale each row.
    # We do this here instead of using scale(), as this must be done after selection (if restricting).
    censcal_cmds <- .initialize_cmd_store()
    if (.heatMapCenterTitle %in% param_choices[[.heatMapCenterScale]][[1]]) {
        censcal_cmds <- .add_command(censcal_cmds, "plot.data$value <- plot.data$value - ave(plot.data$value, plot.data$Y);", name='centering')
    }
    if (.heatMapScaleTitle %in% param_choices[[.heatMapCenterScale]][[1]]) {
        # We don't use sd() for scaling, to mimic scale().
        censcal_cmds <- .add_command(censcal_cmds, "gene.var <- ave(plot.data$value, plot.data$Y, FUN=function(x) { sum(x^2)/(length(x)-1) });", name='gene.var')
        censcal_cmds <- .add_command(censcal_cmds, "plot.data$value <- plot.data$value/sqrt(gene.var);", name='scaling')
    }
    censcal_cmds <- .evaluate_commands(censcal_cmds, eval_env)

    # Define zoom command. Transform the ranges given by zoomData to the actual coordinates in the heatmap
    bounds <- param_choices[[.zoomData]][[1]]
    if (!is.null(bounds)) {
        zoom_cmds <- sprintf("plot.data <- subset(plot.data, Y %%in%% rownames(value.mat)[c(%s)]); # zooming in",
                             paste0(bounds, collapse=","))
        .text_eval(zoom_cmds, eval_env)
    } else {
        zoom_cmds <- NULL
    }

    # Heatmap. Get the colorbar separately to make it easier to guess the real
    # heatmap coordinates from a brush on the final combined plot
    plot_cmds <- c(
        "p0 <- ggplot(plot.data, aes(x=X, y=Y)) +",
        "geom_raster(aes(fill=value)) +",
        "labs(x='', y='') +",
        .get_heatmap_fill_cmd(param_choices, colormap,
                              min(eval_env$plot.data$value, na.rm=TRUE),
                              max(eval_env$plot.data$value, na.rm=TRUE)),
        "scale_y_discrete(expand=c(0, 0)) +",
        "theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.line=element_blank());",
        "heatlegend <- cowplot::get_legend(p0 + theme(legend.position='bottom'));"
    )
    .text_eval(plot_cmds, eval_env)

    # Annotations
    annot_cmds <- list(init="legends <- list()")
    for (i in which(orderBy!=select_as_field)) {
        if (is.numeric(eval_env$plot.data[[paste0("OrderBy", i)]])) {
            color_cmd <- sprintf("scale_fill_gradientn(colors=colDataColorMap(colormap, '%s', discrete=FALSE)(21L), na.value='grey50', name='%s') +",
                                 orderBy[i], orderBy[i])
        } else {
            color_cmd <- sprintf("scale_fill_manual(values=colDataColorMap(colormap, '%s', discrete=TRUE)(%i), na.value='grey50', drop=FALSE, name='%s') +",
                                 orderBy[i], .nlevels(eval_env$plot.data[[paste0("OrderBy", i)]]), orderBy[i])
        }

        annot_cmds[[paste0("OrderBy", i)]] <- c("",
            sprintf("p%i <- ggplot(plot.data, aes(x=X, y=1)) +", i) ,
            sprintf("geom_raster(aes(fill=OrderBy%i%s)) +", i, alpha_cmd),
            "labs(x='', y='') +",
            alpha_legend_cmd,
            sprintf("scale_y_continuous(breaks=1, labels='%s') +", orderBy[i]),
            color_cmd,
            "theme(axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(),
    rect=element_blank(), line=element_blank(), axis.title.y=element_blank(),
    plot.margin=unit(c(0,0,-0.5,0), 'lines'));",
            sprintf("legends[[%i]] <- cowplot::get_legend(p%i + theme(legend.position='bottom', plot.margin=unit(c(0,0,0,0), 'lines')));", i, i)
        )
    }

    if (select_as_field %in% orderBy) {
        i <- which(orderBy==select_as_field)
        annot_cmds[["SelectBy"]] <- c("",
            sprintf("p%i <- ggplot(plot.data, aes(x=X, y=1)) +", i),
            "geom_raster(aes(fill=SelectBy)) +",
            "labs(x='', y='') +",
            "scale_y_continuous(breaks=1, labels='Selected points') +",
            sprintf("scale_fill_manual(values=c(`TRUE`='%s', `FALSE`='white')) +",param_choices[[.selectColor]]),
            "theme(axis.text.x=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(),
      rect=element_blank(), line=element_blank(), axis.title.y=element_blank(),
            plot.margin=unit(c(0,0,-0.5,0), 'lines'));",
            sprintf("legends[[%i]] <- cowplot::get_legend(p%i + theme(legend.position='bottom', plot.margin=unit(c(0,0,0,0), 'lines')));", i, i))
    }
    annot_cmds <- unlist(annot_cmds)

    # Evaluate to get the individual legends
    .text_eval(annot_cmds, envir=eval_env)
    legends <- eval_env$legends

    # Put heatmap and annotations together
    grid_cmds <- c(
          sprintf("cowplot::plot_grid(
      cowplot::plot_grid(
          %s,
          ncol=1, align='v', rel_heights=c(%s)),
      heatlegend, ncol=1, rel_heights=c(%s, %s))",
              paste0("p", c(seq_along(orderBy), 0),
                     c(rep(" + theme(legend.position='none')", length(orderBy)),
                       " + theme(legend.position='none')"), collapse=",\n        "),
              paste(c(rep(.heatMapRelHeightAnnot, length(orderBy)), .heatMapRelHeightHeatmap),
                    collapse=", "),
              1-.heatMapRelHeightColorBar, .heatMapRelHeightColorBar)
    )

    plot_out <- .text_eval(grid_cmds, eval_env)
    return(list(cmd_list=list(data=data_cmds$processed, select=select_cmds, centerscale=censcal_cmds$processed,
                              zoom=zoom_cmds, plot=plot_cmds, annot=annot_cmds, grid=grid_cmds),
                xy=eval_env$value.mat, plot=plot_out, legends=legends))
}

.get_colorscale_limits <- function(min.value, max.value, lower.bound, upper.bound, include.zero) {
  if (is.finite(lower.bound)) v1 <- lower.bound
  else v1 <- min.value
  if (is.finite(upper.bound)) v2 <- upper.bound
  else v2 <- max.value
  if (include.zero) sort(c(v1, v2, 0))
  else c(v1, v2)
}

#' @importFrom shiny showNotification req
#' @importFrom stats dist hclust as.dendrogram order.dendrogram
.cluster_genes <- function(X) {
     if (is.null(dim(X)) || nrow(X) < 2L) {
         showNotification("must have at least 2 features for clustering", type="error") # nocov start
         req(FALSE) # nocov end
     }
     D <- dist(X)
     hc <- hclust(D)
     d <- as.dendrogram(hc)
     rownames(X)[order.dendrogram(d)]
}

.transform_global_to_local_y <- function(yg, n.annot, n.genes) {
    k <- n.genes*(.heatMapRelHeightAnnot*n.annot+.heatMapRelHeightHeatmap)/(1-.heatMapRelHeightColorBar)
    m <- 0.5-.heatMapRelHeightColorBar*k
    v <- round(k*yg+m)
    # Has to be between 1 and n.genes
    v <- min(v, n.genes)
    v <- max(1, v)
    v
}

#' @importFrom scales rescale
.get_heatmap_fill_cmd <- function(param_choices, colormap, min.obs, max.obs) {
    # Set limits of color bar
    limits <- range(min.obs, param_choices[[.heatMapLower]],
                    max.obs, param_choices[[.heatMapUpper]], finite=TRUE, na.rm=TRUE)

    # Get values to define range of colors
    centered <- .heatMapCenterTitle %in% param_choices[[.heatMapCenterScale]][[1]]
    break.vec <- .get_colorscale_limits(min.value=min.obs, max.value=max.obs,
                                        lower.bound=param_choices[[.heatMapLower]],
                                        upper.bound=param_choices[[.heatMapUpper]],
                                        include.zero=centered)

    if (centered) {
        validate(need(
            is.na(param_choices[[.heatMapLower]]) || param_choices[[.heatMapLower]] < 0L,
            sprintf("Lower bound must be negative")
        ))
        validate(need(
            is.na(param_choices[[.heatMapUpper]]) || param_choices[[.heatMapUpper]] > 0L,
            sprintf("Upper bound must be positive")
        ))

        # Centered values - use selected color scale
        col.vec <- strsplit(param_choices[[.heatMapCenteredColors]], "-")[[1]]
        if (param_choices[[.heatMapLower]] > min.obs && is.finite(param_choices[[.heatMapLower]])) {
            break.vec <- c(min.obs, break.vec)
            col.vec <- c(col.vec[1], col.vec)
        }
        if (param_choices[[.heatMapUpper]] < max.obs && is.finite(param_choices[[.heatMapUpper]])) {
            break.vec <- c(break.vec, max.obs)
            col.vec <- c(col.vec, col.vec[length(col.vec)])
        }
        break.vec <- scales::rescale(break.vec, to=c(0, 1), from=limits)
        ## If there is only one value in the matrix (since the data is centered,
        ## the value is 0) and no valid upper or lower bounds have been chosen,
        ## color by the middle value in the chosen color vector
        if (limits[1] == limits[length(limits)]) {
            fill_cmd <- sprintf("scale_fill_gradientn(colors=c('%s'),
                                na.value='grey50') +",
                                strsplit(param_choices[[.heatMapCenteredColors]], "-")[[1]][2])
        } else {
            fill_cmd <- sprintf("scale_fill_gradientn(colors=c('%s'),
                            values=c(%s),
                                limits=c(%s), na.value='grey50') +",
                                paste0(col.vec, collapse="','"),
                                paste0(break.vec, collapse=","),
                                paste0(limits, collapse=","))
        }
    } else {
        validate(need(
            is.na(param_choices[[.heatMapLower]]) || param_choices[[.heatMapLower]] < max.obs,
            sprintf("Lower bound must be lower than largest value in matrix")
        ))
        validate(need(
            is.na(param_choices[[.heatMapUpper]]) || param_choices[[.heatMapUpper]] > min.obs,
            sprintf("Upper bound must be higher than smallest value in matrix")
        ))
        col.vec <- assayColorMap(colormap, param_choices[[.heatMapAssay]], discrete=FALSE)(21L)
        col.add.before <- col.add.after <- break.add.before <- break.add.after <- ""
        if (param_choices[[.heatMapLower]] > min.obs && is.finite(param_choices[[.heatMapLower]])) {
            col.add.before <- sprintf("c('%s', ", col.vec[1])
            break.add.before <- sprintf("c(%s, ", min.obs)
        }
        if (param_choices[[.heatMapUpper]] < max.obs && is.finite(param_choices[[.heatMapUpper]])) {
            col.add.after <- sprintf(", '%s')", col.vec[length(col.vec)])
            break.add.after <- sprintf(", %s)", max.obs)
        }
        if (col.add.before=="" && col.add.after != "") col.add.before <- "c("
        if (break.add.before=="" && break.add.after != "") break.add.before <- "c("
        if (col.add.after=="" && col.add.before != "") col.add.after <- ")"
        if (break.add.after=="" && break.add.before != "") break.add.after <- ")"

        fill_cmd <- sprintf("scale_fill_gradientn(colors=%sassayColorMap(colormap, '%s', discrete=FALSE)(21L)%s,
                            values=%s,
                            limits=c(%s), na.value='grey50') +",
                            col.add.before,
                            param_choices[[.heatMapAssay]],
                            col.add.after,
                            paste0("scales::rescale(", break.add.before,
                                   "seq(", min(break.vec), ", ", max(break.vec), ", length.out=21L)",
                                   break.add.after, ", to=c(0, 1), from=c(", paste0(limits, collapse=", "), "))"),
                            paste0(limits, collapse=","))
    }
    fill_cmd
}
