.make_heatMapPlot <- function(id, all_memory, all_coordinates, se, colormap)
  # Makes a heatmap.
{
  param_choices <- all_memory$heatMapPlot[id,]
  data_cmds <- list()
  
  genes_selected_y <- param_choices[[.heatMapYAxisFeatName]][[1]]
  validate(need( 
    all(genes_selected_y %in% rownames(se)),
    sprintf("Invalid genes for heat map input") # NEED BETTER ERROR MESSAGE HERE?
  ))
  
  assay_choice <- param_choices[[.heatMapAssay]]
  
  data_cmds[["y"]] <- list(
    sprintf("plot.data <- as.matrix(assay(se, %i)[%s, , drop=FALSE]);", 
            assay_choice, paste0('c("', paste(genes_selected_y, collapse = '","'), '")')),
    sprintf("d <- reshape2::melt(plot.data);"),
    sprintf("d <- as.data.frame(merge(d, colData(se), by.x = 'Var2', by.y = 'row.names'));"),
    # sprintf("d <- dplyr::arrange(d, %s);", param_choices[.heatXAxisColData]),
    sprintf("d$Var2 <- factor(d$Var2, levels = unique(d$Var2));")
  )
  
  # color_out <- .process_colorby_choice_for_heatmaps(param_choices, all_memory, se, colormap)
  # data_cmds[["color"]] <- color_out$cmd
  # color_label <- color_out$label
  
  eval_env <- new.env()
  eval(parse(text=unlist(data_cmds)), envir=eval_env)

  extra_cmds <- list(
    sprintf("ggplot(d, aes(x = Var2, y = Var1)) +"),
    sprintf("geom_raster(aes(fill = value)) +"),
    sprintf("geom_segment(aes(x=as.integer(Var2)-0.5, xend=as.integer(Var2)+0.5, y=nrow(plot.data)+0.7, yend=nrow(plot.data)+1.3, color=%s)) +", param_choices[["ColorByColData"]]),
    sprintf("labs(x='', y='') +"),
    sprintf("theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +"),
    "theme(legend.position = 'bottom')"
  )

  plot_out <- eval(parse(text=unlist(c(data_cmds, extra_cmds))), envir=eval_env)
  
  return(list(cmd = c(data_cmds[["y"]], extra_cmds), xy = NULL, plot = plot_out))
}

