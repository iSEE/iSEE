# Colours for shinydashboard::box.

panel_colors <- c(redDimPlot="#3c8dbc",
                  rowStatTable="#00c0ef",
                  rowDataPlot="#dd4b39",
                  colDataPlot="#f39c12",
                  featExprPlot="#00a65a",
                  heatMapPlot="#bc4ddd")

.define_box_statuses <- paste(sprintf(".box.box-%s {
        border-top-color: %s;
}
.box.box-solid.box-%s {
        border: 1px solid %s;
}
.box.box-solid.box-%s > .box-header {
        color: #ffffff;
        background: %s;
        background-color: %s;
}
.box.box-solid.box-%s > .box-header a,
.box.box-solid.box-%s > .box-header .btn {
      color: #ffffff;
}
",
tolower(names(panel_colors)), panel_colors,
tolower(names(panel_colors)), panel_colors,
tolower(names(panel_colors)), panel_colors, panel_colors,
tolower(names(panel_colors)), tolower(names(panel_colors))
), collapse="\n")

brush_fill_color <- c(redDimPlot="#9cf", 
                      featExprPlot="#9f6", 
                      colDataPlot="#ff9", 
                      rowDataPlot="#9cf", 
                      heatMapPlot="#9cf")

brush_stroke_color <- c(redDimPlot="#06f", 
                        featExprPlot="#090", 
                        colDataPlot="#fc0", 
                        rowDataPlot="#06f", 
                        heatMapPlot="#9cf")

brush_stroke_color_full <- c(redDimPlot="#0066ff", 
                             featExprPlot="#009900", 
                             colDataPlot="#ffcc00", 
                             rowDataPlot="#0066ff", 
                             heatMapPlot="#0066ff")
