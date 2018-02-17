# Colours for shinydashboard::box statuses.
panel_colors <- c(redDimPlot="#3c8dbc",
                  rowStatTable="#6b6d77",
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


# Colours for brushing (fill needs to be lighter than the stroke).
brush_stroke_color <- panel_colors

new_colors <- 255 - ((255 - col2rgb(panel_colors))/5)
brush_fill_color <- rgb(new_colors[1,], new_colors[2,], new_colors[3,], maxColorValue=255)
names(brush_fill_color) <- names(panel_colors)

.brushFillOpacity <- 0.25
