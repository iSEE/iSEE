# Colours for shinydashboard::box statuses.
# 8 intense cols + 2 greys for custom, with some matching row/cols plots/tables
panel_colors <- c(RedDimPlot="#3565AA",
                  RowStatTable="#E47E04",
                  RowDataPlot="#F2B701",
                  ColDataPlot="#DB0230",
                  FeatAssayPlot="#7BB854",
                  SampAssayPlot="#07A274",
                  ColStatTable="#B00258",
                  CustomDataPlot="#515356",
                  CustomStatTable="#B0B0B0",
                  HeatMapPlot="#7C378A")

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

#' @importFrom grDevices col2rgb rgb
new_colors <- 255 - ((255 - col2rgb(panel_colors))/5)
brush_fill_color <- rgb(new_colors[1,], new_colors[2,], new_colors[3,], maxColorValue=255)
names(brush_fill_color) <- names(panel_colors)

.brushFillOpacity <- 0.25
