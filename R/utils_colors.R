.define_box_statuses <- function(instances) {
    all_modes <- vapply(instances, .encodedName, "")
    first <- !duplicated(all_modes)
    all_modes <- tolower(all_modes[first])
    all_colors <- vapply(instances[first], .getPanelColor, "")

    paste( 
        sprintf(".box.box-%s {
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
            all_modes, all_colors,
            all_modes, all_colors,
            all_modes, all_colors, all_colors,
            all_modes, all_modes
        ), 
        collapse="\n"
    )
}

.getPanelColor <- function(x) {
    opts <- getOption("iSEE_panel_colors", NULL)
    if (.encodedName(x) %in% opts) {
        opts[.encodedName(x)]
    } else {
        .panelColor(x)
    }
}

#' @importFrom grDevices col2rgb rgb
.lighten_color_for_fill <- function(col) {
    new_colors <- 255 - ((255 - col2rgb(col))/5)
    rgb(new_colors[1,], new_colors[2,], new_colors[3,], maxColorValue=255)
}

.brushFillOpacity <- 0.25
