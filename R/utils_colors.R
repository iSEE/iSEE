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

#' Get panel colors
#'
#' Functions to get/set panel colors at the user and developer level.
#' This determines the color of the panel header as well as (for \linkS4class{DotPlot}s) the color and fill of the brush.
#'
#' @param x An instance of a \linkS4class{Panel} class.
#' 
#' @return
#' A string containing the color assigned to the class of \code{x}.
#' 
#' @details
#' For developers: \code{.panelColor} is a method that should be subclassed for each \linkS4class{Panel} subclass.
#' and determines the default color for all instances of that class.
#'
#' For users: by default, \code{.getPanelColor} will return the default color of each panel as specified by the developer in \code{.panelColor}.
#' However, users can override this by setting the \code{"iSEE_panel_colors"} global variable to a named character vector of colors (see Examples).
#' This can be used to customize the color scheme for any given call to \code{\link{iSEE}}.
#' The names of the vector should be set to the name of class to be overridden; if a class is not named here, its default color is used.
#' 
#' @author Aaron Lun
#'
#' @examples
#' rdp <- RedDimPlot()
#' 
#' # Default color, as specified by the developer:
#' .panelColor(rdp)
#'
#' # Still the default color:
#' .getPanelColor(rdp)
#'
#' # Overriding the default colors
#' options(iSEE_panel_colors=c(RedDimPlot="dodgerblue"))
#' .getPanelColor(rdp)
#' 
#' @rdname getPanelColor
#' @export
.getPanelColor <- function(x) {
    opts <- getOption("iSEE_panel_colors", NULL)
    if (.encodedName(x) %in% names(opts)) {
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
