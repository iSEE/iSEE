#' Define Javascript box classes
#'
#' Define Javascript box classes for different coloring based on the \linkS4class{Panel} subclasses.
#' This should be inserted into the \code{head} tag of the UI in \code{\link{iSEE}}.
#'
#' @param instances A list of all \linkS4class{Panel} classes that might be used in the app.
#'
#' @return A string containing the definition for Panel-specific JS classes.
#'
#' @author Aaron Lun
#'
#' @details
#' Note that JS classes seem to only consider lower-case class names,
#' so it would be unwise to use Panels in \code{instances} with class names that only differ by case.
#'
#' @rdname INTERNAL_define_box_statuses
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
#' It is highly recommended to define colors as hex color codes, for full compatibility with both HTML elements and R plots.
#'
#' For users: by default, \code{.getPanelColor} will return the default color of each panel as specified by the developer in \code{.panelColor}.
#' However, users can override this by setting the \code{panel.color} global option to a named character vector of colors (see Examples).
#' This can be used to customize the color scheme for any given call to \code{\link{iSEE}}.
#' The names of the vector should be set to the name of class to be overridden; if a class is not named here, its default color is used.
#'
#' @author Aaron Lun
#'
#' @examples
#' rdp <- ReducedDimensionPlot()
#'
#' # Default color, as specified by the developer:
#' .panelColor(rdp)
#'
#' # Still the default color:
#' .getPanelColor(rdp)
#'
#' # Overriding the default colors
#' iSEEOptions$set(panel.color=c(ReducedDimensionPlot="#1e90ff"))
#' .getPanelColor(rdp)
#'
#' @rdname getPanelColor
#' @export
.getPanelColor <- function(x) {
    opts <- iSEEOptions$get("panel.color")
    if (.encodedName(x) %in% names(opts)) {
        as.vector(opts[.encodedName(x)])
    } else {
        .panelColor(x)
    }
}

#' Lighten colors for fill
#'
#' Create a lighter version of the color for each Panel,
#' primarily for use in the fill of a brush.
#'
#' @param col String containing the color of a panel.
#' @param as.vector Logical scalar indicating whether the RGB values should be returned directly.
#'
#' @return String containing the lightened color.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_lighten_color_for_fill
#' @importFrom grDevices col2rgb rgb
.lighten_color_for_fill <- function(col, as.vector=FALSE) {
    new_colors <- 255 - ((255 - col2rgb(col))/5)
    if (!as.vector) {
        new_colors <- rgb(new_colors[1,], new_colors[2,], new_colors[3,], maxColorValue=255)
    }
    new_colors
}

.brushFillOpacity <- 0.25
