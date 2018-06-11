#' Jitter points for categorical variables
#'
#' Add quasi-random jitter on the x-axis for violin plots when the x-axis variable is categorical.
#' Add random jitter within a rectangular area for square plots when both x- and y-axis variables are categorical.
#'
#' @param X A factor corresponding to a categorical variable.
#' @param Y A numeric vector of the same length as \code{X} for \code{jitterViolinPoints}, or a factor of the same length as \code{X} for \code{jitterSquarePoints}. 
#' @param grouping A named list of factors of the same length as \code{X}, specifying how elements should be grouped.
#' @param ... Further arguments to be passed to \code{\link{offsetX}}.
#'
#' @details
#' The \code{jitterViolinPoints} function calls \code{\link{offsetX}} to obtain quasi-random jittered x-axis values.
#' This reflects the area occupied by a violin plot, though some tuning of arguments in \code{...} may be required to get an exact match.
#' 
#' The \code{jitterSquarePoints} function will uniformly jitter points on both the x- and y-axes. 
#' The jitter area is a square with area proportional to the frequency of the paired levels in \code{X} and \code{Y}.
#' If either factor only has one level, the jitter area becomes a rectangle that can be interpreted as a bar plot.
#'
#' If \code{grouping} is specified, the values corresponding to each point defines a single combination of levels.
#' Both functions will then perform jittering separately within each unique combination of levels.
#' This is useful for obtaining appropriate jittering when points are split by group, e.g., during faceting.
#'
#' If \code{grouping!=NULL} for \code{jitterSquarePoints} the statistics in the returned \code{summary} data.frame will be stratified by unique combinations of levels.
#' To avoid clashes with existing fields, the names in \code{grouping} should not be \code{"X"}, \code{"Y"}, \code{"Freq"}, \code{"XWidth"} or \code{"YWidth"}.
#'
#' @return
#' For \code{jitterViolinPoints}, a numeric vector is returned containing the jittered x-axis coordinates for all points.
#' 
#' For \code{jitterSquarePoints}, a list is returned with numeric vectors \code{X} and \code{Y}, containing jittered coordinates on the x- and y-axes respectively for all points;
#' and \code{summary}, a data.frame of frequencies and side lengths for each unique pairing of X/Y levels.
#' 
#' @author Aaron Lun
#' @importFrom stats runif
#'
#' @export
#' @rdname jitterPoints
#'
#' @examples
#' X <- factor(sample(LETTERS[1:4], 100, replace=TRUE))
#' Y <- rnorm(100)
#' (out1 <- jitterViolinPoints(X=X, Y=Y))
#' 
#' Y2 <- factor(sample(letters[1:3], 100, replace=TRUE))
#' (out2 <- jitterSquarePoints(X=X, Y=Y))
#'
#' grouped <- sample(5, 100, replace=TRUE)
#' (out3 <- jitterViolinPoints(X=X, Y=Y, grouping=list(FacetRow=grouped)))
#' (out4 <- jitterSquarePoints(X=X, Y=Y2, grouping=list(FacetRow=grouped)))
jitterSquarePoints <- function(X, Y, grouping=NULL) {
    if (!is.factor(Y)) {
        stop("'Y' should be a factor")
    }
    if (!is.factor(X)) {
        stop("'X' should be a factor")
    }

    by_group <- .define_groups(X, Y, grouping)
    jittered_X <- jittered_Y <- numeric(length(Y))
    all_summary <- vector("list", length(by_group))

    # X/Y-jitter for square plots. 
    for (g in seq_along(by_group)) {
        grp <- by_group[[g]]
		current <- data.frame(X=X[grp], Y=Y[grp])
		summary_data <- as.data.frame(table(X=current$X, Y=current$Y))

        norm_freq <- summary_data$Freq / max(summary_data$Freq)
        if (all(is.na(norm_freq))) {
            norm_freq <- numeric(nrow(summary_data))
        }

        # Collapsing to a bar plot if there is only one level on either axis.
    	if (nlevels(Y)==1L && nlevels(X)!=1L) {
			summary_data$XWidth <- 0.4
			summary_data$YWidth <- 0.49 * norm_freq
		} else if (nlevels(Y)!=1L && nlevels(X)==1L) {
            summary_data$XWidth <- 0.49 * norm_freq
            summary_data$YWidth <- 0.4
        } else {
            summary_data$XWidth <- summary_data$YWidth <- 0.49 * sqrt(norm_freq)
        }

        current$Marker <- seq_len(nrow(current))
        combined <- merge(current, summary_data, by=c('X', 'Y'), all_x=TRUE)
        o <- order(combined$Marker)
        width_x <- combined$XWidth[o]
        width_y <- combined$YWidth[o]
        jittered_X[grp] <- width_x*runif(nrow(current), -1, 1)
        jittered_Y[grp] <- width_y*runif(nrow(current), -1, 1)

        # Adding current bits and pieces regarding the grouping.
        if (!is.null(grouping)) {
            for (mode in names(grouping)) {
                summary_data[[mode]] <- rep(grouping[[mode]][grp[1]], nrow(summary_data))
            }
        }
        all_summary[[g]] <- summary_data
    }
    return(list(X=jittered_X + as.integer(X), Y=jittered_Y + as.integer(Y), 
                    summary=do.call(rbind, all_summary)))
}

#' @export
#' @rdname jitterPoints
#' @importFrom vipor offsetX
jitterViolinPoints <- function(X, Y, grouping=NULL, ...) {
    if (!is.numeric(Y)) {
        stop("'Y' should be numeric")
    }
    if (!is.factor(X)) {
        stop("'X' should be a factor")
    }

    jittered_X <- numeric(length(Y))
    by_group <- .define_groups(X, Y, grouping)
    for (g in by_group) {
        jittered_X[g] <- vipor::offsetX(Y[g], x=X[g], ...) 
    }
    return(jittered_X + as.integer(X))
}

.define_groups <- function(X, Y, grouping) {
    stopifnot(length(X)==length(Y))
    if (is.null(grouping)) {
        return(list(seq_along(Y)))
    }
    
    stopifnot(all(lengths(grouping)==length(X)))
    stopifnot(!is.null(names(grouping)))

    o <- do.call(order, grouping)
    nvals <- length(o)
    is_first <- logical(nvals)
    if (nvals) {
        for (grp in grouping) {
            grp <- grp[o]
            is_first <- is_first | c(TRUE, grp[-1]!=grp[-nvals])
        }
    }

    overall_group <- cumsum(is_first)
    by.group <- split(o, overall_group)
    unname(by.group)
}
