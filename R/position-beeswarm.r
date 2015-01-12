#' Beeswarm-style plots to show overlapping points. x must be discrete.
#'
#' @family position adjustments
#' @param width degree of jitter in x direction. Defaults to 90\% of the
#'   resolution of the data.
#' @param nbins the number of divisions of the y-axis to use (default: \code{length(y)/5})
#' @export
#' @examples
#'
#' qplot(class, hwy, data = mpg, position="beeswarm")
position_beeswarm <- function (width = NULL, nbins = NULL) {
  PositionBeeswarm$new(width = width, nbins = nbins)
}

PositionBeeswarm <- proto(Position, {
  width = NULL
  nbins = NULL

  new <- function(.,
                  width = NULL,
                  nbins = NULL) {
    .$proto(width=width,
            nbins=nbins)
  }

  objname <- "beeswarm"

  adjust <- function(., data) {

    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_jitter")

    if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.9
    if (is.null(.$nbins)) {
      .$nbins <- as.integer(length(data$y)/5)
      message("Default number of y-bins used (", .$nbins, ").")
    }
    trans_x <- NULL
    trans_y <- NULL

    if(.$width > 0) {

      y_bins <- seq(min(data$y), max(data$y), length.out=.$nbins)

      trans_x <- function(x) {

        split_y <- split(data$y, x)
        max_len <- max(sapply(split_y, function(i) max(table(cut(i, y_bins)))))

        x_offsets <- lapply(split_y, function(x_class) {
          cuts <- cut(x_class, y_bins)

          xy_offsets <- sapply(split(x_class, cuts), function(xy_bin) {
            len <- length(xy_bin)
            if (len == 0) {
              return(xy_bin)
            } else {
              w <- .$width/max_len
              offsets <- seq((-w)*((len-1)/2), w*((len-1)/2), by=w)
              # Place higher y values at end of "smile"
              offsets <- offsets[order(abs(offsets))][rank(xy_bin, ties="first")]
              return(offsets)
            }
          })

          unsplit(xy_offsets, cuts)
        })

        x_offsets <- unsplit(x_offsets, x)
        return(x + x_offsets)
      }
    }

    transform_position(data, trans_x, trans_y)
  }

})
