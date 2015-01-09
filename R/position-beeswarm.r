#' Beeswarm-style plots to show overlapping points. x must be discrete.
#'
#' @family position adjustments
#' @param width degree of jitter in x direction. Defaults to 90\% of the
#'   resolution of the data.
#' @param height ignored
#' @export
#' @examples
#'
#'
#' qplot(class, hwy, data = mpg, position="beeswarm")
position_beeswarm <- function (width = NULL, height = NULL) {
  PositionBeeswarm$new(width = width, height = height)
}

PositionBeeswarm <- proto(Position, {
  objname <- "beeswarm"

  adjust <- function(., data) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics(c("x", "y"), names(data), "position_jitter")

    if (is.null(.$width)) .$width <- resolution(data$x, zero = FALSE) * 0.9
    if (is.null(.$height)) .$height <- resolution(data$y, zero = FALSE) * 0.4

    trans_x <- NULL
    trans_y <- NULL
    nbins <- as.integer(length(data$y)/5)
    if(.$width > 0) {
      bin_y <- seq(min(data$y), max(data$y), length.out=nbins)
      trans_x <- function(x) {
        # Split the y axis by the values in x
        split.y <- split(data$y, x)
        lengths <- sapply(split.y, function(i) max(table(cut(i, bin_y))))
        max.len <- max(lengths)
        x.offsets <- sapply(split.y, function(i) {
          cuts <- cut(i, bin_y)
          tmp <- sapply(split(i, cuts), function(j) {
            n = length(j)
            if (n != 0) {
              w = .$width/max.len
              return(seq((-w)*((n-1)/2), w*((n-1)/2), by=w))
            } else {
              return(j)
            }
          })
          unsplit(tmp, cuts)
        })
        x.offsets <- unsplit(x.offsets, x)
        x + x.offsets
      }
    }

    transform_position(data, trans_x, trans_y)
  }

})
