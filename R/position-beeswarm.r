#' Beeswarm-style plots to show overlapping points. x must be discrete.
#'
#' @family position adjustments
#' @param width degree of jitter in x direction. Defaults to 90\% of the
#'   resolution of the data.
#' @param height ignored
#' @export
#' @examples
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
    if (is.null(.$nbins)) .$nbins <- as.integer(length(data$y)/5)

    trans_x <- NULL
    trans_y <- NULL

    if(.$width > 0) {

      y_bins <- seq(min(data$y), max(data$y), length.out=nbins)

      trans_x <- function(x) {

        split_y <- split(data$y, x)
        max_len <- max(sapply(split_y, function(i) max(table(cut(i, y_bins)))))

        x_offsets <- sapply(split_y, function(x_class) {
          cuts <- cut(x_class, y_bins)
          xy_offsets <- sapply(split(x_class, cuts), function(xy_bin) {
            len <- length(xy_bin)
            if (len == 0) {
              return(xy_bin)
            }

            w <- .$width/max_len
            offsets <- seq((-w)*((len-1)/2), w*((len-1)/2), by=w)

            return(offsets)
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
