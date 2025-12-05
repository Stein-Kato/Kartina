# Function for making image histograms¨
# Arguments:
#   im: image
#   thr: lower threshold for pixel values
#   ymax: upper limit for the y axis
#
# Copied from https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html#example-1-histogram-equalisation
image_hist = function(im, bins, thr, ymax) {
  im_plot <- as.data.frame(im) |>
    dplyr::mutate(channel = factor(cc, labels = c('R','G','B')))
  ggplot2::ggplot(im_plot[im_plot$value > thr, ], ggplot2::aes(value, col = channel)) +
    ggplot2::geom_histogram(bins = bins, size = 2) +
    ggplot2::facet_wrap(~ channel) +
    ggplot2::coord_cartesian(ylim = c(0, ymax)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size=14),
          axis.text.y = ggplot2::element_text(size=14),
          legend.title = ggplot2::element_text(size=16),
          legend.text = ggplot2::element_text(size=14),
          axis.title = ggplot2::element_text(size=16)
          )
}
