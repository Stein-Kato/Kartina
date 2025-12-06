# Function for making image histograms¨
# Arguments:
#   im: image
#   thr: lower threshold for pixel values
#   ymax: upper limit for the y axis
#   text_size: basis size for text elements
#
# Copied from https://cran.r-project.org/web/packages/imager/vignettes/gettingstarted.html#example-1-histogram-equalisation
image_hist = function(im, bins, thr, ymax, text_size) {
  im_plot <- as.data.frame(im) |>
    dplyr::mutate(channel = factor(cc, labels = c('R','G','B')))

  if(is.na(text_size)) {
    text_size = 16
  }
  ggplot2::ggplot(im_plot[im_plot$value > thr, ], ggplot2::aes(value, col = channel)) +
    ggplot2::geom_histogram(bins = bins, size = 2) +
    ggplot2::facet_wrap(~ channel) +
    ggplot2::coord_cartesian(ylim = c(0, ymax)) +
    ggplot2::theme_classic(base_size = text_size)
}
