#' Histograms and frequency polygons
#'
#' Visualise the distribution of a single continuous variable by dividing
#' the x axis into bins and counting the number of observations in each bin.
#' Histograms (`geom_round_histogram()`) display the counts with bars; frequency
#' polygons (`geom_freqpoly()`) display the counts with lines. Frequency
#' polygons are more suitable when you want to compare the distribution
#' across the levels of a categorical variable.
#'
#' `stat_bin()` is suitable only for continuous x data. If your x data is
#' discrete, you probably want to use [ggplot2::stat_count()].
#'
#' By default, the underlying computation (`stat_bin()`) uses 30 bins;
#' this is not a good default, but the idea is to get you experimenting with
#' different number of bins. You can also experiment modifying the `binwidth` with
#' `center` or `boundary` arguments. `binwidth` overrides `bins` so you should do
#' one change at a time. You may need to look at a few options to uncover
#' the full story behind your data.
#'
#' By default, the _height_ of the bars represent the counts within each bin.
#' However, there are situations where this behavior might produce misleading
#' plots (e.g., when non-equal-width bins are used), in which case it might be
#' preferable to have the _area_ of the bars represent the counts (by setting
#' `aes(y = after_stat(count / width))`). See example below.
#'
#' In addition to `geom_round_histogram()`, you can create a histogram plot by using
#' `scale_x_binned()` with [geom_round_bar()]. This method by default plots tick marks
#' in between each bar.
#'
#' @section Aesthetics:
#' `geom_round_histogram()` uses the same aesthetics as [geom_round_bar()];
#' `geom_freqpoly()` uses the same aesthetics as [ggplot2::geom_line()].
#'
#' @export
#' @inheritParams ggplot2::layer
#' @inheritParams geom_round_bar
#' @param stat Use to override the default connection between
#'   `geom_round_histogram()`/`geom_freqpoly()` and `stat_bin()`. For more information
#'   at overriding these connections, see how the `stat` and
#'   `geom` arguments work.
#' @param binwidth The width of the bins. Can be specified as a numeric value,
#'  or a function that calculates width from the data. The default is to use
#' @param bins Number of bins. Overridden by `binwidth`.
#' @examples
#' ggplot(diamonds, aes(carat)) +
#'   geom_round_histogram()
#' ggplot(diamonds, aes(carat)) +
#'   geom_round_histogram(binwidth = 0.01)
#' ggplot(diamonds, aes(carat)) +
#'   geom_round_histogram(bins = 200)
#' # Map values to y to flip the orientation
#' ggplot(diamonds, aes(y = carat)) +
#'   geom_round_histogram()
#'
#' # For histograms with tick marks between each bin, use `geom_round_bar()` with
#' # `scale_x_binned()`.
#' ggplot(diamonds, aes(carat)) +
#'   geom_round_bar() +
#'   scale_x_binned()
#'
#' # Rather than stacking histograms, it's easier to compare frequency
#' # polygons
#' ggplot(diamonds, aes(price, fill = cut)) +
#'   geom_round_histogram(binwidth = 500)
#' ggplot(diamonds, aes(price, colour = cut)) +
#'   geom_freqpoly(binwidth = 500)
#'
#' # To make it easier to compare distributions with very different counts,
#' # put density on the y axis instead of the default count
#' ggplot(diamonds, aes(price, after_stat(density), colour = cut)) +
#'   geom_freqpoly(binwidth = 500)
#'
#'
#' # When using the non-equal-width bins, we should set the area of the bars to
#' # represent the counts (not the height).
#' # Here we're using 10 equi-probable bins:
#' price_bins <- quantile(diamonds$price, probs = seq(0, 1, length = 11))
#'
#' ggplot(diamonds, aes(price)) +
#'   geom_round_histogram(breaks = price_bins, color = "black") # misleading (height = count)
#'
#' ggplot(diamonds, aes(price, after_stat(count / width))) +
#'   geom_round_histogram(breaks = price_bins, color = "black") # area = count
#'

#'
#' # You can specify a function for calculating binwidth, which is
#' # particularly useful when faceting along variables with
#' # different ranges because the function will be called once per facet
#' ggplot(economics_long, aes(value)) +
#'   facet_wrap(~variable, scales = 'free_x') +
#'   geom_round_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))
geom_round_histogram <- function(mapping = NULL, data = NULL,
                           stat = "bin", position = "stack",
                           ...,
                           binwidth = NULL,
                           bins = NULL,
                           radius = 2,
                           na.rm = FALSE,
                           orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRoundBar,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(
            binwidth = binwidth,
            bins = bins,
            radius = radius,
            na.rm = na.rm,
            orientation = orientation,
            pad = FALSE,
            ...
        )
    )
}
