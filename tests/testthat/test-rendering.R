test_that("exported geoms render panels and legends", {
  columns <- data.frame(x = c("a", "b"), y = c(2, 3))
  plots <- list(
    ggplot(mpg, aes(class, fill = drv)) + geom_round_bar(),
    ggplot(columns, aes(x, y, fill = x)) + geom_round_col(),
    ggplot(mpg, aes(class, hwy, fill = class)) + geom_round_boxplot(),
    ggplot(iris, aes(Species, Petal.Width, fill = Species)) +
      geom_half_round_boxplot(),
    ggplot(columns, aes(x = seq_along(x), y = y, fill = x)) + geom_round_tile(),
    ggplot(
      columns,
      aes(
        xmin = seq_along(x),
        xmax = seq_along(x) + 0.5,
        ymin = 0,
        ymax = y,
        fill = x
      )
    ) +
      geom_round_rect(),
    ggplot(columns, aes(x, y, ymin = y - 1, ymax = y + 1, fill = x)) +
      geom_round_crossbar(),
    ggplot(mpg, aes(hwy, fill = drv)) + geom_round_histogram(binwidth = 5)
  )
  for (plot in plots) {
    expect_s3_class(ggplotGrob(plot), "gtable")
  }
})

test_that("rounded themes and missing observations render", {
  plot <- ggplot(data.frame(x = c(1, 2), y = c(2, NA)), aes(x, y)) +
    geom_round_col(na.rm = TRUE) +
    theme(panel.background = element_round_rect(radius = grid::unit(3, "mm")))
  expect_s3_class(ggplotGrob(plot), "gtable")
})
