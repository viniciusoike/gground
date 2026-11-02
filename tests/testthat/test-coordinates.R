test_that("rounded rectangles render under transformed and polar coordinates", {
  rectangle <- ggplot2::ggplot(
    data.frame(xmin = 1, xmax = 2, ymin = 1, ymax = 3),
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  ) +
    geom_round_rect()

  expect_s3_class(
    ggplot2::ggplotGrob(
      rectangle + suppressWarnings(ggplot2::coord_trans(x = "sqrt"))
    ),
    "gtable"
  )
  expect_s3_class(
    ggplot2::ggplotGrob(rectangle + ggplot2::coord_polar(theta = "x")),
    "gtable"
  )
  expect_s3_class(
    ggplot2::ggplotGrob(rectangle + ggplot2::coord_polar(theta = "y")),
    "gtable"
  )
})

test_that("multiple notched crossbars render in one panel", {
  ranges <- data.frame(
    x = c(1, 2),
    y = c(2, 3),
    ymin = c(1, 2),
    ymax = c(3, 4),
    ynotchlower = c(1.8, 2.8),
    ynotchupper = c(2.2, 3.2),
    notchwidth = c(0.5, 0.5)
  )
  plot <- ggplot2::ggplot(
    ranges,
    ggplot2::aes(
      x,
      y,
      ymin = ymin,
      ymax = ymax,
      ynotchlower = ynotchlower,
      ynotchupper = ynotchupper,
      notchwidth = notchwidth
    )
  ) +
    geom_round_crossbar()

  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
  ranges$ynotchlower[2] <- NA_real_
  ranges$ynotchupper[2] <- NA_real_
  plot <- ggplot2::ggplot(
    ranges,
    ggplot2::aes(
      x,
      y,
      ymin = ymin,
      ymax = ymax,
      ynotchlower = ynotchlower,
      ynotchupper = ynotchupper,
      notchwidth = notchwidth
    )
  ) +
    geom_round_crossbar()
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})
