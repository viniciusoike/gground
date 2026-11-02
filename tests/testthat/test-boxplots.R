find_point_fills <- function(grob) {
  fills <- if (inherits(grob, "points")) grob$gp$fill else character()
  children <- c(grob$children, grob$grobs)
  for (child in children) {
    fills <- c(fills, find_point_fills(child))
  }
  return(fills)
}

test_that("half boxplots render in either orientation", {
  plot <- ggplot2::ggplot(iris, ggplot2::aes(Petal.Width, Species)) +
    geom_half_round_boxplot(errorbar.draw = TRUE)
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})

test_that("outlier fill reaches the rendered points", {
  data <- data.frame(group = "a", value = c(1, 2, 3, 4, 100))
  for (geom in list(geom_round_boxplot, geom_half_round_boxplot)) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(group, value)) +
      geom(outlier.shape = 21, outlier.fill = "red")
    expect_true(
      "#FF0000FF" %in% toupper(find_point_fills(ggplot2::ggplotGrob(plot)))
    )
  }
})

test_that("full horizontal boxplot renders error bars", {
  plot <- ggplot2::ggplot(iris, ggplot2::aes(Petal.Width, Species)) +
    geom_round_boxplot(errorbar.draw = TRUE)
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})

test_that("identity boxplots calculate their default width", {
  data <- data.frame(
    x = 1,
    ymin = 0,
    lower = 1,
    middle = 2,
    upper = 3,
    ymax = 4
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = x,
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    )
  ) +
    geom_round_boxplot(stat = "identity")
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})
