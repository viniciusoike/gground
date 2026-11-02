test_that("corner radii preserve units and interpret numbers as points", {
  expect_equal(as_radius(2), grid::unit(2, "pt"))
  radius <- grid::unit(3, "mm")
  expect_identical(as_radius(radius), radius)
})
