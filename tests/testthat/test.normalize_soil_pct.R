context("Normalize soil percentage")

test_that("Less than 100% works correctly", {
  sand <- 10
  silt <- 20
  clay <- 30
  rock <- 35
  no_rock <- normalize_soil_pct(sand, silt, clay)
  expect_equivalent(no_rock, list(100/6, 100/3, 50))
  expect_equal(Reduce(sum, no_rock), 100)
  w_rock <- normalize_soil_pct(sand, silt, clay, rock)
  r <- (100 - rock) / (sand + silt + clay)
  expect_equivalent(w_rock, list(sand * r, silt * r, clay * r, rock))
  expect_equal(Reduce(sum, w_rock), 100)
})

test_that("Greater than 100% works correctly", {
  sand <- 10
  silt <- 20
  clay <- 30
  rock <- 45
  w_rock <- normalize_soil_pct(sand, silt, clay, rock)
  r <- (100 - rock) / (sand + silt + clay)
  expect_equivalent(w_rock, list(sand * r, silt * r, clay * r, rock))
  expect_equal(Reduce(sum, w_rock), 100)
})
