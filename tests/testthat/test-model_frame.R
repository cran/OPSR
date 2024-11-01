test_that("finds data if no new data is passed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  mf1 <- model.frame(fit, data = dat)
  expect_no_error(model.frame(fit))
  mf2 <- model.frame(fit)
  expect_identical(mf1, mf2)
})

test_that("produces reasonable model frame even if onyl one new obs is passed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  newdat <- dat[1, ]
  mf <- model.frame(fit, data = newdat)
  expect_s3_class(mf, "data.frame")
  expect_equal(dim(mf), c(1, 6))
})
