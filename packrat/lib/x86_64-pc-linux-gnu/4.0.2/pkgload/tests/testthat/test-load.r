context("Loading")

test_that("Package root and subdirectory is working directory when loading", {
  expect_message(load_all("testLoadDir"), "[|].*/testLoadDir[|]")
  expect_message(load_all(file.path("testLoadDir", "R")), "[|].*/testLoadDir[|]")
})

test_that("helpers are available after load_all", {
  load_all("testLoadHelpers")

  # object defined R/
  expect_equal(baz, 1)

  # object defined in a helper
  expect_equal(foo, 1)

  # object definde in helper, referencing lazy data object mtcars2
  expect_equal(head_mtcars, head(mtcars2))

  # object defined in helper using explicitly qualified package name
  expect_equal(helper_baz, baz)

  unload("testLoadHelpers")
})

test_that("warn_if_conflicts works", {
  # no warning if no intersection
  expect_warning(warn_if_conflicts("pkg", c("foo"), c("bar")), NA)

  # warning if an intersection

  withr::with_options(c(crayon.enabled = FALSE),
    expect_warning(warn_if_conflicts("pkg", c("foo"), c("foo", "bar")), "foo().*masks.*pkg::foo()")
  )
})
