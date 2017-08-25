context("current_argument")

test_that("current_argument returns the current named argument", {
  env <- new.env()

  env$linebuffer <- ''
  expect_equal(current_argument(env), "")

  env$linebuffer <- 'fun('
  expect_equal(current_argument(env), "")

  env$linebuffer <- 'fun(foo = '
  expect_equal(current_argument(env), "foo")

  env$linebuffer <- 'fun(foo=T, bar = "xyz/'
  expect_equal(current_argument(env), "bar")

  env$linebuffer <- 'fun(foo = bar == "xyz/'
  expect_equal(current_argument(env), "foo")

  env$linebuffer <- 'foo(bar(baz = "xyz/'
  expect_equal(current_argument(env), "baz")

  env$linebuffer <- 'foo(bar(baz == "xyz/'
  expect_equal(current_argument(env), "")

  env$linebuffer <- 'foo = "bar", baz = "qux'
  expect_equal(current_argument(env), "baz")

  env$linebuffer <- 'foo = bar == "xyz/'
  expect_equal(current_argument(env), "foo")
})
