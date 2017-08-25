context("is_first_argument")

test_that("is_first_argument returns TRUE if the first argument, false otherwise", {
  env <- new.env()

  env$linebuffer <- ''
  expect_false(is_first_argument(env), FALSE)

  env$linebuffer <- 'fun('
  expect_true(is_first_argument(env))

  env$linebuffer <- 'fun(foo'
  expect_true(is_first_argument(env))

  env$linebuffer <- 'fun("foo'
  expect_true(is_first_argument(env))

  env$linebuffer <- 'fun(foo='
  expect_false(is_first_argument(env))

  env$linebuffer <- 'fun(foo = '
  expect_false(is_first_argument(env))

  env$linebuffer <- 'fun(foo = "bar"'
  expect_false(is_first_argument(env))

  env$linebuffer <- 'fun(bar,'
  expect_false(is_first_argument(env))
})
