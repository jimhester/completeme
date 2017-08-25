context("current_function")

test_that("current_function returns the current function, `\"\"` otherwise", {
  env$linebuffer <- ''
  expect_equal(current_function(env), "")

  env$linebuffer <- 'fun('
  expect_equal(current_function(env), "fun")

  env$linebuffer <- 'fun(foo = '
  expect_equal(current_function(env), "fun")

  env$linebuffer <- 'fun(foo=T, bar = "xyz/'
  expect_equal(current_function(env), "fun")

  env$linebuffer <- 'fun(foo = bar == "xyz/'
  expect_equal(current_function(env), "fun")

  env$linebuffer <- 'foo = "bar", baz = "qux'
  expect_equal(current_function(env), "")

  env$linebuffer <- 'foo = bar == "xyz/'
  expect_equal(current_function(env), "")

  env$linebuffer <- 'foo(bar(baz = "xyz/'
  expect_equal(current_function(env), "bar")

  env$linebuffer <- 'foo(); bar(baz = "xyz/'
  expect_equal(current_function(env), "bar")
})
