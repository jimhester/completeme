context("is_comment")

test_that("is_comment returns TRUE if currently in a comment", {
  env <- new.env()

  env$linebuffer <- ''
  expect_false(is_comment(env))

  env$linebuffer <- '# a commment'
  expect_true(is_comment(env))

  env$linebuffer <- 'foo <- bar # a commment'
  expect_true(is_comment(env))

  env$linebuffer <- ' # a commment'
  expect_true(is_comment(env))

  env$linebuffer <- ' "#" a commment'
  expect_false(is_comment(env))

  env$linebuffer <- ' " \\" #" a commment'
  expect_false(is_comment(env))
})
