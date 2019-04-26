context("in_Function")
env <- new.env()


test_that("Empty string returns False", {
  env$linebuffer <- ''
  env[["start"]] <- 1L
  expect_false(in_function(env))
})

test_that("When opening a function parenthesis the function is returned",{
  env$linebuffer <- 'fun('
  env[["start"]] <- 4L
  expect_equal(.DollarNames(in_function(env)), "currentFunction")
  expect_equal(in_function(env)$currentFunction, "fun")

})

test_that("When in a function parenthesis the function data is returned",{
  env$linebuffer <- 'utils::citation()'
  env[["start"]] <- 16L

  inFun <- in_function(env)

  expect_equal(.DollarNames(inFun),c("currentFunction","currArgs","IsFirstArg","currentArg"))
  expect_equal(inFun$currentFunction, "citation")
  expect_equal(length(inFun$currArgs), 3L)
  expect_equal(inFun$currArgs$package, "base")
  expect_null(inFun$currArgs$lib.loc)
  expect_null(inFun$currArgs$auto)
  expect_true(inFun$IsFirstArg)
  expect_equal(inFun$currentArg, "package")
})

