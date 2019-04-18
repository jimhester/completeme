# completeme
[![Travis build status](https://travis-ci.org/jimhester/completeme.svg?branch=master)](https://travis-ci.org/jimhester/completeme)
[![Coverage status](https://codecov.io/gh/jimhester/completeme/branch/master/graph/badge.svg)](https://codecov.io/github/jimhester/completeme?branch=master)

The goal of completeme is to help me, help you. It allows package authors and
users to register a set of completion functions to define custom completions.

## Package Authors

Package authors can define custom completions for their package and register
them with **completeme** in `.onLoad()`.

```r
.onLoad <- function(pkg, lib) {
  completeme::register_completion(mypackagename = completion_function)
}
```

---

Name inspired by [YouCompleteMe](https://github.com/Valloric/YouCompleteMe) and [Maverick](https://media.giphy.com/media/tChHURpFSqaQg/giphy.gif).
