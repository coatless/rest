library(testthat)
library(rest)

# Don't want to use keyrings on CRAN
withr::with_envvar(
  c(GH_NO_KEYRING = "true"),
  test_check("rest")
)
