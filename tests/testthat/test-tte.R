context("tte")

library(testthat)

# Tests TTE (time-to-end) module and utility functions

test_that("kmEventTables works without errors", {
  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data=survival::lung, conf.type='log-log')
  testthat::expect_s3_class(kmEventTables(fit), "htmlTable")
})
