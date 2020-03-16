context("test-utils.R")

#' @importFrom tibble tibble
NULL

test_that("totalSubjects works with tibbles", {
  t1 <- tibble(id = 1:10, y = sample(c("a", "b"), 10, replace = TRUE))
  t2 <- tibble(id = 6:15, y = sample(c("a", "b"), 10, replace = TRUE))
  tlst <- list(t1 = t1, t2 = t2)
  
  expect_length(totalSubjects(tlst, idkey = "id"), 15)
})

test_that("totalSubjects works with data frame", {
  df1 <- data.frame(id = 1:10, y = sample(c("a", "b"), 10, replace = TRUE))
  df2 <- data.frame(id = 6:15, y = sample(c("a", "b"), 10, replace = TRUE))
  dflst <- list(df1 = df1, df2 = df2)
  
  expect_length(totalSubjects(dflst, idkey = "id"), 15)
})

test_that("columnLabels returns the column names if there are no labels", {
  df1 <- data.frame(id = 1:10, y = sample(c("a", "b"), 10, replace = TRUE))
  
  expect_equivalent(colnames(df1), columnLabels(df1))
})

# test_that("columnLabels works with some missing attributes", {
#   id_col <- 1:10
#   y_col <- sample(c("a", "b"), 10, replace = TRUE)
#   Hmisc::label(id_col) <- "The ID label"
#   df1 <- data.frame(id = id_col, y = y_col)
#   
#   expect_equal(columnLabels(df1), list(id = "The ID label", y = "y"))
# })
# -
# test_that("columnLabels works with duplicate attributes", {
#   id_col <- 1:10
#   y_col <- sample(c("a", "b"), 10, replace = TRUE)
#   Hmisc::label(id_col) <- "The ID label"
#   df1 <- data.frame(id = id_col, y = y_col, id_2 = id_col)
#   
#   expect_equal(columnLabels(df1), list(id = "The ID label (id)", y = "y", id_2 = "The ID label (id_2)"))
# })