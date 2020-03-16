context("test-dataset_filters.R")

test_that("query.DatasetFilter works", {
  datasets <- list(
    "my_dataset" = data.frame(
      a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
      b = c(29L, 4L, 15L, 28L, 41L, 25L, 41L, 15L, 6L, 17L),
      USUBJID = as.character(seq_len(10)),
      stringsAsFactors = FALSE
    )
  )
  dsfilter <- DatasetFilter("my_dataset", variable = "a", filterType = "categorical", catValues = c("x", "y"))
  
  query_res <- query(dsfilter, datasets[["my_dataset"]])
  expect_equal(nrow(query_res), 8)
  
  expect_equivalent(query_res$USUBJID, c("1", "3", "4", "6", "7", "8", "9", "10"))
  
  # Only use minimum value
  dsfilter2 <- DatasetFilter("my_dataset", variable = "b", filterType = "numeric", minVal = 25)
  query_res2 <-query(dsfilter2, datasets[["my_dataset"]])
  
  expect_equal(nrow(query_res2), 5)
  
  # Only use maximum value
  dsfilter3 <- DatasetFilter("my_dataset", variable = "b", filterType = "numeric", maxVal = 25)
  query_res3 <-query(dsfilter3, datasets[["my_dataset"]])
  
  expect_equal(nrow(query_res3), 6)
  
  # Use both min/max value
  dsfilter4 <- DatasetFilter("my_dataset", variable = "b", filterType = "numeric", minVal = 25, maxVal = 28)
  query_res4 <-query(dsfilter4, datasets[["my_dataset"]])
  
  expect_equal(nrow(query_res4), 2)
})

test_that("query.DatasetFilter with missing data works", {
  my_dataset <- data.frame(
      a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
      b = c(NA, 4L, NA, 28L, 41L, NA, 41L, 15L, NA, 17L),
      USUBJID = as.character(seq_len(10)),
      stringsAsFactors = FALSE
    )
  
  dsfilter <- DatasetFilter("my_dataset", variable = "b", filterType = "numeric", onlyMissing = TRUE)
  
  query_res <- query(dsfilter, my_dataset)
  expect_equal(nrow(query_res), 4)
})