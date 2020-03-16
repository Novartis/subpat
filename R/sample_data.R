#' @export
filter_sample <-  DatasetFilter(dataset_name = "ADSL", variable = "a", filterType = "categorical", catValues = c("x", "y"))

#' @export
example_datasets <- list(
  "ADSL" = data.frame(
    a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
    b = c(29L, 4L, 15L, 28L, 41L, 25L, 41L, 15L, 6L, 17L),
    USUBJID = as.character(seq_len(10)),
    stringsAsFactors = FALSE
  ),
  "ADAE" = data.frame(
    a = c("x", "z", "y", "x", "z", "x", "x", "y", "x", "y"),
    c = c(1, NA, NA, 1, 2, NA, NA, 4, 3, 1),
    dates = seq(as.Date("2019-07-10"), as.Date("2019-08-12"), length.out = 10),
    USUBJID = as.character(seq_len(10)),
    stringsAsFactors = FALSE
  )
)

#' @export
example_populations <- list(
  # Population 1
  structure(list(dataset_names = c("ADSL", "ADAE"), filters = structure(list(
    filter_1 = structure(list(dataset_name = "ADAE", variable = "c", 
                              filterType = "numeric", includeMissing = FALSE, onlyMissing = FALSE, 
                              createDate = "Fri Jul 26 18:44:11 2019", minVal = 1L, 
                              maxVal = 4L), .Names = c("dataset_name", "variable", 
                                                       "filterType", "includeMissing", "onlyMissing", "createDate", 
                                                       "minVal", "maxVal"), class = "DatasetFilter")), .Names = "filter_1"), 
    name = "New population: 1", createDate = "Fri Jul 26 18:44:05 2019", 
    editDate = "Fri Jul 26 18:44:13 2019"), .Names = c("dataset_names", 
                                                       "filters", "name", "createDate", "editDate"), class = "PopulationFilter")
)
