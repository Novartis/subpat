#' Generic function to query
#' @export
query <- function(x, ...) {
  UseMethod("query", x)
}

#' Generic function to query subjects
#' @export
querySubjects <- function(x, ...) {
  UseMethod("querySubjects", x)
}

#' Constructor for "PopulationFilter" class. A list of filters with more specific functionality
#' 
#' @param datasets the named list of data frames we want to filter.
#' @param name the name of the population filter
#' @param initFilters a list of DatasetFilters
#' @param createDate the data this filter was created
#' @param editData the date this filter was last edited
#' @return PopulationFilter object
#' @export
PopulationFilter <- function(dataset_names, name = NULL, initFilters = list(), createDate = date(), editDate = date()) {
  fc <- list(
    dataset_names = dataset_names,
    filters = initFilters,
    name = name,
    createDate = createDate,
    editDate = editDate
  )
  class(fc) <- "PopulationFilter"
  fc
}

toString.PopulationFilter <- function(x, ...) {
  res <- paste0("Data sets: ", paste0(x$dataset_names, collapse = ", "))
  filter_strings <- paste0("\t", lapply(x$filters, toString), collapse = "\n\t")
  res <- paste(res, "Filters:", filter_strings, sep = "\n")
  res
}

#' Returns a population filter as pandoc
#' @param popfilter PopulationFilter
#' @export
toPandoc <- function(popfilter) {
  
  pander::pandoc.header(paste0("Sub-population: ", popfilter$name), level = 2)
  pander::pandoc.p(paste0("Create date: ", popfilter$createDate))
  pander::pandoc.p(paste0("Last edit: ", popfilter$editDate))
  pander::pandoc.header("Filters", level = 3)
  pander::pandoc.list(
    lapply(popfilter$filters, toString)
  )
}

#' Gets a string of the variable names in the population filter
#' @param popfilter the PopulationFilter
#' @export
getFilterVarNames <- function(popfilter) {
  if(length(popfilter$filters) == 0) {
    "No filters"
  } else {
    paste0(purrr::map(popfilter$filters, "variable"), collapse = ", ")
  }
}

#' Query the population filter
#' @param popfilter The PopulationFilter object
#' @param datasets a list of data sets to query
#' @param idkey the id key common to all data sets
#' @export
query.PopulationFilter <- function(popfilter, datasets, idkey = "USUBJID", commonTable = NULL) {
  subjects <- querySubjects.PopulationFilter(popfilter, datasets, idkey = idkey, commonTable = commonTable, mergeResults = TRUE)
  lapply(datasets, function(ds) {
    dplyr::filter(ds, get(idkey) %in% subjects)
  })
}

#' Gets all ids that match the current filters in the given filter collection
#' This applies all the filters for a given dataset only to that dataset.
#' Use \code{query.PopulationFilter} make sure filters apply cross-dataset
#' 
#' @param PopulationFilter PopulationFilter object
#' @param idkey id key name for the filter
#' @param commonTable a string of the table name in the filter collection that contains all metadata about the subjects (E.g. ADSL)
#' @return ids that match the current filter in `PopulationFilter`
#' @export
queryIndividual.PopulationFilter <- function(popfilter, datasets, idkey = "USUBJID", commonTable = NULL) {
  # Initialize with all the datasets
  if(!is.null(commonTable)) {
    commonIndex <- grep(commonTable, names(datasets))
    if(length(commonIndex) > 1) {
      warning("Found more than one common table in datasets...")
      warning(names(datasets)[commonIndex])
    } else if(length(commonIndex) == 0) {
      warning("Common table not found in dataset!")
      results <- list()
    } else if(length(commonIndex) == 1) {
      results <- datasets[commonIndex]
    }
  } else {
    queriedDatasets <- unlist(purrr::map(popfilter$filters, "dataset_name"))
    
    # No filters, return back the data sets
    if(length(queriedDatasets) == 0) return(NULL)
    
    # Only use the datasets that are actually queried
    results <- datasets[queriedDatasets]
  }
  
  filters <- popfilter$filters
  for(f in filters) {
    if(is.null(f$dataset_name) || is.null(f$variable)) next;
    
    # Query each using the previous results as the dataset
    # This is the equivalent of doing intersections
    
    results[[f$dataset_name]] <- query(f, dataset = results[[f$dataset_name]] %||% datasets[[f$dataset_name]])
  }
  
  return(results)
}

#' Query the subjects from a population filter
#' @param popfilter the PopulationFilter object
#' @param datasets the datasets to filter on
#' @param idkey the subject identification key in the 
#' @param mergeResults Boolean, should the results from each data set be merged? If TRUE, returns a single vector of subject ids
#' If FALSE, will return a list with keys of the data set names and a vector of ids for each data set
#' @export
querySubjects.PopulationFilter <- function(popfilter, datasets, idkey = "USUBJID", commonTable = NULL, mergeResults = TRUE) {
  if(is.null(datasets)) return(NULL)
  
  # If we have a null population filter, return all subject ids from the data sets
  if (is.null(popfilter) || length(popfilter$filters) == 0) {
    if(length(datasets) == 1) {
      return(unique(datasets[[1]][[idkey]]))
    } else {
      return(Reduce(union, purrr::map(datasets, idkey)))
    }
  }
  
  res <- queryIndividual.PopulationFilter(popfilter, datasets, idkey, commonTable)
  rtn <- NULL
  
  if(!is.null(res)) {
    # Get a list of each of the filtered ids
    if(length(datasets) == 1) {
      return(unique(res[[1]][[idkey]]))
    } else {
      idResults <- purrr::map(res, idkey) %>%
        purrr::map(., unique)
    }
    
    # Return the entire result list
    if (! mergeResults) {
      return(idResults)
    }
    
    # Get the intersection of all ids if we have filters applied
    if (length(popfilter$filters) > 0) {
      rtn <- Reduce(intersect, idResults)
    } else if (length(popfilter$filters) == 0) {
      if (! is.null(commonTable)) {
        # Match the common table name
        commonIndex <- grep(commonTable, names(datasets))
        
        if(length(commonIndex) > 1) {
          warning("Found more than one common table in datasets...")
          warning(names(datasets)[commonIndex])
        } else if(length(commonIndex) == 0) {
          warning("Common table not found in dataset!")
          rtn <- rtn <- Reduce(union, idResults)
        } else if(length(commonIndex) == 1) {
          commonName <- names(datasets)[[commonIndex]]
          rtn <- res[[commonName]][[idkey]]
        }
      } else {
        # If we don't have a common table, take the UNION between all the datasets on the idkey
        # TODO: I think this should actually be an intersection
        rtn <- Reduce(union, idResults)
      }
    }
  } 
  
  if(!is.null(rtn)) {
    if (! mergeResults) {
      # Return the non-merged results with the filters applied to each dataset
      # Intersect the final subject list with each of the id results
      return(purrr::map(idResults, ~ intersect(.x, rtn)))
    } else {
      return(rtn)
    }
  }
}

#' Create object for data set filter
#' 
#' @param dataset_name data set name  to filter on
#' @param variable name of variable from `dataset_name` that we are filtering on
#' @param filterType One of "categorical", "date", or "numeric"
#' @param catValues If categorical is selected for \code{filterType} should provide a vector of choices to select. A subject will match if \code{variable} is in \code{catValues}
#' @param minVal the minimum value to match if \code{filterType} is "date" or "numeric". Selected values greater than or equal to \code{minVal}
#' @param maxVal the maximum value to match if \code{filterType} is "date" or "numeric". Selected values less than or equal to \code{maxVal}
#' @param includeMissing Boolean. Should we include missing values? Defaults to \code{FALSE}. 
#' @param onlyMissing Boolean. Should we only include subjects with missing values from \code{variable}? Defaults to \code{FALSE}. Need to provide the value filter. If "categorical" type is given, then catValues must be provided. If `date` or `numeric` is given, one or both of minVal or maxVal should be provided.
#' @export
DatasetFilter <- function(dataset_name, variable = NULL, filterType = c("categorical", "date", "numeric"),
                          catValues = NULL, minVal = NULL, maxVal = NULL,
                          includeMissing = FALSE, onlyMissing = FALSE) {
  if (length(filterType) != 1) stop("Can only create with one filter type: 'categorical', 'date', 'numeric'")
  
  if(filterType %in% c("numeric", "date") && !missing(minVal) && !missing(maxVal) && minVal > maxVal) {
    stop("min value is greater than the max value")
  }
  
  filterObj <- list()
  filterObj$dataset_name <- dataset_name
  
  filterObj$variable <- variable
  
  filterObj$filterType <- filterType
  filterObj$catValues <- catValues
  filterObj$minVal <- minVal
  filterObj$maxVal <- maxVal
  filterObj$includeMissing <- includeMissing
  filterObj$onlyMissing <- onlyMissing
  # Add current time for creation date
  filterObj$createDate <- date()
  
  class(filterObj) <- "DatasetFilter"
  
  filterObj
}

#' Query the dataset filter and get back the data set with filtered rows
#' @param x the DatasetFilter
#' @param dataset the dataset (data frame, tibble, etc.) that we want to filter
#' @export
#' @importFrom dplyr filter intersect
query.DatasetFilter <- function(x, dataset, ...) {
  if(is.null(x)) return(NULL)
  
  if(!is.null(dataset)) {
    res <- dataset
  } else {
    return(NULL)
  }
  
  if(x$onlyMissing) {
    res <- res %>% filter(is.na(get(x$variable)))
  } else {
    if(x$filterType == "categorical" && length(x$catValues) > 0) {
      # If we are including missing, then add the na values as well.
      res <- res %>% filter(get(x$variable) %in% x$catValues | (x$includeMissing & is.na(get(x$variable))))
    } else if (x$filterType %in% c("date", "numeric")) {
      if (! is.null(x$minVal)) {
        res <- res %>% filter(get(x$variable) >= x$minVal | (x$includeMissing & is.na(get(x$variable)))) %>% intersect(res)
      }
      if (! is.null(x$maxVal)) {
        res <- res %>% filter(get(x$variable) <= x$maxVal| (x$includeMissing & is.na(get(x$variable)))) %>% intersect(res)
      }
    }
  }
  
  res
}

#' Get the unique patients that match the given filter
#' @param x a DatasetFilter
#' @export
querySubjects.DatasetFilter <- function(x, dataset, idkey = "USUBJID", ...) {
  if(!x$onlyMissing && (is.null(x$catValue) || x$catValue == "") && is.null(x$minVal) && is.null(x$maxVal)) {
    if(! is.null(dataset)) {
      # Return all the subjects if we have an empty filter
      unique(dataset[, idkey, drop = TRUE])
    }
    else {
      NULL
    }
  } else {
    res <- query.DatasetFilter(x, dataset, idkey, ...)
    if(nrow(res) > 0) {
      unique(res[, idkey, drop = TRUE])
    }
  }
}

#' toString implemention for DatasetFilter class
#' @param label boolean indicating whether or not we want to use the SAS label or variable name. Defaults to the short variable name.
#' @export
toString.DatasetFilter <- function(x, label = FALSE, ...) {
  if(is.null(x)) return(NULL)
  
  if(label && !is.null(x$label)) {
    variable <- x$label
  } else {
    variable <- x$variable
  }
  
  res <- ""
  
  if (x$filterType == "categorical") {
    if (length(x$catValues) == 1) {
      res <- paste(variable, "=", x$catValues) 
    } else if (length(x$catValues) > 1) {
      res <- paste0(variable, " in (", paste(x$catValues, collapse=", "), ")")
    } else {
      # No categorical variables selected
      res <- paste0(variable, "(Nothing selected)")
    }
  } else {
    if(! is.null(x$minVal)) {
      res <- paste0(x$minVal, " <= ")
    }
    res <- paste0(res, x$variable)
    if(! is.null(x$maxVal)) {
      res <- paste0(res, " <= ", x$maxVal)
    }
  }
  
  if(x$onlyMissing) {
    res <- paste("Missing", variable)
  } else if (x$includeMissing) {
    res <- paste(res, "(With NA)")
  } else {
    res <- paste(res, "(Remove NA)")
  }
  
  res
}

#' print implemention for DatasetFilter class
#' @export
print.DatasetFilter <- function(x, ...) {
  print(paste(x$dataset_name, toString(x), sep=": "))
}

#' Convert a DataFilter to data frame
#' @export
as.data.frame.DatasetFilter <- function(x, ...) {
  data.frame(dataset = x$dataset_name, variable = x$variable, query = toString(x))
}