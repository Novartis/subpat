#' Returns the common columns of a list of data frame (or other objects that has column names)
#'
#' @param datalist list (or similar) of data frame (or similar) with column names
#' @return A list with two keys: `common` that contains the common names as well as 
commonColumns <- function(datalist) {
  allCols <- lapply(datalist, colnames)
  
  commonCols <- Reduce(intersect, allCols)
  # nonCommonCols <- lapply(allCols, setdiff, y = commonCols)
  commonCols
}

#' Create a 'subject level' table from a list of data sets
#' Assume that the data sets have some common variables
#' Extract these variables into a new data set
#' 
#' @param datalist a list of data sets
createCommonTable <- function(datalist) {
  commoncol <- commonColumns(datalist)
  
  if(length(commoncol) == 0) {
    return(NULL)
  }
  

  
  # Extract only the common columns from each data set
  onlyCommon <- lapply(datalist, function(x) {
    x[, commoncol, drop = FALSE]
  })

  # Join into one data set
  warnlevel <- options()$warn
  mergedCommon <- tryCatch({
      options(warn = 2)
      do.call(rbind, onlyCommon)
    },
    error = function(cond) {
      return(NULL)
    },
    warning = function(cond) {
      return(NULL)
    },
    finally = {
      options(warn = warnlevel)
    }
  )
  
  mergedCommon
}

#' Check if a value is a date
#' 
#' From: https://stackoverflow.com/a/37062951/1351718
#' 
#' @param x object
#' @return boolean indicating if x is a Date or not.
is.date <- function(x) inherits(x, 'Date')

#' Extract the row number from a id in a datatable when row number is the last digit in the id
#' 
#' @examples
#' \dontrun{
#' extractRowNumber("population-population_card-controls_delete_button_2")
#' }
extractRowNumber <- function(idstr) {
  match <- gsub(".+([0-9])+$", "\\1", idstr)
  as.integer(match)
}

# Adds quotes around a given variabl
addQuotes <- function(x, escape = TRUE) {
  if(escape) paste0('\"', x, '\"')
  else paste0('"', x, '"')
}

#' Check if a given css class is in the class string
#' 
#' @param css_class The class we are looking for
#' @param class_string The class string that we want to find \code{css_class} in
#' @return Boolean indicating if \code{css_class} is in \code{class_string}
hasClass <- function(css_class, class_string) {
  !is.null(class_string) && css_class %in% strsplit(class_string, " ")[[1]]
}

#' Gets the labels from a SAS data set (read in by haven::read_sas)
#' Returns the column names if there are no labels
#' 
#' @param tb the tibble read in by haven::read_sas
#' @export
columnLabels <- function(tb, removeId = NULL) {
  res <- lapply(tb, attr, "label")
  null_res <- unlist(lapply(res, is.null))
  # Return the column names if all the label attributes are null
  if(all(null_res)) {
    res <- colnames(tb)
    names(res) <- res
  }
  # Use the variable names for the columns if no label
  res[null_res] <- colnames(tb)[null_res]
  
  # If we have duplicates, add the variable to the label
  dups <- res %in% res[duplicated(res)]
  res[dups] <- glue::glue("{res} ({names(res)})")[dups]
  
  # Remove the id column if removeId supplied
  if (! is.null(removeId)) {
    res[! grepl(removeId, names(res))]
  } else {
    res
  }
}

#' Gets the labels from a SAS data set in format for selectInput
#' list with key as label and value as the variable name
#' 
#' @param tb the tibble read in by haven::read_sas
#' @param removeId the name of a column to remove.
columnLabelsInvert <- function(tb, removeId = NULL) {
  clabs <- columnLabels(tb, removeId = removeId)
  res <- names(clabs)
  names(res) <- unlist(clabs)
  
  res
}

#' Gets the column from a given label
#' @param dataset the dataset (tibble, data frame)
#' @param lab the label we are looking for in the column names
#' @return the column in the data set that matches \code{lab}
#' #' @seealso \code{\link{labelToName}} which return the column \emph{name}, rather than the column itself.
labelToColumn <- function(dataset, lab) {
  m <- match(lab, columnLabels(dataset))
  dataset[[m]]
}

#' Gets the given variable name from a label
#' 
#' @param dataset the dataset (tibble, data frame)
#' @param lab the label we are looking for in the column names
#' @return the column name in the data set that match \code{lab}
#' @seealso \code{\link{labelToColumn}} which return the actual column, rather than the column name.
labelToName <- function(dataset, lab) {
  m <- match(lab, columnLabels(dataset))
  if(length(m) == 0 || is.na(m)) return(NULL)
  
  names(dataset)[[m]]
}

#' Get the total number of subjects across a list of data sets.
#' Takes the union of all 
#' @param datasets a list of data sets (data frames, tibbles)
#' @param idkey the idkey for the subjects. Default to USUBJID
#' @return a list of subjects
totalSubjects <- function(datasets, idkey = "USUBJID") {
  if(length(datasets) == 1) {
    unique(datasets[[1]][[idkey]])
  } else {
    Reduce(union, lapply(names(datasets), function(x) {
      ds <- datasets[[x]]
      # Check that the idkey is in each dataset, warning otherwise
      if(! (idkey %in% names(ds))) {
        warning("Did not find ", idkey, " in data set:", x)
      }
      ds[[idkey]]
    }))
  }
}

readPathDatasets <- function(path) {
  f <- list()
  
  # List of sas files , full name
  f$sasFiles <- list.files(path, pattern = ".sas7bdat", ignore.case = T, full.names = T)
  
  # list of sas files, short name, without extension
  fname <- tolower(list.files(path, pattern = ".sas7bdat", ignore.case = T, full.names = F))
  f$fname <- toupper(unlist(strsplit(fname,".sas7bdat")))
  
  
  # adams = short and long names of ADAM data sets
  f$adams<- data.frame(short=f$fname, long=f$fname, stringsAsFactors = F)
  
  # Matching adam specs from define.xml with adam files listed from folder
  f$fd <- data.frame(short=f$fname, long=f$fname, stringsAsFactors = F)
  f$fname <- glue::glue_data(f$fd, "{short} ({long})")
  
  f$sasDataList <- purrr::map(f$sasFiles, haven::read_sas)
  names(f$sasDataList) <- f$fname
  
  f
}

# get variable labels
getSASLabels <- function(adam){
  
  ab <- data.frame(label=unlist(purrr::map(adam, ~attr(.x, c("label")))))
  ab$var <- row.names(ab)
  list(labels=ab%>%glue::glue_data("{var} ({label})"), data=ab)
}

# Returns a if a is not null and b if a is null
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Returns numeric column names
#' @param d the data set
#' @param showLabels should the labels be returned in addition to the values?
#' If TRUE, names(list) = labels(d)
numericColNames <- function(d, showLabels = TRUE) {
  cond <- unlist(lapply(d, is.numeric))

  if(showLabels) {
    numCols <- columnLabelsInvert(d)[cond]
  } else {
    numCols <- colnames(d)[cond]
  }

  # Find the AVAL column and place in front
  # Also put time columns in front as well
  avalIndex <- grep("(^AVAL$|^time$)", numCols, ignore.case = TRUE)

  # Place the AVAL index first
  sortedIndices <- union(avalIndex, seq_along(numCols))

  # Return the numeric columns with AVAL first
  numCols[sortedIndices]
}

# Finds strata values where the unique values are <= limit
strataColNames <- function(d, limit = 10, showLabels = TRUE) {
  if(is.null(showLabels) || showLabels) {
    res <- columnLabelsInvert(d)
  } else {
    res <- colnames(d)
  }

  cond <- unlist(lapply(d, function(x) {
    length(unique(x)) <= limit
  }))

  strataCols <- res[cond]

  # Find the STR columns and place in front
  strIndices <- grep("^STRVAL", strataCols)

  # Place the strata STRVAL indices first
  sortedIndices <- union(strIndices, seq_along(strataCols))

  # Return the numeric columns with STRVAL first
  strataCols[sortedIndices]
}

#' Partitions a numerical vector into factors
#' Ensures that all values have a category (NA will still be NA)
partition <-function(x, breaks, ...) {
  if(length(breaks) > 0) {
    # Always include the min/max values
    min_x <- min(x, na.rm = TRUE)
    max_x <- max(x, na.rm = TRUE)
    min_breaks <- min(breaks, na.rm = TRUE)
    max_breaks <- max(breaks, na.rm = TRUE)
    if(min_breaks < min_x) stop("The minimum break: ", min_breaks, " is less than the minimum value: ", min_x)
    if(max_breaks > max_x) stop("The max break: ", max_breaks, " is greater than the minimum value: ", max_x)
    cut(x, breaks = unique(c(min_x, breaks, max_x)), include.lowest = TRUE, ...)
  }

}
