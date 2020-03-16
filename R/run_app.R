

#' Runs the PLG Shiny application
#' 
#' @export
runPlg <- function() {
  path <- "shiny-apps/PLG"

  shiny::runApp(system.file(path, package = "subpat"))
}

#' Runs the TTE shiny application
#' 
#' @export
runTTE <- function(useBs4Dash = FALSE) {
  path <- "shiny-apps/TTE"
  
  shiny::runApp(system.file(path, package = "subpat"))
}


#' Runs the various module examples
#' 
#' Example applications:
#' Call with index subpatExamples(2) or name subpatExamples('06_tte_simple')
#'    01_add-filter-example
#'    02_edit-population-example
#'    03_subpopulation-example
#'    04_table-listing-example
#'    05_variable-selection-example
#'    06_tte_simple
#' @param i the index or name of the example
#' @export
subpatExamples <- function(i) {
  example_folder <- system.file("shiny-examples", package = "subpat")
  example_list <- list.files(example_folder)
  if(missing(i)) {
    cat("Example applications:\n")
    cat("Call with index subpatExamples(2) or name subpatExamples('06_tte_simple')\n")
    cat(paste("\t", example_list, collapse = "\n"))
  } else {
    if(is.character(i)){
      i <- grep(i,example_list)
      if(length(i) == 0)
        stop("Provide a valid name !")
    }
    if(i < 1 || i > length(example_list))
      stop("Provide a valid index !")
      
    shiny::runApp(paste(example_folder, example_list[[i]], sep = "/"))
  }
}
