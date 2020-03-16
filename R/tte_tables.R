# Collection of functions to create time-to-event tables

#' Create the table summary for the events based on RATIFY's kmQ function
#'
#' @param fit output from \code{survival::survfit}
#' @return data frame of the event summary. Includes the total number (N), the number of events (No. Events), and number censored (No. Censored) in each strata
#' @seealso \url{https://gitlabce.statwb.eu.novartis.net/MAFE3/RATIFY/blob/ce87136a47eae7ec2d9e52df1d9879b00a05cc13/functions/31_km_functions_fm_ratify.R#L14}
#'
#' @export
#'
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data=survival::lung)
#' subpat::kmEventSummary(fit)
kmEventSummary = function(fit) {
  kmsum <- data.frame(summary(fit)$table)[, c(1,4)]
  kmsum2 <- data.frame(t(kmsum))
  kmsum2['censored',] <- kmsum2['records',] - kmsum2['events',]

  kmsum2['events',] <- paste0(kmsum2['events',], ' (', sprintf("%.1f",100*(kmsum2['events',]/kmsum2['records',]  )), ' %)')
  kmsum2['censored',] <- paste0(kmsum2['censored',], ' (', sprintf("%.1f",100*(as.numeric(kmsum2['censored',])/as.numeric(kmsum2['records',] ) )), ' %)')

  row.names(kmsum2) <- c('N', 'No. Events', 'No. Censored')

  names(kmsum2) <- rownames(kmsum)

  kmsum2
}

#' Create the table summary for the quartiles (Q1, Median, Q3), from RATIFY's kmQ function
#' @param fit output from \code{survival::survfit}
#' @return data frame of the event summary.
#' @importFrom stats quantile
#' @seealso \url{https://gitlabce.statwb.eu.novartis.net/MAFE3/RATIFY/blob/ce87136a47eae7ec2d9e52df1d9879b00a05cc13/functions/31_km_functions_fm_ratify.R#L9}
#' @export
#'
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data=survival::lung)
#' subpat::kmQuartiles(fit)
kmQuartiles = function(fit) {
  test <-data.frame(quantile(fit, probs = c(0.25, 0.5, 0.75), conf.int = TRUE))
  test$Q1 <- formatCI(test$quantile.25, test$lower.25, test$upper.25)
  test$Median <- formatCI(test$quantile.50, test$lower.50, test$upper.50)
  test$Q3 <- formatCI(test$quantile.75, test$lower.75, test$upper.75)
  testx <- data.frame(t(test[, 10:12]))

  names(testx) <- row.names(summary(fit)$table)

  testx
}

#' Creates a event table from a survival model.
#' Return a table with cumulative number of  events, the number at risk, and the survival probability with confidence interval.
#'
#' @importFrom dplyr group_by mutate
#' @param fit output from \code{survival::survfit}
#' @param times Optional vector of times to show in the table. By default will include equally spaced times from the minimum event time to the maximum event time with 10 time points.
#' @return kable output of the KM event table from survfit \code{fit} at time points \code{times}
#'
#' @export
#'
#' @examples
#' library(survival)
#' fit <- survfit(Surv(time, status) ~ sex, data=survival::lung)
#' subpat::kmEventTables(fit)
kmEventTables <- function(fit, times = round(seq(min(fit$time), ceiling(max(fit$time)), length.out = 10))) {
  sf <- summary(fit, times = times)

  # Create a data frame based on fit summary
  # group by the strata variable
  d <- data.frame(strata = sf$strata,
                  time = sf$time,
                  events = sf$n.event,
                  at_risk = sf$n.risk,
                  ci = sprintf("%.1f [%.1f %.1f]", sf$surv * 100, sf$lower * 100, sf$upper * 100)) %>%
    group_by(strata) %>%
    # Make events be cumulative
    mutate(events = cumsum(events))
  
  # Split the data sets based on strata and
  # remove the strata column (first column) from each data set
  # Strata name is still available in the names of the list
  spd <- lapply(split(d, f = d$strata), function(x) x[, -1])

  # Remove one for Time variable and one for Strata
  table_ncols <- ncol(d) - 2
  strata_count <- length(spd)

  # Header for the confidence interval string
  ci_string <- paste0("Survival Prob. [", round(fit$conf.int * 100, 2), "% CI]")

  tab_colnames <- c("Cum. Events", "At Risk", ci_string)

  # Join based on the "time" column
  joined_spd <- Reduce(function(x, y) full_join(x, y, by = "time"), spd)

  htmlTable::htmlTable(joined_spd,
                       rnames = FALSE,
                       css.cell = "padding-left: 1.5em; padding-right: 1.5em;",
                       col.rgroup = c("none", "#F7F7F7"),
                       # Lower header
                       header= c("Time", rep(tab_colnames, strata_count)),
                       # Upper header: The treatment names
                       cgroup= c(" ", names(spd)),
                       n.cgroup=c(1,3,3),
                       css.table = "margin-top: 1em; margin-bottom: 1em; width: 850px; ")
}


#' Format confidence interval
#' 
#' @param val The point estimate for the confidence inteval
#' @param lower The lower bound of the confidence interval
#' @param upper The upper bound of the confidence interval
#' @param digits the number of digits after the decimal place. Default 1
#' @return string "val [lower, upper]"
formatCI <- function(val, lower, upper, digits = 1) {
  if(all(is.na(c(val, lower, upper)))) {
    "NA"
  } else {
    format_str <- sprintf("%%.%df [%%.%df %%.%df]", digits, digits, digits)
    sprintf(format_str, val, lower, upper)
  }
}


#' Create the hazard ratio table
#'
coxphHazardTable <- function(cox_mod, dataset) {

  coefs <- broom::tidy(cox_mod, exponentiate = TRUE)
  covariates <- attr(cox_mod$terms,"term.labels")

  covariates <- Filter(Negate(function(x) grepl("strata\\(", x)), covariates)
  names(covariates) <- covariates

  num_covariates <- unlist(lapply(covariates, function(x) is.numeric(dataset[[x]])))

  covariate_levels <- unlist(lapply(covariates, function(x) {
    if(is.numeric(dataset[[x]])) {
      2
    } else {
      length(cox_mod$xlevels[[x]])
    }
  }))
  # covariates replicated for each of the levels
  covar_rep <- unlist(lapply(seq_along(covariates), function(i) rep(covariates[[i]], covariate_levels[[i]] - 1)))

  # Split based on the factor lengths
  covar_coeffs <- split(coefs, cut(seq_len(nrow(coefs)), breaks = c(0, cumsum(covariate_levels - 1))))

  ref_cats <- vector("character", length(num_covariates))
  # Get the first element of the level
  ref_cats[!num_covariates] <- lapply(covariates[!num_covariates], function(x) cox_mod$xlevels[[x]][[1]])

  # Add Reference: to the reference categories
  ref_cats[ref_cats != ""] <- paste0("Reference: ", ref_cats[ref_cats != ""])

  rgroup <- gsub("[[:space:]]", "&nbsp;", sprintf("%-15s %-20s", covariates, ref_cats))

  # Add to last column
  # attr(covariates, "add") <- ref_cats

  new_terms <- lapply(mapply(strsplit, x = coefs$term, split = covar_rep), function(x) {
    if(length(x) > 1) {
      # Indent the rows...
      paste0("&nbsp;&nbsp;", x[[2]])
    } else {
      x[[1]]
    }
  })

  coefs$term <- new_terms

  coefs[, -1] <- round(coefs[, -1], 3)

  tab <- data.frame(variable = unlist(coefs$term), hr = formatCI(coefs$estimate, coefs$conf.low, coefs$conf.high, digits = 2))

  htmlTable::htmlTable(tab,
                       rnames = FALSE,
                       align = paste(c("l", rep("c", ncol(tab) - 1)), collapse = ""),
                       header= c("Variable", "HR [95% CI]"),
                       n.rgroup = covariate_levels - 1,
                       escape.html = FALSE,
                       rgroup = rgroup,
                       css.cell = "padding-left: 1.5em; padding-right: 1.5em;",
                       css.table = "margin-top: 1em; margin-bottom: 1em; width: 850px; ")
}
