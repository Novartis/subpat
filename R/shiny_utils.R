#' Creates the control column html buttons prefixed with a given id
#'
#' When a delete button is pressed, `deletePopulationClicked` will change
#' When the edit button is pressed, `editPopulationClicked` will change
#' 
#' The checkbox will have id ns(id)_checkbox_index
#' The delete buttons will have id ns(id)_delete_button_index
#' The edit buttons will have id ns(id)_edit_button_index
#' @param ns the namespace (defaults to no namespace)
#' @param id the id (without namespace)
#' @param len the number of control elements to create
population_control_column <- function(ns = NS(NULL), id, len) {
  if(is.null(len)) {
    return(character(0))
  }
  
  
  f <- function(i) {
    toString(
      tagList(
        # Checkboxes
        actionButton(
          paste(ns(id), "check_button", i, sep = "_"),
          label = NULL,
          icon = tags$i(class = "fa-square far"),
          # Add the date to each event so that it prevents shiny from optimizing the events
          # Would be fixed if we could use shiny v1.1 
          # https://shiny.rstudio.com/articles/communicating-with-js.html
          onclick = paste0('$("#" + this.id + " .fa-square").toggleClass("fa-square fa-check-square")
                    $("#" + this.id).toggleClass("checked")
                    Shiny.onInputChange(\"', ns("checkboxClicked"), '\"', ', {id: this.id, classes: this.className, time: Date()})
                   ')
        ),
        actionButtonStatus(
          paste(ns(id), "delete_button", i, sep = "_"),
          label = NULL,
          icon = icon('trash-alt'),
          status = "danger",
          onclick = paste0('Shiny.onInputChange(\"', ns("deletePopulationClicked"), '\"', ',this.id)')
        ),
        actionButton(
          paste(ns(id), "edit_button", i, sep = "_"),
          label = NULL,
          icon = icon('edit'),
          onclick = paste0('Shiny.onInputChange(\"', ns("editPopulationClicked"), '\"', ', this.id)')
        )
      )
    )
  }
  
  unlist(lapply(seq_len(len), f))
}

#' Creates a data table with controls in a column
#' Warning: The created table has `escape = FALSE` by default since we are generating HTML code
#' 
#' @param df The data frame we want to add the control column to
#' @param ns Namespace function
#' @param id id of the created columns 
#' @param ... The options to be passed into datatable.
datatableControls <- function(df, id, ns = NS(''), ...) {
  if(is.null(df) || nrow(df) == 0) DT::datatable(NULL)
    
  df_controls <- cbind(
    controls = population_control_column(ns, id, nrow(df)),
    df
  )
  
  DT::datatable(df_controls, escape = FALSE, ...)
}

#' Creates the patient count badge
#' @param p the selected number of patients
#' @param total the total number of subjects
#' @param as.string if TRUE returns the html as a character
patientCountBadge <- function(p, total, name = NULL, as.string = FALSE) {
  name_str <- paste0("(", name, ")")
  badge <- bs4Badge(paste(p, "/", total, "patients", ifelse(is.null(name), "", name_str)), status = "success")
  if(as.string) {
    as.character(badge)
  } else {
    badge
  }
}


#' Copied from shiny input-action.R
#' This is not exposed in shiny, but need it to reimplement actionButton
validateIcon <- function(icon) {
  if (is.null(icon) || identical(icon, character(0))) {
    return(icon)
  } else if (inherits(icon, "shiny.tag") && icon$name == "i") {
    return(icon)
  } else {
    stop("Invalid icon. Use Shiny's 'icon()' function to generate a valid icon")
  }
}

#' Copied from shiny::actionButton, but provide support for status
#' 
#' @param status Button color: "primary", "danger", "info", "success", "warning", "secondary", "dark" or "light"
actionButtonStatus <- function (inputId, label, icon = NULL, width = NULL, status = "default", ...) 
{
  value <- restoreInput(id = inputId, default = NULL)
  tags$button(id = inputId, style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), type = "button",
    class = paste("btn", paste0("btn-", status),  "action-button"), `data-val` = value, 
    list(validateIcon(icon), label), ...)
}

bs4AccordionItemExpanded <- function (..., id, title = NULL, status = NULL, width = 12, expanded = FALSE) 
{
  accordionItemCl <- "card"
  if (!is.null(status)) 
    accordionItemCl <- paste0(accordionItemCl, " card-", 
                              status)
  headerTag <- shiny::tags$div(class = "card-header",
                               id = paste0("header_", id),
                               shiny::tags$h4(class = "card-title",
                                              shiny::tags$a(href = paste0("#", id),
                                                            `aria-controls` = id,
                                                            `aria-expanded` = tolower(as.character(expanded)),
                                                            `data-target` = paste0("#",id),
                                                            `data-toggle` = "collapse",
                                                            title)))
  bodyTagClass <- paste0("panel-collapse collapse in", ifelse(expanded, " show", ""))
  bodyTag <- shiny::tags$div(`aria-labelledby` = paste0("header_", 
                                                        id), id = id, class = bodyTagClass, shiny::tags$div(class = "card-body", 
                                                                                                                            ...))
  accordionItemTag <- shiny::tags$div(class = accordionItemCl)
  accordionItemTag <- shiny::tagAppendChildren(accordionItemTag, 
                                               headerTag, bodyTag)
  shiny::tags$div(id = paste0(id, "_outer"),
                  class = if (!is.null(width)) 
    paste0("col-sm-", width), accordionItemTag)
}

bs4Badge <- function(...) {
  bs4Dash::bs4Badge(...)
}

bs4Accordion <- function (..., id) {
  items <- list(...)
  len <- length(items)
  lapply(seq_len(len), FUN = function(i) {
    items[[i]]$children[[1]]$children[[2]]$attribs[["data-parent"]] <<- paste0("#", 
                                                                               id, "accordion")
  })
  shiny::tags$div(id = paste0(id, "accordion"), items)
}

#' Create a simple page
bs4SimplePage <- function(...) {
  bs4DashPage(
    sidebar = bs4DashSidebar(disable = TRUE),
    body = bs4DashBody(...)
  )
}

myLoading <- function() {
  bs4Dash::bs4Loading()
}