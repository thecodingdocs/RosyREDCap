#' @title Add Field Transformation to the Database
#' @description
#' `r lifecycle::badge("experimental")`
#' Adds a new field transformation to the REDCap database (`project`). This
#' allows users to define custom transformations for a specific field in a form,
#' including its type, label, choices, and associated function for data
#' manipulation.
#'
#' @inheritParams REDCapSync save_project
#' @param field_name Character. The name of the field to which the
#' transformation
#' will be applied.
#' @param form_name Character. The name of the form containing the field.
#' @param field_type Character. The type of the field in REDCap (e.g., "text",
#' "checkbox", "dropdown").
#' @param field_type_R Character. The corresponding R data type for the field.
#' Default is `NA`.
#' @param field_label Character. The label for the field. Default is `NA`.
#' @param select_choices_or_calculations Character. A string specifying the
#' choices (for dropdown, radio, or checkbox fields) or calculations (for
#' calculated fields). Default is `NA`.
#' @param field_note Character. An optional note or comment for the field.
#' Default is `NA`.
#' @param identifier Character. A string indicating whether the field is an
#' identifier (e.g., "Y" for yes). Default is an empty string (`""`).
#' @param units Character. The units of measurement for the field, if
#' applicable. Default is `NA`.
#' @param data_func Function or NA. An optional function to transform or
#' validate the data in the field. Default is `NA`.
#'
#' @return
#' The updated `project` object with the field transformation added.
#'
#' @details
#' This function facilitates the addition of a new field transformation to a
#' REDCap database. The transformation includes metadata such as the field's
#' type, label, and choices, along with an optional function to process the
#' data. This is particularly useful for customizing or extending the
#' functionality of existing REDCap forms and fields.
#'
#' @seealso
#' \code{\link{save_project}} for saving the database or summaries.
#'
#' @export
add_project_field <- function(
    project,
    field_name,
    form_name,
    field_type,
    field_type_R = NA,
    field_label = NA,
    select_choices_or_calculations = NA,
    field_note = NA,
    identifier = "",
    units = NA,
    data_func = NA) {
  project <- assert_blank_project(project, silent = TRUE)
  # if(!project$data %>% is_something())stop("Must have transformed data to add new vars.")
  fields <- project$metadata$fields
  in_original_redcap <- field_name %in% fields$field_name
  if (is_something(select_choices_or_calculations)) {
    select_choices_or_calculations <- choice_vector_string(select_choices_or_calculations)
  }
  if (in_original_redcap) {
    original_fields_row <- fields[which(fields$field_name == field_name), ]
    if (missing(form_name)) form_name <- original_fields_row$form_name
    if (missing(field_type)) {
      field_type <- original_fields_row$field_type
      field_type_R <- original_fields_row$field_type_R
    }
    if (is.na(field_label)) field_label <- original_fields_row$field_label
    if (is.na(select_choices_or_calculations)) {
      select_choices_or_calculations <- original_fields_row$select_choices_or_calculations
    }
    if (is.na(field_note)) field_note <- original_fields_row$field_note
    if (identifier == "") identifier <- original_fields_row$identifier
  }
  if (!is_something(data_func)) {
    warning("if no `data_func` is provided, the column is only added to the metadata", immediate. = TRUE)
  }
  if (is_something(data_func)) {
    func_template <- "data_func = function(project,field_name){YOUR FUNCTION}"
    if (!is.function(data_func)) {
      stop("`data_func` must be a function ... ", func_template)
    }
    allowed_args <- c("project", "field_name", "form_name")
    if (!any(allowed_args %in% names(formals(data_func))) ||
        !all(names(formals(data_func)) %in% allowed_args)) {
      stop("`data_func` must have two arguments (project and field_name) ... ",
           func_template)
    }
  }
  field_row <- data.frame(
    field_name = as.character(field_name),
    form_name = as.character(form_name),
    field_type = as.character(field_type),
    field_label = as.character(field_label),
    select_choices_or_calculations = as.character(select_choices_or_calculations),
    field_note = as.character(field_note),
    identifier = as.character(identifier),
    field_type_R = as.character(field_type_R),
    units = as.character(units),
    in_original_redcap = as.logical(in_original_redcap),
    field_label_short = as.character(field_label),
    field_func = function_to_string(data_func),
    stringsAsFactors = FALSE
  )
  row_match <- which(project$transformation$fields$field_name == field_name)
  included_already <- length(row_match) > 0
  if (included_already) {
    compare_this <- project$transformation$fields[row_match,]
    rownames(compare_this) <- NULL
    rownames(field_row) <- NULL
    if (identical(field_row,compare_this)) {
      # should there be a message?
      return(invisible(project)) #return if the same
    }
  }
  project$transformation$fields <- project$transformation$fields[which(project$transformation$fields$field_name != field_name), ]
  project$transformation$fields <-
    project$transformation$fields %>%
    dplyr::bind_rows(field_row)
  project$transformation$field_functions[[field_name]] <-
    data_func %>% clean_function()
  if(length(project$summary$all_records$was_transformed)>0){
    project$summary$all_records$was_transformed <- FALSE
  }
  message("added '", field_name, "' column")
  invisible(project)
}
#' @noRd
add_default_fields <- function(project) {
  forms <- project$metadata$forms
  last_non_rep <- forms$form_name[which(!forms$repeating)] %>% dplyr::last()
  form_names <- forms$form_name[which(forms$repeating)]
  if (!project$metadata$is_longitudinal) {
    has_non_rep <- length(last_non_rep) > 0
    if (has_non_rep) {
      for (form_name in form_names) {
        form_label <- forms$form_label[which(forms$form_name == form_name)]
        project <- project %>% add_project_field(
          field_name = paste0("n_forms_", form_name),
          form_name = last_non_rep,
          field_type = "text",
          field_type_R = "integer",
          field_label = paste0(form_label, " Forms"),
          units = "n",
          data_func = function(project, field_name, form_name) {
            form <- gsub("n_forms_", "", field_name)
            #need another way to count if multiple id cols
            id_col <- project$metadata$form_key_cols[[form_name]]
            final_vector <- project$data[[form_name]][[id_col]] %>%
              find_match(project$data[[form]][[id_col]], count_only = TRUE) %>%
              as.character()
            final_vector
          }
        )
      }
    }
  }
  for (form_name in form_names) {
    form_label <- forms$form_label[which(forms$form_name == form_name)]
    project <- project %>% add_project_field(
      field_name = paste0(form_name, "_compound_key"),
      form_name = form_name,
      field_type = "text",
      field_type_R = "character",
      field_label = paste(form_label, "Compound Key"),
      data_func = function(project, field_name, form_name) {
        col_names <- project$metadata$form_key_cols[[form_name]]
        form <- NULL
        while (length(col_names) > 0) {
          if (is.null(form)) {
            form <- project$data[[form_name]][[col_names[1]]]
          } else {
            form <- form %>% paste0("_", project$data[[form_name]][[col_names[1]]])
          }
          col_names <- col_names[-1]
        }
        form
      }
    )
  }
  invisible(project)
}
#' @noRd
combine_project_fields <- function(project) {
  the_names <- project$transformation$fields$field_name
  fields <- project$metadata$fields
  if (is.null(the_names)) {
    cli_alert_danger("Nothing to add. Use `add_project_field()`")
    return(fields)
  }
  for (field_name in the_names) {
    field_row <- project$transformation$fields[which(project$transformation$fields$field_name == field_name), ]
    form_name <- field_row$form_name
    # if(any(fields$field_name==field_name))stop("field_name already included")
    current_row <- which(fields$field_name == field_name)
    if (length(current_row) > 0) {
      fields <- fields[-current_row, ]
      i <- current_row
      if (i > 1) i <- i - 1
    } else {
      i <- which(fields$form_name == form_name & fields$field_name == paste0(form_name, "_complete"))
      if (length(i) > 0) {
        if (i[[1]] > 1) {
          i <- i - 1
        }
      }
      if (length(i) == 0) {
        i <- which(fields$form_name == form_name)
      }
      if (length(i) > 1) {
        i <- i[[1]]
      }
      if (length(i) == 0) i <- nrow(fields)
    }
    if (length(i) == 0) stop("insert_after error")
    top <- fields[1:i, ]
    bottom <- NULL
    if (i < nrow(fields)) bottom <- fields[(i + 1):nrow(fields), ]
    fields <- top %>%
      dplyr::bind_rows(field_row) %>%
      dplyr::bind_rows(bottom)
  }
  fields
}
#' @rdname default-transformations
#' @title Add Default Forms Transformation to the Database
#' @description
#' `r lifecycle::badge("experimental")`
#' Applies default transformations to specific forms within the REDCap database
#' (`project`).
#' This function modifies the `project` object to include default
#' transformations, which may
#' involve adjustments, calculations, or reformatting of data in predefined
#' forms.
#'
#' @inheritParams REDCapSync save_project
#' @param forms_transformation a data.frame that matches instruments. See
#' `default_project_transformation` for an example.
#' @return
#' The updated `project` object with default transformations applied to the
#' specified forms.
#'
#' @details
#' This function is designed to streamline and standardize data processing by
#' applying default transformations to the database forms. The transformations
#' are predefined within the function and ensure consistency across datasets.
#'
#' @seealso
#' \code{\link{save_project}} for saving the database or summaries.
#' @export
add_project_transformation <- function(project,
                                       forms_transformation) {
  lifecycle::signal_stage("experimental", "add_project_transformation()")
  if (missing(forms_transformation)) {
    forms_transformation <- default_project_transformation(project)
  }
  forms_tranformation_cols <- c(
    "form_name",
    "form_label",
    "repeating",
    "form_name_remap",
    "form_label_remap",
    "merge_to",
    "by.x",
    "by.y",
    "x_first"
  )
  if (project$metadata$is_longitudinal) {
    forms_tranformation_cols <- forms_tranformation_cols %>%
      append("repeating_via_events")
  }
  if (!all(names(forms_transformation) %in% forms_tranformation_cols)) {
    cli_alert_wrap(
      "Use `add_default_forms_transformation(project)` is an example!"
    )
    stop(
      "forms_transformation needs the following colnames... ",
      forms_tranformation_cols %>% toString()
    )
  }
  if (!is.null(project$transformation$forms)) {
    if (identical(project$transformation$forms, forms_transformation)) {
      #message
      return(invisible(project))
    }
  }
  # add more checks
  project$transformation$forms <- forms_transformation
  project$summary$all_records$was_transformed <- FALSE
  invisible(project)
}
