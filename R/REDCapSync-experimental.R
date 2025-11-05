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
  project <- REDCapSync:::assert_blank_project(project, silent = TRUE)
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
      field_type_R <- REDCapSync:::field_types_to_R(field_type)
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
    # field_func = function_to_string(data_func),
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
find_match <- function(x, ref, count_only = FALSE) {
  final_match <- list()
  final_match[seq_along(x)] <- NA
  next_match <- match(x, ref)
  next_match_index <- which(!is.na(next_match))
  while (length(next_match_index) > 0L) {
    final_match[next_match_index] <-
      next_match_index %>% lapply(function(index) {
        out <- NULL
        if (all(is.na(final_match[[index]]))) {
          out <- next_match[index]
        } else {
          out <- c(final_match[[index]], next_match[index])
        }
        out
      })
    ref[next_match[which(!is.na(next_match))]] <- NA
    next_match <- match(x, ref)
    next_match_index <- which(!is.na(next_match))
  }
  if (count_only) {
    final_match <- final_match %>%
      lapply(function(x) {
        if (is.na(x[1])) {
          return(NA)
        }
        length(x)
      }) %>%
      unlist()
  }
  final_match
}
#' @noRd
combine_project_fields <- function(data_list,transformation) {
  the_names <- transformation$fields$field_name
  fields <- data_list$metadata$fields
  if (is.null(the_names)) {
    cli_alert_danger("Nothing to add. Use `add_project_field()`")
    return(fields)
  }
  for (field_name in the_names) {
    field_row <- transformation$fields[which(transformation$fields$field_name == field_name), ]
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
#' @title rmarkdown_project
#' @description
#' `r lifecycle::badge("experimental")`
#' Generates an RMarkdown report for the given REDCap database (`project`
#' object). This function creates an RMarkdown file in the specified directory
#' or default directory, allowing users to create custom reports based on the
#' database content.
#'
#' @details
#' This function checks if a directory is specified, and if not, defaults to the
#' `output` folder within the project's directory. It generates the RMarkdown
#' file that can then be used for further processing or rendering into HTML,
#' PDF, or other formats.
#'
#' @inheritParams save_project
#' @param dir_other Character string specifying the directory where the
#' RMarkdown report will be saved. If not provided, it defaults to the `output`
#' directory inside the project's main directory.
#' @return A message indicating the creation of the RMarkdown report and the
#' path to the generated file.
#' @seealso
#' \code{\link[REDCapSync]{save_project}} for saving the `project` object.
#' @family db_functions
#' @export
rmarkdown_project <- function(project, dir_other) {
  if (missing(dir_other)) {
    dir <- get_dir(project) %>% file.path("output")
  } else {
    dir <- dir_other
  }
  filename <- paste0(project$short_name, "_full_summary_", gsub("-", "_", Sys.Date()), ".pdf")
  rmarkdown::render(
    input = system.file("rmarkdown", "pdf.Rmd", package = pkg_name),
    output_format = "pdf_document",
    output_file = dir %>% file.path(filename),
    output_dir = dir,
    quiet = FALSE
  )
}
#' @title Run Quality Checks
#' @inheritParams save_project
#' @return project object
#' @export
run_quality_checks <- function(project) {
  project <- assert_blank_project(project)
  if (is_something(project$quality_checks)) {
    for (qual_check in names(project$quality_checks)) {
      the_function <- project$quality_checks[[qual_check]]
      if (is.function(the_function)) {
        project <- the_function(project)
      }
    }
  }
  invisible(project)
}
#' @noRd
check_field <- function(project, form, field_name, autofill_new = TRUE) {
  form <- field_names_to_form_names(project, field_name)
  records <- form[[project$metadata$id_col]] %>% unique()
  bad_records <- records[which(!records %in% project$summary$all_records[[project$metadata$id_col]])]
  if (length(bad_records) > 0) stop("Records not included in project: ", records %>% toString())
  cols_mandatory_structure <- project$metadata$form_key_cols[[form]]
  cols_mandatory <- c(cols_mandatory_structure, field_name)
  old <- project$data[[form]][, cols_mandatory]
  old <- old[which(old[[project$metadata$id_col]] %in% records), ]
  new <- form
  missing_structure_cols <- cols_mandatory[which(!cols_mandatory %in% colnames(new))]
  col_names <- cols_mandatory[which(cols_mandatory %in% colnames(new))]
  new <- new[, col_names]
  included_records <- records[which(records %in% old[[project$metadata$id_col]])]
  if (length(missing_structure_cols) > 0) {
    included_records_many_rows <- included_records[which(unlist(lapply(included_records, function(record) {
      length(which(old[[project$metadata$id_col]] == record)) > 1
    })))]
    if (length(included_records_many_rows) > 0) stop("form is missing structural columns (", missing_structure_cols %>% toString(), ") and has ", form, " rows with multiple entries... remove them or add the intended columns: ", included_records_many_rows %>% toString())
    if ("redcap_repeat_instrument" %in% missing_structure_cols) new$redcap_repeat_instrument <- form
    if ("redcap_repeat_instance" %in% missing_structure_cols) {
      new$redcap_repeat_instance <- new[[project$metadata$id_col]] %>%
        lapply(function(record) {
          if (record %in% included_records) {
            return(old$redcap_repeat_instance[which(old[[project$metadata$id_col]] == record)])
          }
          "1"
        }) %>%
        unlist()
    }
    # add event?
  }
  z <- new %>% find_form_diff2(old, ref_cols = cols_mandatory_structure)
  if (!is.null(z)) {
    i_of_old_name_change <- which(!colnames(old) %in% cols_mandatory_structure)
    colnames(old)[i_of_old_name_change] <- paste0(colnames(old)[i_of_old_name_change], "_old")
    z_old <- z %>% merge(old, by = cols_mandatory_structure)
    # add autoallow NA
    if (nrow(z) > 0) {
      # message("fix these in REDCap --> ",paste0(out,collapse = " | "))
      choices <- c("upload new", "keep old", "manual entry", "launch redcap link only")
      for (i in seq_len(nrow(z))) {
        form <- z[i, ]
        x <- z_old[i, ]
        new_answer <- x[[field_name]]
        old_answer <- x[[paste0(field_name, "_old")]]
        ask <- TRUE
        if (autofill_new) {
          if (is.na(old_answer) && !is.na(new_answer)) {
            ask <- FALSE
          }
        }
        if (ask) {
          print.data.frame(z_old[i, ])
          choice <- utils::menu(choices, title = paste0("What would you like to do?"))
        } else {
          choice <- 1
        }
        if (choice == 1) {
          form %>%
            labelled_to_raw_form(project) %>%
            upload_form_to_REDCap(project)
          message("Uploaded: ", form %>% paste0(collapse = " | "))
        }
        if (choice == 2) {
          message("Did not change anything")
        }
        if (choice == 3) {
          project %>% link_REDCap_record(form[[project$metadata$id_col]])
          form[[field_name]] <- readline("What would you like it to be? ")
          print.data.frame(form)
          form %>%
            labelled_to_raw_form(project) %>%
            upload_form_to_REDCap(project)
        }
        if (choice == 4) { # account for repeat? instance
          project %>% link_REDCap_record(form[[project$metadata$id_col]], form, instance = form[["redcap_repeat_instance"]])
        }
      }
    }
  }
}
#' @title Edit REDCap Data While Viewing
#' @description
#' Allows for editing a specific field in a REDCap project while simultaneously
#' viewing the corresponding records and fields from other forms. Supports
#' viewing and updating records individually, with flexible field selection.
#'
#' @inheritParams save_project
#' @param optional_form Optional data frame. A data frame containing the data to
#' be edited. If not provided, the function will pull the data from the REDCap
#' database using the specified `field_name_to_change`.
#' @param records Character or numeric vector. The records to be edited. If not
#' provided, the function will use the unique values from the specified forms.
#' @param field_name_to_change Character. The field name to be changed in the
#' REDCap database.
#' @param field_names_to_view Optional character vector. A list of field names
#' to view alongside the field being edited. Defaults to `NULL`, in which case
#' only the field being changed will be viewed.
#' @param upload_individually Logical. If `TRUE`, each change is uploaded
#' individually. Default is `TRUE`.
#'
#' @return
#' A modified `project` object with changes to the specified field(s) in the
#' REDCap project.
#'
#' @details
#' This function is useful when you want to edit specific fields in a REDCap
#' project while also reviewing related data from other forms in the project.
#' The `field_name_to_change` must be provided, and you can also specify
#' additional fields to view while editing. The data is either passed through
#' `optional_form` or pulled from the project based on the provided field names.
#'
#' @seealso
#' \code{\link{save_project}} for saving the modified database.
#'
#' @export
edit_REDCap_while_viewing <- function(project,
                                      optional_form,
                                      records,
                                      field_name_to_change,
                                      field_names_to_view = NULL,
                                      upload_individually = TRUE) {
  change_form <- field_names_to_form_names(project, field_name_to_change)
  view_forms <- field_names_to_form_names(project, field_names_to_view)
  field_names_to_view <- c(field_name_to_change, field_names_to_view) %>% unique()
  # if(length(view_forms)>1)stop("only one form combinations are allowed.")
  if (missing(records)) records <- project$data[[view_forms]][[project$metadata$id_col]] %>% unique()
  # all_forms <- c(change_form, view_forms) %>% unique()
  ref_cols_change <- project$metadata$form_key_cols[[change_form]]
  # ref_cols_view <- project$metadata$form_key_cols[[view_forms]]
  if (missing(optional_form)) {
    optional_form <- project[["data"]][[change_form]][, unique(c(ref_cols_change, field_names_to_view))]
  }
  if (is.null(field_names_to_view)) field_names_to_view <- colnames(optional_form)
  # if(!all(ref_cols%in%colnames(form)))stop("form must contain all ref_cols")
  if (length(records) > 0) {
    # message("fix these in REDCap --> ",paste0(out,collapse = " | "))
    rows_of_choices <- which(project$metadata$choices$field_name == field_name_to_change)
    has_choices <- length(rows_of_choices) > 0
    choices1 <- c("Do Nothing", "Edit", "Launch Redcap Link Only")
    if (has_choices) {
      choices2 <- c("Do Nothing", project$metadata$choices$name[rows_of_choices], "Launch Redcap Link Only")
    } else {
      choices2 <- c("Do Nothing", "Manual Entry", "Launch Redcap Link Only")
    }
    is_repeating_form <- change_form %in% project$metadata$forms$form_name[which(project$metadata$forms$repeating)]
    form <- NULL
    form_change <- project$data[[change_form]]
    row.names(form_change) <- NULL
    form_change <- form_change[, unique(c(ref_cols_change, field_name_to_change))]
    for (record in records) { # record <- records%>% sample(1)
      record_was_updated <- FALSE
      form_view <- optional_form[which(optional_form[[project$metadata$id_col]] == record), ]
      form_view_simp <- form_view[, unique(c(project$metadata$id_col, field_names_to_view))] %>% unique()
      row.names(form_view_simp) <- NULL
      form_view_simp %>%
        t() %>%
        print()
      if (nrow(form_change) == 0) {
        print("Nothing in form_change. If you choose edit it will add an instance...")
        blank_row <- data.frame(
          record
        )
        colnames(blank_row)[[1]] <- project$metadata$id_col
        if ("redcap_repeat_instance" %in% ref_cols_change) {
          blank_row$redcap_repeat_instance <- "1"
          blank_row$redcap_repeat_instrument <- change_form
        }
        blank_row[[field_name_to_change]] <- NA
      } else {
        print(form_change)
      }
      choice1 <- utils::menu(choices1, title = paste0("What would you like to do?"))
      if (choice1 == 3) {
        project %>% link_REDCap_record(record = record)
      }
      if (choice1 == 2) {
        if (nrow(form_change) == 0) form_change <- blank_row
        for (j in seq_len(nrow(form_change))) {
          message("Old answer (", field_name_to_change, "): ", form_change[j, field_name_to_change])
          choice2 <- utils::menu(choices2, title = paste0("What would you like to do?"))
          choice <- choices2[choice2]
          form_sub <- form_change[j, ]
          if (choice %in% c("Manual Entry", "Do Nothing", "Launch Redcap Link Only")) {
            if (choice == "Do Nothing") {
              message("Did not change anything")
            }
            if (choice == "Manual Entry") {
              form_sub[[field_name_to_change]] <- readline("What would you like it to be? ")
              if (upload_individually) {
                form_sub %>%
                  labelled_to_raw_form(project) %>%
                  upload_form_to_REDCap(project)
                message("Uploaded: ", form_sub %>% paste0(collapse = " | "))
                record_was_updated <- TRUE
              } else {
                form <- form %>% dplyr::bind_rows(form_sub)
              }
            }
            if (choice == "Launch Redcap Link Only") { # account for repeat? instance
              project %>% link_REDCap_record(record = record, page = change_form, instance = form_change[j, "redcap_repeat_instance"])
            }
          } else {
            form_sub[[field_name_to_change]] <- choice
            if (upload_individually) {
              form_sub %>%
                labelled_to_raw_form(project) %>%
                upload_form_to_REDCap(project)
              message("Uploaded: ", form_sub %>% paste0(collapse = " | "))
              record_was_updated <- TRUE
            } else {
              form <- form %>% dplyr::bind_rows(form_sub)
            }
          }
        }
        if (is_repeating_form) {
          choice3 <- 2
          the_max <- 0
          if (nrow(form_change) > 0) {
            the_max <- form_change$redcap_repeat_instance %>%
              as.integer() %>%
              max()
          }
          while (choice3 == 2) {
            choice3 <- utils::menu(c("No", "Yes"), title = paste0("Would you like to add an additional instance?"))
            if (choice3 == 2) {
              form_sub <- data.frame(
                record_id = record,
                redcap_repeat_instrument = change_form,
                redcap_repeat_instance = as.character(the_max + 1),
                stringsAsFactors = FALSE
              )
              colnames(form_sub)[1] <- project$metadata$id_col
              choice2 <- utils::menu(choices2, title = paste0("What would you like to do?"))
              choice <- choices2[choice2]
              if (choice %in% c("Manual Entry", "Do Nothing", "Launch Redcap Link Only")) {
                if (choice == "Do Nothing") {
                  message("Did not change anything")
                }
                if (choice == "Manual Entry") {
                  form_sub[[field_name_to_change]] <- readline("What would you like it to be? ")
                  if (upload_individually) {
                    form_sub %>%
                      labelled_to_raw_form(project) %>%
                      upload_form_to_REDCap(project)
                    message("Uploaded: ", form_sub %>% paste0(collapse = " | "))
                    record_was_updated <- TRUE
                  } else {
                    form <- form %>% dplyr::bind_rows(form_sub)
                  }
                  the_max <- the_max + 1
                }
                if (choice == "Launch Redcap Link Only") { # account for repeat? instance
                  project %>% link_REDCap_record(record = record, page = change_form, instance = form_change[j, "redcap_repeat_instance"])
                }
              } else {
                form_sub[[field_name_to_change]] <- choice
                if (upload_individually) {
                  form_sub %>%
                    labelled_to_raw_form(project) %>%
                    upload_form_to_REDCap(project)
                  message("Uploaded: ", form_sub %>% paste0(collapse = " | "))
                  record_was_updated <- TRUE
                } else {
                  form <- form %>% dplyr::bind_rows(form_sub)
                }
                the_max <- the_max + 1
              }
            }
          }
        }
      }
    }
    if (record_was_updated) project <- sync_project(project)
  }
  if (!upload_individually) {
    form %>%
      labelled_to_raw_form(project) %>%
      upload_form_to_REDCap(project)
  }
}
#' @title add_fields_to_data_list
#' @export
add_fields_to_data_list <- function(data_list,transformation){
  metadata <- data_list$metadata
  named_df_list <- data_list$data
  the_names <- transformation$fields$field_name
  has_fields <- !is.null(the_names)
  original_fields <- metadata$fields
  the_names_existing <- the_names[which(the_names %in% original_fields$field_name)]
  the_names_new <- the_names[which(!the_names %in% original_fields$field_name)]
  field_names <- c(the_names_existing, the_names_new) %>% unique()
  for (field_name in field_names) {
    field <- NA
    row_of_interest <- transformation$fields[which(transformation$fields$field_name == field_name), ]
    form_name <- row_of_interest$form_name
    field_func <- transformation$field_functions[[field_name]]
    environment(field_func) <- environment()
    if (is_something(field_func)) {
      if (form_name %in% names(named_df_list)) {
        field <- field_func(project = data_list,
                            field_name = field_name,
                            form_name = form_name)
      }
    }
    if (field_name %in% the_names_existing) {
      form_old <- named_df_list[[form_name]][[field_name]]
      if (!identical(field, form_old)) {
        ref_cols <- metadata$form_key_cols[[form_name]]
        new <- old <- named_df_list[[form_name]][, c(ref_cols, field_name)]
        new[[field_name]] <- field
        form <- REDCapSync:::find_form_diff2(
          new = new,
          old = old,
          ref_cols = ref_cols,
          view_old = FALSE,
          message_pass = paste0(form_name, " - ", field_name, ": ")
        )
        if (is_something(form)) {
          transformation$data_updates[[field_name]] <- form
        }
      }
    }
    if (form_name %in% names(named_df_list)) {
      named_df_list[[form_name]][[field_name]] <- field
    }
  }
  fields <- combine_project_fields(data_list,transformation)
  fields$original_form_name <- fields$form_name
  # fields$form_name <- forms_transformation_original$form_name_remap[match(fields$form_name, forms_transformation_original$form_name)]
  # fields <- fields[order(match(fields$form_name, forms_transformation$form_name)), ]
  # new function RosyUtils
  first <- 1:which(colnames(fields) == "form_name")
  move <- which(colnames(fields) == "original_form_name")
  last <- which(colnames(fields) != "original_form_name")[-first]
  fields <- fields[, c(first, move, last)]
  metadata$fields <- fields
  metadata$choices <- REDCapSync:::fields_to_choices(fields)
  metadata$form_key_cols <- REDCapSync:::get_key_col_list(data_list = data_list)
  cli_alert_wrap(paste0("Added new fields"), bullet_type = "v")
  data_list$metadata <- metadata
  data_list$data <- named_df_list
  data_list
}
#' @title Find the project_import and project differences
#' @description
#' This function compares the data in the `project` object (new data) with the
#' previous or reference data to identify differences. It returns a list of
#' differences for upload. The function ensures that the new data matches the
#' structure defined by the metadata and provides warnings when discrepancies
#' are found.
#' @param to_be_uploaded a data.frame or list of data.frames to be uploaded
#' @inheritParams save_project
#' @param view_old Logical. If TRUE, it will display a preview of the old data
#' (default is FALSE).
#' @param n_row_view Numeric. Defines how many rows of the old data to view
#' (default is 20).
#' @return A list of differences between the new and old data (`upload_list`).
#' @details
#' The function compares the data in `project$data_updates` (new data) with the
#' current data in the database (`project$data`). If the form names in the new
#' data do not match the `project$metadata$forms$form_name`, a warning is
#' issued. The function goes through each table in the new data and compares it
#' with the old data, recording the differences.
#'
#' The `compare` and `to` parameters allow users to specify specific data
#' choices to compare, though their exact usage will depend on how the function
#' is fully implemented.
#' @export
find_upload_diff <- function(to_be_uploaded,
                             project,
                             view_old = FALSE,
                             n_row_view = 20) {
  project <- assert_blank_project(project)
  # if (!all(names(new_list) %in% project$metadata$forms$form_name)) warning("All upload names should ideally match the project form names, `project$metadata$forms$form_name`", immediate. = TRUE)
  # already_used <- NULL
  was_df <- is.data.frame(to_be_uploaded)
  was_list <- is.list(to_be_uploaded)
  if (!was_df && !was_list) {
    stop("`to_be_uploaded` must be list of data.frames or a date.frame")
  }
  if (was_df) {
    to_be_uploaded <- list(upload_me = to_be_uploaded)
  }
  for (user_name in names(to_be_uploaded)) { # form_name <- names(new_list) %>% sample(1)
    new <- to_be_uploaded[[user_name]]
    ref_cols <- project$metadata$raw_structure_cols
    ref_cols <- ref_cols[which(ref_cols %in% colnames(new))]
    data_cols <- colnames(new)[which(!colnames(new) %in% ref_cols)]
    form_names <- field_names_to_form_names(project, data_cols)
    # if (any(form_names %in% already_used)) {
    #   stop("REDCapSync will not allow you to upload items from same form multiple times in one loop without refreshing.")
    # }
    if (length(form_names) > 1) {
      if (!all(form_names %in% project$metadata$forms$form_name[which(project$metadata$forms$repeating)])) {
        stop("Can't have variables in multiple forms in an upload data.frame unless it is a non-repeating form")
      }
      stop("Can only upload data from one form at a time.")
    }
    relevant_data_cols <- data_cols %>% vec1_in_vec2(
      form_names_to_field_names(
        form_names = form_names,
        project = project
      )
    )
    keep <- c(ref_cols, relevant_data_cols)
    drop <- data_cols %>% vec1_not_in_vec2(form_names_to_field_names(form_names = form_names, project = project))
    if (length(drop) > 0) {
      message("Dropping field_names that aren't part of REDCap metadata: ", toString(drop))
    }
    final <- find_form_diff2(
      new = new[, keep],
      old = project$data[[form_names]][, keep],
      ref_cols = ref_cols,
      message_pass = paste0(user_name, ": "),
      view_old = view_old,
      n_row_view = n_row_view
    )
    if (was_df) {
      to_be_uploaded <- final
    } else {
      to_be_uploaded[[user_name]] <- final
    }
  }
  if (is_something(to_be_uploaded)) {
    return(invisible(to_be_uploaded))
  }
  message("No upload updates!")
  invisible(NULL)
}
