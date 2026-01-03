age <- function(dob,
                age.day = lubridate::today(),
                units = "years",
                floor = TRUE) {
  calc_age <- lubridate::interval(dob, age.day) / lubridate::duration(num = 1L, units = units)
  if (floor){
    return(as.integer(floor(calc_age)))
  }
  calc_age
}
cli_alert_wrap <- function(text = "",
                           url = NULL,
                           bullet_type = "i",
                           collapse = TRUE,
                           file = NULL,
                           silent = FALSE) {
  if (silent) {
    return(invisible())
  }
  url_if <- ""
  file_if <- ""
  if (length(url) > 0L) {
    # url |> lapply(function(x){assert_web_link(x)})
    # doesnt work for /subheaders/
    # url_if <- " {.url {url}}"
    url_names <- names(url)
    if (is.list(url)) {
      url_names <- unlist(url)
      if (is_named_list(url)) {
        url_names <- names(url)
      }
      url <- unlist(url)
    }
    if (is.null(url_names))
      url_names <- url
    if (collapse)
      url_if <- paste(url_if, collapse = " and ")
    url_if <- paste0(" {cli::col_blue(cli::style_hyperlink('",
                     url_names,
                     "', '",
                     url,
                     "'))}")
  }
  if (length(file) > 0L) {
    file_names <- names(file)
    if (is.list(file)) {
      file_names <- unlist(file)
      if (is_named_list(file))
        file_names <- names(file)
      file <- unlist(file)
    }
    if (is.null(file_names))
      file_names <- file
    if (collapse)
      file_if <- paste(file_if, collapse = " and ")
    file_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      sanitize_path(file_names),
      "', '",
      sanitize_path(paste0("file://", file)),
      "'))}"
    )
  }
  for (i in seq_along(url_if))
    text[i] <- paste0(text[i], url_if[i])
  for (i in seq_along(file_if))
    text[i] <- paste0(text[i], file_if[i])
  names(text)[seq_along(text)] <- bullet_type
  cli::cli_bullets(text)
}
now_time <- function() {
  as.POSIXct(Sys.time(), tz = Sys.timezone())
}
process_df_list <- function(list,
                            drop_empty = TRUE,
                            silent = FALSE) {
  if (is_something(list)) {
    if (!is_df_list(list))
      stop("list must be ...... a list :)")
    if (drop_empty) {
      is_a_df_with_rows <- list |>
        lapply(function(x) {
          is_df <- is.data.frame(x)
          out <- FALSE
          if (is_df) {
            out <- nrow(x) > 0L
          }
          out
        }) |>
        unlist()
      keeps <- which(is_a_df_with_rows)
      drops <- which(!is_a_df_with_rows)
      if (length(drops) > 0L) {
        if (!silent) {
          cli_alert_wrap("Dropping non-data.frames and empties... ",
                         toString(names(drops)))
        }
      }
      list <- list[keeps]
    }
    if (length(list) > 0L) {
      if (!is_named_df_list(list)) {
        names(list) <- paste0(seq_along(list))
      }
    }
  }
  list
}
is_something <- function(thing, row = 0L) {
  out <- FALSE
  if (is.function(thing)) {
    return(TRUE)
  }
  if (!is.null(thing)) {
    if (is.data.frame(thing)) {
      if (nrow(thing) > row) {
        out <- TRUE
      }
    } else {
      if (length(thing) > 0L) {
        if (is.list(thing)) {
          out <- TRUE
        } else {
          if (length(thing) == 1L) {
            if (!is.na(thing)) {
              if (is.character(thing)) {
                if (thing != "") {
                  out <- TRUE
                }
              } else {
                out <- TRUE
              }
            }
          } else {
            out <- TRUE
          }
        }
      }
    }
  }
  out
}
sanitize_path <- function(path) {
  sanitized <- gsub("\\\\", "/", path)
  sanitized <- normalizePath(sanitized, winslash = "/", mustWork = FALSE)
  sanitized
}
all_character_cols <- function(DF) {
  as.data.frame(lapply(DF, as.character))
}
all_character_cols_list <- function(list) {
  lapply(list, all_character_cols)
}
vec1_in_vec2 <- function(vec1, vec2) {
  vec1[which(vec1 %in% vec2)]
}
vec1_not_in_vec2 <- function(vec1, vec2) {
  vec1[which(!vec1 %in% vec2)]
}
ul <- function(x) {
  length(unique(x))
}
wl <- function(x) {
  length(which(x))
}
drop_nas <- function(x) {
  x[!unlist(lapply(x, is.na))]
}
is_named_df_list <- function(x, strict = FALSE) {
  is_named_list(x) && is_df_list(x, strict = strict)
}
is_named_list <- function(x,
                          silent = TRUE,
                          recursive = FALSE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (is.null(names(x))) {
    return(FALSE)
  }
  named_all <- TRUE
  if (recursive) {
    for (n in names(x)) {
      element <- x[[n]]
      if (is.list(element)) {
        named_all <- named_all && is_named_list(element)
        if (!silent && !named_all)
          message("'", n, "' is not named")
      }
    }
  }
  named_all # Return the result
}
dw <- function(x) {
  which(duplicated(x))
}
is_consecutive_srt_1 <- function(vec) {
  if (vec[1L] != 1L) {
    return(FALSE)
  }
  if (length(vec) > 1L) {
    for (i in 2L:length(vec)) {
      if (vec[i] != vec[i - 1L] + 1L) {
        return(FALSE)
      }
    }
  }
  TRUE
}
remove_html_tags <- function(text_vector) {
  gsub(pattern = "<[^>]+>", replacement = "", text_vector)
}
drop_if <- function(x, drops) {
  x[which(!x %in% drops)]
}
sample1 <- function(x) {
  sample(x, 1L)
}
list.files.real <- function(path,
                            full.names = TRUE,
                            recursive = FALSE) {
  grep(
    "~$",
    sanitize_path(
      list.files(path, full.names = full.names, recursive = recursive)
    ),
    fixed = TRUE,
    value = TRUE,
    invert = TRUE
  )
}
wrap_text <- function(text,
                      max_length = 40L,
                      spacer = "\n") {
  words <- unlist(strsplit(text, " "))
  current_line <- ""
  result <- ""
  for (word in words) {
    if (nchar(current_line) + nchar(word) + 1L > max_length) {
      result <- paste0(result, current_line, spacer)
      current_line <- word
    } else {
      if (nchar(current_line) == 0L) {
        current_line <- word
      } else {
        current_line <- paste0(current_line, " ", word)
      }
    }
  }
  result <- paste0(result, current_line)
  result
}
clean_env_names <- function(env_names,
                            silent = FALSE,
                            lowercase = TRUE) {
  cleaned_names <- character(length(env_names))
  for (i in seq_along(env_names)) {
    name <- env_names[i]
    is_valid <- is_env_name(name, silent = TRUE)
    if (is_valid)
      cleaned_names[i] <- name
    if (!is_valid) {
      if (!silent)
        message("Invalid environment name: '", name)
      cleaned_name <- gsub("__", "_", gsub(" ", "_", gsub("-", "", name)))
      if (lowercase)
        cleaned_name <- tolower(cleaned_name)
      if (cleaned_name %in% cleaned_names) {
        if (!silent)
          message("Non-unique environment name: '",
                  name,
                  "', added numbers...")
        cleaned_name <- cleaned_name |> paste0("_", max(wl(cleaned_name %in% cleaned_names)) + 1L)
      }
      cleaned_names[i] <- cleaned_name
    }
  }
  cleaned_names
}
is_df_list <- function(x, strict = FALSE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (length(x) == 0L) {
    return(FALSE)
  }
  if (is_nested_list(x)) {
    return(FALSE)
  }
  out <- unlist(lapply(x, is.data.frame))
  if (strict) {
    return(all(out))
  }
  any(out)
}
is_env_name <- function(env_name, silent = FALSE) {
  result <- tryCatch({
    if (is.null(env_name))
      stop("env_name is NULL")
    if (nchar(env_name) == 0L) {
      stop("Short name cannot be empty.")
    }
    if (grepl("^\\d", env_name)) {
      stop("Short name cannot start with a number.")
    }
    if (grepl("[^A-Za-z0-9_]", env_name)) {
      stop("Short name can only contain letters, numbers, and underscores.")
    }
    TRUE # Return TRUE if all checks pass
  }, error = function(e) {
    if (!silent){
      message(e$message)
    }
    FALSE # Return FALSE if any error occurs
  })
  result
}
is_nested_list <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (is.data.frame(x)) {
    return(FALSE)
  }
  outcome <- length(x) == 0L
  for (i in seq_along(x)) {
    outcome <- outcome || is_nested_list(x[[i]])
    # print(outcome)
  }
  outcome
}
clean_num <- function(num) {
  formatC(num, format = "d", big.mark = ",")
}
is_date <- function(date) {
  OUT <- grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d{4}-\\d{2}$|^\\d{4}$", date)
  if (OUT) {
    OUT2 <- date |>
      strsplit(split = "-") |>
      unlist()
    year <- OUT2[[1L]]
    check_date <- year
    if (length(OUT2) == 1L) {
      check_date <- check_date |> paste0("-01")
      OUT2[[2L]] <- "01"
    }
    if (length(OUT2) == 2L) {
      check_date <- check_date |> paste0("-01")
      OUT2[[3L]] <- "01"
    }
    year <- year |> as.integer()
    month <- OUT2[[2L]] |> as.integer()
    day <- OUT2[[3L]] |> as.integer()
    OUT <- month >= 1L &&
      month <= 12L &&
      day >= 1L &&
      day <= 31L && year >= 1900L && year <= lubridate::year(Sys.Date())
  }
  OUT
}
is_date_full <- function(date) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
}
date_imputation <- function(dates_in, date_imputation) {
  # followup add min max
  z <- lapply(dates_in, is_date) |> as.logical()
  x <- which(z & !is_date_full(dates_in))
  y <- which(!z)
  date_out <- dates_in
  if (length(y) > 0L) {
    date_out[y] <- NA
  }
  if (length(x) > 0L) {
    if (missing(date_imputation))
      date_imputation <- NULL
    if (is.null(date_imputation)) {
      date_out[x] <- NA
    }
    if (!is.null(date_imputation)) {
      date_out[x] <- dates_in[x] |>
        lapply(function(date) {
          admiral::impute_dtc_dt(
            date,
            highest_imputation = "M",
            # "n" for normal date
            date_imputation = date_imputation
            # min_dates = min_dates |> lubridate::ymd() |> as.list(),
            # max_dates = max_dates |> lubridate::ymd() |> as.list()c
          )
        }) |>
        unlist()
    }
  }
  date_out
}
find_df_diff2 <- function(new,
                          old,
                          ref_cols = NULL,
                          message_pass = "",
                          view_old = TRUE,
                          n_row_view = 20) {
  new <- all_character_cols(new)
  old <- all_character_cols(old)
  if (!all(colnames(new) %in% colnames(old))) {
    stop("All new DF columns must be included in old DF")
  }
  if (!all(ref_cols %in% colnames(new)) | !all(ref_cols %in% colnames(old))) {
    stop("ref_cols must be included in both dfs")
  }
  if (length(ref_cols) > 1) {
    new_keys <- apply(new[, ref_cols], 1, paste, collapse = "_")
    old_keys <- apply(old[, ref_cols], 1, paste, collapse = "_")
  } else {
    new_keys <- new[, ref_cols]
    old_keys <- old[, ref_cols]
  }
  if (anyDuplicated(old_keys) > 0) {
    stop("Keys must lead to unique rows! (old DF)")
  }
  if (anyDuplicated(new_keys) > 0) {
    stop("Keys must lead to unique rows! (new DF)")
  }
  appended_old_col_suffix <- "__old"
  if (any(endsWith(unique(colnames(old), colnames(new)), appended_old_col_suffix))) {
    stop("colnames cant end with '", appended_old_col_suffix, "'")
  }
  merged_df <- merge(
    new,
    old,
    by = ref_cols,
    suffixes = c("", appended_old_col_suffix),
    all.x = TRUE
  )
  placeholder <- "NA_placeholder"
  rows_to_keep <- NULL
  cols_to_view <- cols_to_keep <- which(colnames(merged_df) %in% ref_cols)
  COLS <- colnames(new)[which(!colnames(new) %in% ref_cols)]
  for (COL in COLS) {
    vector1 <- merged_df[[COL]]
    compare_COL <- paste0(COL, appended_old_col_suffix)
    vector2 <- merged_df[[compare_COL]]
    vector1_no_na <- ifelse(is.na(vector1), placeholder, vector1)
    vector2_no_na <- ifelse(is.na(vector2), placeholder, vector2)
    # Compare vectors element-wise
    are_not_equal <- which(vector1_no_na != vector2_no_na)
    if (length(are_not_equal) > 0) {
      rows_to_keep <- rows_to_keep |> append(are_not_equal)
      additional_cols <- which(colnames(merged_df) == COL)
      cols_to_keep <- cols_to_keep |> append(additional_cols)
      if (view_old) {
        cols_to_view <- cols_to_view |>
          append(additional_cols) |>
          append(which(colnames(merged_df) == compare_COL))
      }
    }
  }
  if (length(rows_to_keep) > 0) {
    rows_to_keep <- unique(rows_to_keep)
    cols_to_keep <- unique(cols_to_keep)
    if (view_old) {
      rows_to_keep2 <- rows_to_keep
      done <- FALSE
      while (!done) {
        length_of_rows_to_keep <- length(rows_to_keep2)
        if (length_of_rows_to_keep == 0) {
          done <- TRUE
        } else {
          indices <- 1:ifelse(length_of_rows_to_keep < n_row_view, length_of_rows_to_keep, n_row_view)
          rows_to_keep3 <- rows_to_keep2[indices]
          print.data.frame(merged_df[rows_to_keep3, unique(cols_to_view)])
          choice <- utils::menu(choices = c("Check more rows", "Proceed with no more checking", "Stop the function"), title = "What would you like to do?")
          if (choice == 3) {
            stop("Stopped as requested!")
          }
          if (choice == 2) done <- TRUE
          if (choice == 1) rows_to_keep2 <- rows_to_keep2[-indices]
        }
      }
    }
    message(message_pass, length(rows_to_keep), " rows have updates")
    return(merged_df[rows_to_keep, cols_to_keep])
  } else {
    message(message_pass, "No changes!")
    return(NULL)
  }
}
