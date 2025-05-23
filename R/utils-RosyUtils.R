age <- function(dob, age.day = lubridate::today(), units = "years", floor = TRUE) {
  calc.age = lubridate::interval(dob, age.day) / lubridate::duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
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
  if (length(url) > 0) {
    # url %>% lapply(function(x){assert_web_link(x)})
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
    if (is.null(url_names)) url_names <- url
    if (collapse) url_if <- paste0(url_if, collapse = " and ")
    url_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      url_names,
      "', '",
      url,
      "'))}"
    )
  }
  if (length(file) > 0) {
    file_names <- names(file)
    if (is.list(file)) {
      file_names <- unlist(file)
      if (is_named_list(file)) file_names <- names(file)
      file <- unlist(file)
    }
    if (is.null(file_names)) file_names <- file
    if (collapse) file_if <- paste0(file_if, collapse = " and ")
    file_if <- paste0(
      " {cli::col_blue(cli::style_hyperlink('",
      sanitize_path(file_names),
      "', '",
      sanitize_path(paste0("file://", file)),
      "'))}"
    )
  }
  for (i in seq_along(url_if)) text[i] <- paste0(text[i], url_if[i])
  for (i in seq_along(file_if)) text[i] <- paste0(text[i], file_if[i])
  names(text)[seq_along(text)] <- bullet_type
  cli::cli_bullets(text)
}
now_time <- function() {
  return(as.POSIXct(Sys.time(), tz = Sys.timezone()))
}
process_df_list <- function(list, drop_empty = TRUE, silent = FALSE) {
  if (is_something(list)) {
    if (!is_df_list(list)) stop("list must be ...... a list :)")
    if (drop_empty) {
      is_a_df_with_rows <- list %>%
        lapply(function(x) {
          is_df <- is.data.frame(x)
          out <- FALSE
          if (is_df) {
            out <- nrow(x) > 0
          }
          out
        }) %>%
        unlist()
      keeps <- which(is_a_df_with_rows)
      drops <- which(!is_a_df_with_rows)
      if (length(drops) > 0) {
        if (!silent) {
          cli_alert_wrap(
            "Dropping non-data.frames and empties... ",
            toString(names(drops))
          )
        }
      }
      list <- list[keeps]
    }
    if (length(list) > 0) {
      if (!is_named_df_list(list)) {
        names(list) <- paste0(seq_along(list))
      }
    }
  }
  list
}
is_something <- function(thing, row = 0) {
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
      if (length(thing) > 0) {
        if (is.list(thing)) {
          out <- TRUE
        } else {
          if (length(thing) == 1) {
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
  return(out)
}
sanitize_path <- function(path) {
  sanitized <- gsub("\\\\", "/", path)
  sanitized <- normalizePath(sanitized, winslash = "/", mustWork = FALSE)
  return(sanitized)
}
all_character_cols <- function(DF) {
  as.data.frame(lapply(DF, as.character))
}
all_character_cols_list <- function(list) {
  lapply(list, all_character_cols)
}
as_comma_string <- function(vec) {
  paste0(vec, collapse = ", ")
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
is_named_list <- function(x, silent = TRUE, recursive = FALSE) {
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
        if (!silent && !named_all) message("'", n, "' is not named")
      }
    }
  }
  return(named_all) # Return the result
}
dw <- function(x) {
  which(duplicated(x))
}
is_consecutive_srt_1 <- function(vec) {
  if (vec[1] != 1) {
    return(FALSE)
  }
  if (length(vec) > 1) {
    for (i in 2:length(vec)) {
      if (vec[i] != vec[i - 1] + 1) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}
remove_html_tags <- function(text_vector) {
  # Regular expression to match HTML tags
  html_pattern <- "<[^>]+>"
  # Use gsub to remove the HTML tags from each element in the vector
  cleaned_vector <- gsub(html_pattern, "", text_vector)
  return(cleaned_vector)
}
drop_if <- function(x, drops) {
  x[which(!x %in% drops)]
}
sample1 <- function(x) {
  sample(x, 1)
}
list.files.real <- function(path, full.names = TRUE, recursive = FALSE) {
  grep("~$", sanitize_path(list.files(path, full.names = full.names, recursive = recursive)), fixed = TRUE, value = TRUE, invert = TRUE)
}
wrap_text <- function(text, max_length = 40, spacer = "\n") {
  words <- unlist(strsplit(text, " "))
  current_line <- ""
  result <- ""
  for (word in words) {
    if (nchar(current_line) + nchar(word) + 1 > max_length) {
      result <- paste0(result, current_line, spacer)
      current_line <- word
    } else {
      if (nchar(current_line) == 0) {
        current_line <- word
      } else {
        current_line <- paste0(current_line, " ", word)
      }
    }
  }
  result <- paste0(result, current_line)
  return(result)
}
clean_env_names <- function(env_names, silent = FALSE, lowercase = TRUE) {
  cleaned_names <- character(length(env_names))
  for (i in seq_along(env_names)) {
    name <- env_names[i]
    is_valid <- is_env_name(name, silent = TRUE)
    if (is_valid) cleaned_names[i] <- name
    if (!is_valid) {
      if (!silent) message("Invalid environment name: '", name)
      cleaned_name <- gsub("__", "_", gsub(" ", "_", gsub("-", "", name)))
      if (lowercase) cleaned_name <- tolower(cleaned_name)
      if (cleaned_name %in% cleaned_names) {
        if (!silent) message("Non-unique environment name: '", name, "', added numbers...")
        cleaned_name <- cleaned_name %>% paste0("_", max(wl(cleaned_name %in% cleaned_names)) + 1)
      }
      cleaned_names[i] <- cleaned_name
    }
  }
  return(cleaned_names)
}
is_df_list <- function(x, strict = FALSE) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (length(x) == 0) {
    return(FALSE)
  }
  if (is_nested_list(x)) {
    return(FALSE)
  }
  out <- unlist(lapply(x, is.data.frame))
  if (strict) {
    return(all(out))
  }
  return(any(out))
}
is_env_name <- function(env_name, silent = FALSE) {
  result <- tryCatch(
    {
      if (is.null(env_name)) stop("env_name is NULL")
      if (nchar(env_name) == 0) {
        stop("Short name cannot be empty.")
      }
      if (grepl("^\\d", env_name)) {
        stop("Short name cannot start with a number.")
      }
      if (grepl("[^A-Za-z0-9_]", env_name)) {
        stop("Short name can only contain letters, numbers, and underscores.")
      }
      return(TRUE) # Return TRUE if all checks pass
    },
    error = function(e) {
      if (!silent) message(e$message)
      return(FALSE) # Return FALSE if any error occurs
    }
  )
  return(result)
}
is_nested_list <- function(x) {
  if (!is.list(x)) {
    return(FALSE)
  }
  if (is.data.frame(x)) {
    return(FALSE)
  }
  outcome <- length(x) == 0
  for (i in seq_along(x)) {
    outcome <- outcome || is_nested_list(x[[i]])
    # print(outcome)
  }
  return(outcome)
}
clean_num <- function(num) {
  formatC(num, format = "d", big.mark = ",")
}
is_date <- function(date) {
  OUT <- grepl("^\\d{4}-\\d{2}-\\d{2}$|^\\d{4}-\\d{2}$|^\\d{4}$", date)
  if (OUT) {
    OUT2 <- date %>%
      strsplit(split = "-") %>%
      unlist()
    year <- OUT2[[1]]
    check_date <- year
    if (length(OUT2) == 1) {
      check_date <- check_date %>% paste0("-01")
      OUT2[[2]] <- "01"
    }
    if (length(OUT2) == 2) {
      check_date <- check_date %>% paste0("-01")
      OUT2[[3]] <- "01"
    }
    year <- year %>% as.integer()
    month <- OUT2[[2]] %>% as.integer()
    day <- OUT2[[3]] %>% as.integer()
    OUT <- month >= 1 && month <= 12 && day >= 1 && day <= 31 && year >= 1900 && year <= lubridate::year(Sys.Date())
  }
  OUT
}
is_date_full <- function(date) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", date)
}
date_imputation <- function(dates_in, date_imputation) {
  # followup add min max
  z <- lapply(dates_in, is_date) %>% as.logical()
  x <- which(z & !is_date_full(dates_in))
  y <- which(!z)
  date_out <- dates_in
  if (length(y) > 0) {
    date_out[y] <- NA
  }
  if (length(x) > 0) {
    if (missing(date_imputation)) date_imputation <- NULL
    if (is.null(date_imputation)) {
      date_out[x] <- NA
    }
    if (!is.null(date_imputation)) {
      date_out[x] <- dates_in[x] %>%
        lapply(function(date) {
          admiral::impute_dtc_dt(
            date,
            highest_imputation = "M", # "n" for normal date
            date_imputation = date_imputation
            # min_dates = min_dates %>% lubridate::ymd() %>% as.list(),
            # max_dates = max_dates %>% lubridate::ymd() %>% as.list()
          )
        }) %>%
        unlist()
    }
  }
  date_out
}
