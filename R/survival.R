font_maker <- function(size = 12L,
                       style = "bold",
                       color = "black") {
  styles <- c("plain", "bold", "italic", "bold.italic")
  if (!style %in% styles)
    stop("style must be of type: ", styles |> paste(collapse = ", "))
  c(as.numeric(size), as.character(style), as.character(color))
}
#' @title make_survival
#' @param DF data.frame
#' @param start_col character string of DF colname for start dates
#' @param end_col character string of DF colname for end dates
#' @param units one of "weeks", "months", "years"
#' @param time_col character string of DF colname for time (will then ignore
#' start and end)
#' @param status_col character string of DF colname for status (should be factor
#'  or 0/1 pr T/F)
#' @param strat_col character string of DF colname for stratification
#' @param allowed_levels character string of allowed levels
#' @param pval logical for including pval
#' @param pval.coord coordinates of pval
#' @param risk.table logical
#' @param title character string for title
#' @param palette colors
#' @param conf.int logical for confidence interval shading
#' @param xlim limits for x axis
#' @param legend.position ong of top, bottom, left, right
#' @param tables.height table high as character
#' @param show_stats logcial for stat printing
#' @return survival curve plot
#' @export
make_survival <- function(DF,
                          start_col,
                          end_col,
                          time_col,
                          status_col,
                          strat_col,
                          units = "months",
                          allowed_levels = NULL,
                          pval = TRUE,
                          pval.coord = c(0L, 0.25),
                          risk.table = TRUE,
                          title = NULL,
                          palette = NULL,
                          conf.int = TRUE,
                          xlim = NULL,
                          legend.position = "top",
                          tables.height = 0.25,
                          show_stats = FALSE) {
  if (!is.data.frame(DF))
    stop("DF has to be a data.frame")
  DF_ori <- DF
  if (missing(time_col)) {
    DF[["time_col"]] <- age(
      dob = DF[[start_col]],
      age.day = DF[[end_col]],
      floor = FALSE,
      units = units
    )#imputation here?
    time_col <- "time_col"
  } else {
    DF[["time_col"]] <- DF[[time_col]]
  }
  DF[["status_col"]] <- DF[[status_col]]
  vars <-  c("time_col", "status_col")
  if (!missing(strat_col)) {
    vars <-  append(vars, strat_col)
    if (!is.null(allowed_levels)) {
      DF <- DF[which(DF[[strat_col]] %in% allowed_levels), ]
    }
  }
  DF <- DF[, vars] |> stats::na.omit() |> clone_attr(DF_ori)
  legend.title <- NULL
  legend.labs <- NULL
  if (!missing(strat_col)) {
    if (!is.null(strat_col)) {
      legend.title <- strat_col
      x <- attr(DF[[strat_col]], "label")
      if (!is.null(x))
        legend.title <- x
      DF[["strat_col"]] <- DF[[strat_col]]
      fit <- survival::survfit(survival::Surv(time_col, status_col) ~ strat_col, data = DF)
      if (is.factor(DF[[strat_col]])) {
        legend.labs <- levels(DF[[strat_col]])
        legend.labs <- vec1_in_vec2(legend.labs, unique(DF[[strat_col]]))
      } else {
        legend.labs <- unique(DF[[strat_col]])
      }
    }
  } else {
    fit <- survival::survfit(survival::Surv(time_col, status_col) ~ 1L, data = DF)
  }
  if (is.null(fit)) {
    return(NULL)
  }
  plot <-  survminer::ggsurvplot(
    fit = fit,
    data = DF,
    surv.median.line = "hv",
    # Add medians survival
    # Change legends: title & labels,
    # size = 1,                   # change line size
    title = title,
    xlim = xlim,
    break.x.by = 1L,
    # legend = "bottom",
    legend = legend.position,
    legend.title = legend.title,
    # legend.labs = data[[group]] |> attr("levels"),
    legend.labs = legend.labs,
    # Add p-value and tervals
    combine = TRUE,
    pval = pval,
    pval.coord = pval.coord,
    conf.int = conf.int,
    xlab = stringr::str_to_title(units),
    ylab = "Survival Probability",
    # Add risk table
    risk.table = risk.table,
    tables.height = tables.height,
    # risk.table.height = 0.25,   # Useful to change when you have multiple groups
    tables.theme = survminer::theme_cleantable(),
    surv.scale = "percent",
    # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
    # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
    palette = palette,
    ggtheme = ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")),
    font.x = font_maker(14L),
    font.y = font_maker(14L),
    font.main = font_maker(14L),
    font.submain = font_maker(14L),
    font.tickslab = font_maker(14L),
    font.caption = font_maker(14L),
    font.title = font_maker(14L),
    font.subtitle = font_maker(14L),
    font.legend = font_maker(11L),
    table.theme = ggplot2::theme_classic()
  )
  if (show_stats) {
    print(fit)
  }
  plot
}
