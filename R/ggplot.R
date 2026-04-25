#' @noRd
create_gauge_plot <- function(percentage_done,
                              title = NULL,
                              caption = NULL,
                              text_size = 16L) {
  color_func <- scales::col_numeric(palette = c("red", "yellow", "green"),
                                    domain = c(0L, 1L))
  df <- data.frame(
    part = c("Complete", "Incomplete"),
    percentage = c(percentage_done, 1L - percentage_done),
    stringsAsFactors = FALSE
  )
  df$start <- dplyr::lag(df$percentage, default = 0L) * pi
  df$end <- df$start + (df$percentage * pi)
  gauge_plot <- ggplot2::ggplot(df) +
    ggforce::geom_arc_bar(
      ggplot2::aes(
      x0 = 1L,
      y0 = 1L,
      fill = df$part,
      start = df$start - pi / 2L,
      end = df$end - pi / 2L,
      r0 = 0.75,
      r = 1L
    )) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::scale_fill_manual(values = c(
      Complete = color_func(percentage_done),
      Incomplete = "grey80"
    )) +
    ggplot2::labs(title = title, caption = caption) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = text_size
      ),
      # plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = text_size
      )
    )
  gauge_plot
}
