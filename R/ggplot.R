color_func <- scales::col_numeric(palette = c("red", "yellow", "green"),
                                  domain = c(0, 1))
#' @title create_gauge_plot
#' @export
create_gauge_plot <- function(percentage_done,
                              title = NULL,
                              caption = NULL,
                              text_size = 16) {
  df <- data.frame(
    part = c("Complete", "Incomplete"),
    percentage = c(percentage_done, 1 - percentage_done)
  )
  df$start <- dplyr::lag(df$percentage, default = 0) * pi
  df$end = df$start + (df$percentage * pi)
  gauge_plot <- ggplot(df) +
    ggforce::geom_arc_bar(aes(
      x0 = 1,
      y0 = 1,
      fill = part,
      start = start - pi / 2,
      end = end - pi / 2,
      r0 = 0.75,
      r = 1
    )) +
    coord_fixed() +
    theme_void() +
    scale_fill_manual(values = c(
      "Complete" = color_func(percentage_done),
      "Incomplete" = "grey80"
    )) +
    labs(title = title, caption = caption) +
    theme(
      legend.position = "none",
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = text_size
      ),
      # plot.subtitle = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.caption = element_text(
        hjust = 0.5,
        face = "bold",
        size = text_size
      )
    )
  gauge_plot
}
