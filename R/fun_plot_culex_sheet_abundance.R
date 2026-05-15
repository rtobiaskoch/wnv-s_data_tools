#' Plot Abundance by Zone and Year
#'
#' Creates a line plot of mosquito abundance by zone and year. Zones NW, SW, NE, SE are grouped into "FC".
#' Filters to method "L" and optionally excludes specified zones (e.g., "BC").
#'
#' @param data A data frame containing at least the columns: `zone`, `year`, `method`, and `total`.
#' @param filter_zone Optional. A character vector of zones to exclude. Default is `NULL` (include all zones).
#'
#' @return A ggplot object showing abundance over time by zone.
#' @importFrom dplyr mutate if_else group_by filter summarize n
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_fill_brewer scale_color_brewer theme_classic
#' @export
plot_abundance_by_zone <- function(data, filter_zone = NULL) {
  data <- data %>%
    dplyr::mutate(zone = dplyr::if_else(zone %in% c("NW", "SW", "NE", "SE"), "FC", zone)) %>%
    dplyr::filter(method == "L")
  
  if (!is.null(filter_zone)) {
    data <- dplyr::filter(data, !zone %in% filter_zone)
  }
  
  data %>%
    dplyr::group_by(zone, year) %>%
    dplyr::summarize(
      n = dplyr::n(),
      total = sum(total),
      abund = total / n,
      .groups = "drop"
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = abund, fill = zone, color = zone)) +
    ggplot2::geom_line(position = "dodge") +
    ggplot2::geom_point() +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_classic()
}
