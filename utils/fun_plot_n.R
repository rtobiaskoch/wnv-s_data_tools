#' Plot Counts by One or More Grouping Variables
#'
#' This function groups a data frame by one or more variables and plots the count
#' of observations per group using a bar chart.
#'
#' @param df A data frame or tibble.
#' @param ... One or more unquoted column names to group by (tidy evaluation).
#'
#' @return A `ggplot2` bar plot object showing the number of rows per group. If multiple
#' grouping variables are supplied, they will be combined into a single label on the x-axis.
#'
#' @examples
#' # Count by one grouping variable
#' plot_n(mtcars, cyl)
#'
#' # Count by multiple grouping variables
#' plot_n(mtcars, cyl, gear)
#'
#' @import dplyr group_by count mutate
#' @import ggplot2
#' @import forcats fct_reorder
#' @importFrom tidyr unite

plot_n <- function(df, ...) {
  df %>%
    group_by(...) %>%
    count() %>%
    tidyr::unite("group_label", ..., sep = " | ") %>%
    mutate(group_label = forcats::fct_reorder(group_label, n)) %>%
    ggplot(aes(group_label, n)) +
    geom_col() +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title = element_blank()
    )
}
