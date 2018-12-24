#' Visualize extracted overall feature importance
#'
#' Generates ggplot of top ranked features
#'
#' To be used after data is generated from \code{HSPSModelR::var_imp_overall()}
#'
#' @param data tibble of results from \code{HSPSModelR::var_imp_overall()}
#' @param top_num \code{integer} number of top features to display
#' @importFrom dplyr top_n mutate
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 ggplot aes geom_point coord_flip theme_minimal labs theme
#'  %+% element_blank
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @export
#'
#' @return \code{tibble} of ranked features including columns:
#' \itemize{
#'  \item model
#'  \item feature
#'  \item Overall - Scaled score produced by \code{caret::varImp()}
#'  \item rank - Ordered rank within each model
#' }
#'
#' @author "Dallin Webb <dallinwebb@@byui.edu>"
#' @seealso \link[caret]{varImp}
#'
#' @examples
#' \dontrun{
#'
#' vars <- var_imp_overall(models_list)
#'
#' gg_var_imp(vars, top_num = 20)
#'
#' }
gg_var_imp <- function(data, top_num = 20) {

  if (!sum(c("features","rank_scaled") %in% names(data)) == 2) {
    stop("Data must contain 'features' and 'rank_scaled' columns ",
         "produced by the 'HSPSModelR::var_imp_overall() function")
  }


  data %>%
    top_n(top_num, rank_scaled) %>%
    mutate(features = fct_reorder(features, rank_scaled, max)) %>%
    ggplot(aes(features, rank_scaled)) +
    geom_point(size = 4) +
    coord_flip() +
    theme_minimal() +
    labs(x = NULL,
         y = "Scaled Feature Rank",
         title = "Feature importance based within and between models") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
}
