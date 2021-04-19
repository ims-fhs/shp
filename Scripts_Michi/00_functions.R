#' extract model estimates using modelsummary::get_estimates and add them to an
#' existing coeff_table of other models (or create a new table, if  coeff_table == NULL)
#'
#' @param model
#' @param coeff_table
#' @param name
#'
#' @return a tibble
#' @export
#'
#' @examples
add_model_to_coeff_table <- function(model, coeff_table = NULL, name=deparse(substitute(model))) {
  #https://stackoverflow.com/questions/24309910/how-to-get-name-of-variable-in-r-substitute

  # s <- summary(model)              # /// # FE -> s$coefficients / RE -> s$varcor
  # s <-  modelsummary::get_gof(m1)  # /// Goodness of fit

  if (is.null(coeff_table)) {
    # Initialize the coefficient-table with first column "model_name" and all 10
    # other columns (lmer-models have 10 columns, lm-models only have 6 columns
    # -> we always want the 10 columns)
    coeff_table <- structure(list(model_name = character(0), effect = character(0), group = character(0),
                                  term = character(0), estimate = numeric(0), std.error = numeric(0),
                                  statistic = numeric(0), df = numeric(0), p.value = numeric(0),
                                  conf.low = numeric(0), conf.high = numeric(0)), row.names = integer(0),
                                  class = c("tbl_df", "tbl", "data.frame"))
  }
  # extract parameter-estimates from model and add first column "model_name"
  df <- modelsummary::get_estimates(model)
  df <- base::cbind(model_name = name, df)
  # df <- df[!grepl("cor__", df$term),] # neglect parameters concerning correlation

  coeff_table <- dplyr::bind_rows(coeff_table, df)
  return(coeff_table)
}
