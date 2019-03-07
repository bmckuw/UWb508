#' Coefficient summary table for linear models with robust standard errors and robust CIs
#'
#' @param model A model object created by the fit of linear model using lm()
#' @param type Character string giving type of robust standard error: HC1 is default
#' @param level Confidence coefficient for CIs as proportion: .95 for 95\% CIs is default
#' @param digits Number of digits to round coefficients, SEs and CIs to in output.  Default is 2.
#' @import sandwich
#' @import lmtest
#' @export robust
#' @return A table with coefficient estimates, and CIs and P-values based on robust standard errors
#' @examples
#' model <- lm(mpg ~ gear, data = mtcars)
#' robust(model)
robust <- function(model, type = c("HC1"),  level = 0.95, digits = 2){
  requireNamespace("sandwich", quietly = TRUE)
  df <- model$df.residual
  result <- lmtest::coeftest(model, vcov. = vcovHC, type = type, df = df)
  ci <- lmtest::coefci(model, vcov. = vcovHC, type = type, df = df,
               level = level)
  Pval <- signif(result[,4])
  result <- round(cbind(result[, 1:3], ci), digits)
  result <- data.frame(result, Pval)
  names(result)[c(2:5)] <- c("Robust SE", "T value", "2.5%", "97.5%")
  return(result)
}
