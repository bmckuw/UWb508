#' Large sample CI for a binomial probability
#'
#' @param x number of binomial "successes" (proportion numerator)
#' @param n number of binomial "trials" (proportion denominator)
#' @param conf Confidence coefficient for CIs as proportion: .95 for 95\% CIs is default
#' @importFrom stats qnorm
#' @export largen_binom_ci
#' @return Estimate of binomial probability (sample proportion), large-sample se, and CI
#' @examples
#' largen_binom_ci(15, 44)
#' largen_binom_ci(0, 20)

largen_binom_ci <- function(x, n, conf = .95){
  if (x == 0) return("Too few binomial successes to use this command")
  if (x == 0) return("Too few binomial failures to use this command")
  if (n < 10) return("Too few observations to use this command")
  phat <- x/n
  se <- sqrt(phat*(1-phat)/n)
  zstar <- qnorm(1 - (1-conf)/2)
  return(list(phat = phat, se = se, ci = c(phat - se*zstar, phat+se*zstar)))
}

