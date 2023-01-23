#' Estimate Intraclass Correlation Coefficient
#'
#' Uses the method described in Sommet, N. and Morselli, D. (2017). Keep Calm and Learn Multilevel Logistic Modeling: A Simplified Three-Step Procedure Using Stata, R, Mplus, and SPSS. International Review of Social Psychology, 30(1), 203–218, DOI: https://doi.org/10.5334/irsp.90
#' @param outcome Character. The name of the outcome variable. No default.
#' @param cluster.variable Character. The name of the variable defining clusters. No default.
#' @param data A data.frame. No default.
#' @param digits Numeric. The number of significant digits. Defaults to 3.
#' @export
estimate_icc <- function(outcome, cluster.variable, data, digits = 3) {
    ## Check arguments
    for (argument in c(outcome, cluster.variable))
         assertthat::assert_that(is.character(argument) & length(argument) == 1)
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.numeric(digits))

    ## Build null model
    arguments <- list(as.formula(paste0(outcome, " ~ ", "( 1 | ", cluster.variable, ")")),
                      data = as.name("data"),
                      family = "binomial")
    M0 <- do.call(lme4::glmer, arguments)

    ## Estimate the intraclass correlation coefficient. According to
    ## Sommet et al. the 3.14159^2/3, or 3.29, "refers to the standard
    ## logistic distribution, that is, the assumed level-1 variance
    ## component: We take this assumed value, as the logistic
    ## regression model does not include level-1 residual"
    icc <- round(M0@theta[1]^2/ (M0@theta[1]^2 + (3.14159^2/3)), digits = digits)
    return (icc)
}
