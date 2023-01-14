#' Estimate Intraclass Correlation Coefficient
#'
#' Uses the method described in Sommet, N. and Morselli, D. (2017). Keep Calm and Learn Multilevel Logistic Modeling: A Simplified Three-Step Procedure Using Stata, R, Mplus, and SPSS. International Review of Social Psychology, 30(1), 203–218, DOI: https://doi.org/10.5334/irsp.90
#' @param data A data.frame. No default.
#' @export
estimate_icc <- function(data) {
    ## Check arguments
    assert_that(is.data.frame(data))

    ##
    M0 <- glmer(bieber ~ ( 1 | classes), data = data, family = "binomial")
    summary(M0)

    icc <- M0@theta[1]^2/ (M0@theta[1]^2 + (3.14159^2/3)) # See notes below  
    return (icc)
}

## Notes
## 
## From Sommet et al: 3.14159^2/3, or 3.29, "refers to the standard
## logistic distribution, that is, the assumed level-1 variance
## component: We take this assumed value, as the logistic regression
## model does not include level-1 residual"
