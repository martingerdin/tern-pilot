#' Calculate bootstrap confidence intervals
#'
#' This function calculates bootstrap confidence intervals for a given set of bootstrapped outcome results.
#'
#' @param bootstrapped.outcome.results A list of bootstrapped outcome results.
#' @param ci.levels A numeric vector specifying the confidence levels for the intervals.
#' @param ci.type A character string specifying the type of confidence interval calculation.
#'
#' @return A list of confidence intervals for each bootstrapped outcome result.
#'
#' @details This function uses the boot::boot.ci function to calculate the confidence intervals.
#' It iterates over each bootstrapped outcome result and calls the calculate_bootstrap_ci function to calculate the interval.
#' If an error occurs during the calculation, a message is printed and a dummy object is returned.
#' The resulting confidence intervals are stored in a list with names corresponding to the bootstrapped outcome results.
#'
#' @importFrom assertthat assert_that
#' @importFrom boot boot.ci
#' @export
calculate_bootstrap_cis <- function(
    bootstrapped.outcome.results,
    ci.levels = c(0.75, 0.85, 0.95),
    ci.type = "basic") {
    ## Check arguments
    assertthat::assert_that(
        is.list(bootstrapped.outcome.results),
        is.numeric(ci.levels),
        is.character(ci.type)
    )

    ## Calculate confidence intervals
    dummy.object <- structure(rep(NA, 9), dim = c(
        3L,
        3L
    ), dimnames = list(NULL, c("level", "lower", "upper")))
    bootstrapped.outcome.results.ci <- lapply(
        seq_along(bootstrapped.outcome.results$t0),
        function(index) {
            t0.value <- bootstrapped.outcome.results$t0[index]
            ci <- dummy.object
            if (!is.na(t0.value)) {
                ci <- tryCatch(
                    calculate_bootstrap_ci(
                        bootstrapped.outcome.results,
                        index,
                        ci.type,
                        ci.levels
                    ),
                    error = function(e) {
                        message(paste0("Error calculating CI for index ", index))
                        message(e)
                        return(dummy.object)
                    }
                )
            }
            return(ci)
        }
    )
    names(bootstrapped.outcome.results.ci) <- names(bootstrapped.outcome.results$t0)
    return(bootstrapped.outcome.results.ci)
}

#' Calculate bootstrap confidence interval
#'
#' This function calculates a bootstrap confidence interval for a specific bootstrapped outcome result.
#'
#' @param bootstrapped.outcome.results A list of bootstrapped outcome results.
#' @param index The index of the bootstrapped outcome result to calculate the interval for.
#' @param ci.type A character string specifying the type of confidence interval calculation.
#' @param ci.levels A numeric vector specifying the confidence levels for the interval.
#'
#' @importFrom boot boot.ci
#' @export
calculate_bootstrap_ci <- function(
    bootstrapped.outcome.results,
    index,
    ci.type,
    ci.levels) {
    boot.ci.object <-
        boot::boot.ci(
            boot.out = bootstrapped.outcome.results,
            index = index,
            type = ci.type,
            conf = ci.levels
        )
    ci <- boot.ci.object[[ci.type]][, c(1, 4:5)]
    colnames(ci) <- c("level", "lower", "upper")
    return(ci)
}
