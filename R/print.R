#' Print Method for Summary OPSR Objects
#'
#' @param x and object of class `"summary.opsr"`
#'
#' @param digits minimum number of significant digits to be used for most numbers (passed to [`stats::printCoefmat`]).
#' @param ... further arguments passed to or from other methods.
#'
#' @return Prints summary in 'pretty' form and returns `x` invisibly.
#'
#' @method print summary.opsr
#'
#' @seealso [`stats::printCoefmat`], [`summary.opsr`]
#' @export
print.summary.opsr <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  ## output formatting for wald tests
  cat_wald <- function(wald, note) {
    r <- function(x) round(x, digits = digits)
    cat("Wald chi2 (", note, "): ", r(wald$chisq), " on ", wald$df, " DF, ",
        "p-value: < ", r(wald$pval), "\n", sep = "")
  }

  with(x, {
    cat("--------------------------------------------\n")
    cat("Ordinal probit switching regression\n")
    cat(maxim_type, ", ", iterations, " iterations\n", sep = "")
    cat("Return code ", return_code, ": ", message, "\n", sep = "")
    cat("Runtime: ", format(unclass(runtime), digits = 3), " ", attr(runtime, "units"), "\n", sep = "")
    cat("Log-Likelihood:", GOF$LLfinal, "\n")
    cat("AIC:", GOF$AIC, "\n")
    cat("BIC:", GOF$BIC, "\n")
    cat("Number of regimes:", nReg, "\n")
    cat("Number of observations: "); cat(nObs[1], "("); cat(nObs[-1], sep = ", "); cat(")\n")

    cat("Estimated parameters:", nParams, "\n")
    cat("Estimates:\n")
    stats::printCoefmat(coef_table, digits = digits)
    cat("\n")
    cat_wald(wald$null, "null")
    cat_wald(wald$rho, "rho")
    cat("--------------------------------------------\n")
  })
  invisible(x)
}
