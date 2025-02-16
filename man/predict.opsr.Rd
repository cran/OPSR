% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict.opsr}
\alias{predict.opsr}
\title{Predict Method for OPSR Model Fits}
\usage{
\method{predict}{opsr}(
  object,
  newdata,
  group,
  counterfact = NULL,
  type = c("response", "unlog-response", "prob", "mills"),
  ...
)
}
\arguments{
\item{object}{an object of class \code{"opsr"}.}

\item{newdata}{an optional data frame in which to look for variables used in
\code{object$formula}. See also \code{\link{model.matrix.opsr}}.}

\item{group}{predict outcome of this group (regime).}

\item{counterfact}{counterfactual group.}

\item{type}{type of prediction. Can be abbreviated. See 'Details' section for
more information.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
a vector of length \code{nrow(newdata)} (or data used during estimation).
}
\description{
Obtains predictions for the selection process (probabilities), the outcome process,
or returns the inverse mills ratio. Handles also log-transformed outcomes.
}
\details{
Elements are \code{NA_real_} if the \code{group} does not correspond to the observed
regime (selection outcome). This ensures consistent output length.

If the \code{type} argument is \code{"response"} then the continuous outcome is predicted.
Use \code{"unlog-response"} if the outcome response was log-transformed during estimation.
\code{"prob"} returns the probability vector of belonging to \code{group} and \code{"mills"}
returns the inverse mills ratio.
}
\examples{
sim_dat <- opsr_simulate()
dat <- sim_dat$data
model <- ys | yo ~ xs1 + xs2 | xo1 + xo2
fit <- opsr(model, dat)
p <- predict(fit, group = 1, type = "response")

fit_log <- update(fit, . | log(yo) ~ .)
p_unlog <- predict(fit, group = 1, type = "unlog-response")
}
\seealso{
\code{\link[stats:predict]{stats::predict}}
}
