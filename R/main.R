# Define the options for each parameter
modelTypes <- c(
  "Exponential Aerts" = "exp-aerts",
  "Inverse Exponential Aerts" = "invexp-aerts",
  "Hill Aerts" = "hill-aerts",
  "Gamma Aerts" = "gamma-aerts",
  "Inverse Gamma Aerts" = "invgamma-aerts",
  "Lomax Aerts" = "lomax-aerts",
  "Inverse Lomax Aerts" = "invlomax-aerts",
  "Log-Normal Aerts" = "lognormal-aerts",
  "Log-Skew-Normal Aerts" = "logskew-aerts",
  "Inverse Log-Skew-Normal Aerts" = "invlogskew-aerts",
  "Logistic Aerts" = "logistic-aerts",
  "Probit Aerts" = "probit-aerts",
  "Gamma EFSA" = "gamma-efsa",
  "Least Mean Squares" = "LMS"
)

fitTypes <- c("laplace", "mle", "mcmc")

bmrTypes <- c(
  "Standard Deviation" = "sd",
  "Relative" = "rel",
  "Hybrid" = "hybrid",
  "Absolute" = "abs"
)

distributions <- c(
  "Normal" = "normal",
  "Normal with normalized coefficient of variation" = "normal-ncv",
  "Lognormal" = "lognormal"
)

outcomeTypes <- c(
  "Continuous" = "continuous",
  "Dichotomous" = "dichotomous"
)

averageOrFit <- c(
  "Model Averaging" = "average",
  "Individual Model" = "fit"
)
