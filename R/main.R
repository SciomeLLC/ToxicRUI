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
  "Least Mean Squares" = "LMS",
  "Hill" = "hill",
  "Exponential-3" = "exp-3",
  "Exponential-5" = "exp-5",
  "Power" = "power",
  "Polynomial" = "polynomial"
)

modelTypes_dich <- c(
  "Hill" = "hill",
  "Gamma" = "gamma",
  "Logistic" = "logistic",
  "Log-Logistic" = "log-logistic",
  "Log-Probit" = "log-probit",
  "Multistage" = "multistage",
  "Probit" = "probit",
  "Q-Linear" = "qlinear",
  "Weibull" = "weibull"
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
  "Continuous - Full Data" = "continuous",
  "Dichotomous" = "dichotomous",
  "Continuous - Summary Stats" = "continuous-summary"
)

averageOrFit <- c(
  "Model Averaging" = "average",
  "Individual Model" = "fit"
)
