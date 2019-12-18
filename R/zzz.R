text_col <- function(x) {
  # This function is adapted from: https://github.com/tidyverse/tidyverse
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    text_col(paste(
      "Learn CroptimizR at:",
      crayon::blue$underline$bold("https://SticsRPacks.github.io/CroptimizR")
    ))
  )
}
