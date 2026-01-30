.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Learn CroptimizR at: https://SticsRPacks.github.io/CroptimizR"
  )
  packageStartupMessage("")

  cite <- utils::citation("CroptimizR")
  # Remove underscores around titles (used by R to indicate italics)
  cite_text <- gsub("_([^_]*)_", "\\1", format(cite, style = "text"))

  packageStartupMessage("Please cite CroptimizR in your work:")
  packageStartupMessage(format(cite_text, style = "text"))
}
