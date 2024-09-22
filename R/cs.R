#' Show Time Series Cheat Sheet
#'
#' This function opens the time series cheat sheet in the default browser or shows it in the console.
#'
#' @export
show <- function() {
  cheat_path <- system.file("cheatsheet.md", package = "tscs")

  if (cheat_path == "") {
    stop("Cheat sheet not found. Make sure the file exists in the package's 'inst' folder.")
  }

  # Convert the file path to a URL-friendly format for Windows
  cheat_url <- if (.Platform$OS.type == "windows") {
    paste0("file:///", gsub("\\\\", "/", cheat_path))
  } else {
    cheat_path
  }

  if (interactive()) {
    utils::browseURL(cheat_url)
  } else {
    cat(readLines(cheat_path), sep = "\n")
  }
}
