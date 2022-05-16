#' Sanitize R's version
#'
#' Used to validate R version, strip whitespaces and, optionally, to stripp off
#' leading inequalities.
#'
#' @inheritParams create_app
#' @param clean Boolean. If TRUE, \code{><=} are removed. Defaults to FALSE.
#' @param R_version_min Minimal supported R version. If \code{NULL} check won't
#'   be done. Default value is \code{"3.0.2"}.
#' @keywords internal
#' @export
sanitize_R_version <- function(R_version, clean = FALSE, R_version_min = "3.0.2"){

  # Remove spaces
  R_version <- gsub(" ", "", R_version)

  # Check for valid R version
  test <- gsub("^[<>=]+", "", R_version)
  test_vec <- strsplit(test, "\\.")[[1]]
  # add patchlevel version "0" if none given
  if (length(test_vec) < 3) test_vec[3] = "0"
  test.full <- paste(test_vec, collapse = ".")
  R_version <- gsub(test, test.full, R_version)
  rsv <- tryCatch(R_system_version(test.full),
                  error = function(e) stop(glue::glue("R_version ({test}) is not valid."), call. = FALSE))
  # Check for minimal supported R version
  if (!is.null(R_version_min) && rsv < R_version_min) {
    stop(glue::glue("R_version ({test}) <= {R_version_min} is not supported."), call. = FALSE)
  }

  # Check the inequality
  if (grepl("[<>=]", R_version)) {
    breakpoint <- attr(regexpr("[<>=]+", R_version), "match.length")
    inequality <- substr(R_version, 1, breakpoint)
    if (grepl("=[<>]", inequality)) {
      stop(glue::glue("R_version's inequality, {inequality}, is not a valid logical operator"), call. = FALSE)
    } else if (nchar(inequality) == 1) {
      stop(glue::glue("RInno only supports >=, <= and == in R_version"), call. = FALSE)
    } else if (breakpoint > 2) {
      stop(glue::glue("R_version = {R_version} is not supported."), call. = FALSE)
    }

  } else {
    # add >= if no inequality is specified
    R_version <- paste0(">=", R_version)
  }
  if (clean) R_version <- gsub("[<>=]", "", R_version)
  return(R_version)
}
