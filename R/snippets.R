#'
#' @title Add `{tm}` snippets to RStudio
#'
#' @description This function adds useful `{tm}` code snippets to RStudio.
#'
#' @param force Force the re-installation when the snippets are already installed.
#'
#' @import snippr
#' @import dplyr
#' @importFrom fs path_home_r path_ext_set
#' @importFrom  cli cat_bullet
#' @importFrom  purrr keep discard map
#' @export
add_tm_snippets <- function(force = FALSE) {
  # R snippets file
  path <- path_home_r(".R", "snippets", path_ext_set("r", "snippets"))

  if (!create_if_needed(path)) {
    cat_bullet("Skip installation of snippets",
      bullet_col = "red",
      bullet = "bullet"
    )
  }

  # retrieve current and new snippets
  current_all_snippets <- snippets_get(path = path)
  current_non_tm_snippets <- current_all_snippets[!grepl("^tm\\.", names(current_all_snippets), perl = TRUE)]
  current_tm_snippets <- current_all_snippets[grepl("^tm\\.", names(current_all_snippets), perl = TRUE)]
  new_tm_snippets <- snippets_read(path = system.file("rstudio/r.snippets", package = "tidymodules"))
  # calculate differences
  del_snippets <- setdiff(names(current_tm_snippets), names(new_tm_snippets))
  keep_snippets <- intersect(names(current_tm_snippets), names(new_tm_snippets))
  add_snippets <- setdiff(names(new_tm_snippets), names(current_tm_snippets))
  # print some informations
  if (length(del_snippets) > 0) {
    cat_bullet(paste0("Deleting ", length(del_snippets), " snippet(s):"),
      bullet_col = "orange",
      bullet = "bullet"
    )
    invisible(map(del_snippets, cat_bullet, bullet = "dot"))
  }
  existing_snippets <- current_non_tm_snippets
  save_snippets <- NULL
  if (length(keep_snippets) > 0) {
    if (force) {
      cat_bullet(paste0("Re-installing ", length(keep_snippets), " existing snippet(s):"),
        bullet_col = "green",
        bullet = "tick"
      )
      invisible(map(keep_snippets, cat_bullet, bullet = "dot"))
      save_snippets <- new_tm_snippets[keep_snippets]
    } else {
      cat_bullet(paste0("Skip installation of ", length(keep_snippets), " existing snippet(s):"),
        bullet_col = "red",
        bullet = "bullet"
      )
      invisible(map(keep_snippets, cat_bullet, bullet = "dot"))
      existing_snippets <- c(existing_snippets, current_tm_snippets[keep_snippets])
    }
  }
  if (length(add_snippets) > 0) {
    cat_bullet(paste0("Installing ", length(add_snippets), " new snippets:"),
      bullet_col = "green",
      bullet = "tick"
    )
    invisible(map(add_snippets, cat_bullet, bullet = "dot"))
    save_snippets <- c(save_snippets, new_tm_snippets[add_snippets])
  }

  final_snippets <- existing_snippets
  if (!is.null(save_snippets)) {
    final_snippets <- c(final_snippets, save_snippets)
  }

  snippets_write(final_snippets, path = path)
}
