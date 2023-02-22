#' Create a module
#'
#' This function creates a `{tm}` module class inside the current folder.
#'
#' @param name The class name of the module.
#' @param path Where to created the file. Default is `getwd()`. The function will add `R` to the path if the sub-folder exists.
#' @param prefix filename prefix. Default is `tm`. Set to `NULL`` to disable.
#' @param inherit Parent module class. Default is TidyModule.
#' @param open Should the file be opened?
#' @param dir_create Creates the directory if it doesn't exist, default is `TRUE`.
#' @param export Logical. Should the module be exported? Default is `FALSE`.
#' @note As a convention, this function will automatically capitalize the first character of the `name` argument.
#'
#' @importFrom cli cat_bullet
#' @importFrom utils file.edit
#' @importFrom fs path_abs path file_create
#' @importFrom snippr snippets_read
#'
#' @export
add_module <- function(name,
                       inherit = "TidyModule",
                       path = getwd(),
                       prefix = "tm",
                       open = TRUE,
                       dir_create = TRUE,
                       export = FALSE) {
  name <- file_path_sans_ext(name)
  # Capitalize
  name <- paste0(toupper(substring(name, 1, 1)), substring(name, 2))

  dir_created <- create_if_needed(
    fs::path(path),
    type = "directory"
  )
  if (!dir_created) {
    cat_red_bullet(
      "File not added (needs a valid directory)"
    )
    return(invisible(FALSE))
  }

  if (dir.exists(fs::path(path, "R"))) {
    path <- fs::path(path, "R")
  }

  old <- setwd(path_abs(path))
  on.exit(setwd(old))

  where <- fs::path(
    paste0(ifelse(is.null(prefix), "", paste0(prefix, "_")), name, ".R")
  )

  if (!check_file_exist(where)) {
    cat_red_bullet(
      "File not created (already exists)"
    )
    return(invisible(FALSE))
  }

  # make sure the provided parent module is valid
  import <- NULL
  parent <- inherit
  # TidyModule object
  if (is(parent, "TidyModule")) {
    parent <- class(parent)[1]
  }
  # Load the class generator from the name
  if (class(parent) == "character") {
    tryCatch(
      {
        parent <- eval(parse(text = parent))
      },
      error = function(e) {
        cat_red_bullet(
          paste0("Could not find module defined with 'inherit' = ", inherit)
        )
        return(invisible(FALSE))
      }
    )
  }
  # Retrieve package dependency and parent module name
  if (is(parent, "R6ClassGenerator")) {
    clist <- get_R6CG_list(parent)
    if ("TidyModule" %in% clist) {
      import <- environmentName(parent$parent_env)
      if (import == "R_GlobalEnv") {
        import <- NULL
      }
      parent <- clist[1]
    } else {
      cat_red_bullet(
        paste0("Could not find module defined with 'inherit' = ", deparse(substitute(inherit)))
      )
      return(invisible(FALSE))
    }
  }


  # Retrieve content from package snippet
  file_content <- snippr::snippets_read(path = system.file("rstudio/r.snippets", package = "tidymodules"))$tm.mod.new
  file_content <- unlist(strsplit(file_content, "\\n"))
  for (l in seq_len(length(file_content))) {
    # remove $ escapes \\
    file_content[l] <- sub("\\$", "$", file_content[l], fixed = TRUE)
    # remove tabs
    file_content[l] <- sub("\t", "", file_content[l])
    # remove snippet placeholders
    file_content[l] <- gsub("\\$\\{\\d+:(\\w+)\\}", "%\\1", file_content[l])
    # remove cursor pointer
    file_content[l] <- sub("\\$\\{0\\}", "", file_content[l])
    # substitute module name
    if (grepl("MyModule", file_content[l])) {
      file_content[l] <- gsub("MyModule", "s", file_content[l])
      file_content[l] <- sprintf(file_content[l], name)
    }
    # substitute parent module
    if (grepl("TidyModule", file_content[l])) {
      file_content[l] <- gsub("TidyModule", "s", file_content[l])
      file_content[l] <- sprintf(file_content[l], parent)
    }
    # manage export
    if (grepl("@export", file_content[l])) {
      if (!export) {
        file_content[l] <- "#' @noRd "
      }
      if (!is.null(import)) {
        file_content[l] <- paste0("#'\n#' @import ", import, "\n", file_content[l])
      }
    }
  }
  writeLines(file_content, where, sep = "\n")

  cat_created(fs::path(path, where))
  open_or_go_to(where, open)
}

# bunch of utility functions copied from golem
# WILL FACILITATE MIGRATING THIS FUNCTION TO GOLEM

#' @importFrom utils menu
yesno <- function(...) {
  cat(paste0(..., collapse = ""))
  menu(c("Yes", "No")) == 1
}

#' @importFrom fs file_exists
check_file_exist <- function(file) {
  res <- TRUE
  if (file_exists(file)) {
    cat_orange_bullet(file)
    res <- yesno("This file already exists, override?")
  }
  return(res)
}

#' @importFrom fs dir_create file_create
create_if_needed <- function(path,
                             type = c("file", "directory"),
                             content = NULL) {
  type <- match.arg(type)
  # Check if file or dir already exist
  if (type == "file") {
    dont_exist <- file_not_exist(path)
  } else if (type == "directory") {
    dont_exist <- dir_not_exist(path)
  }
  # If it doesn't exist, ask if we are allowed
  # to create it
  if (dont_exist) {
    ask <- yesno(
      sprintf(
        "The %s %s doesn't exist, create?",
        basename(path),
        type
      )
    )
    # Return early if the user doesn't allow
    if (!ask) {
      return(FALSE)
    } else {
      # Create the file
      if (type == "file") {
        if (dir_not_exist(dirname(path))) {
          dir_create(dirname(path), recurse = TRUE)
        }
        file_create(path)
        write(content, path, append = TRUE)
      } else if (type == "directory") {
        dir_create(path, recurse = TRUE)
      }
    }
  }

  # TRUE means that file exists (either
  # created or already there)
  return(TRUE)
}


#' @importFrom cli cat_bullet
cat_green_tick <- function(...) {
  cat_bullet(
    ...,
    bullet = "tick",
    bullet_col = "green"
  )
}

#' @importFrom cli cat_bullet
cat_red_bullet <- function(...) {
  cat_bullet(
    ...,
    bullet = "bullet",
    bullet_col = "red"
  )
}

#' @importFrom cli cat_bullet
cat_orange_bullet <- function(...) {
  cat_bullet(
    ...,
    bullet = "bullet",
    bullet_col = "orange"
  )
}

#' @importFrom cli cat_bullet
cat_info <- function(...) {
  cat_bullet(
    ...,
    bullet = "arrow_right",
    bullet_col = "grey"
  )
}

#' @importFrom fs path_file
cat_exists <- function(where) {
  cat_red_bullet(
    sprintf(
      "%s already exists, skipping the copy.",
      path_file(where)
    )
  )
  cat_info(
    sprintf(
      "If you want replace it, remove the %s file first.",
      path_file(where)
    )
  )
}

cat_created <- function(where,
                        file = "File") {
  cat_green_tick(
    sprintf(
      "%s created at %s",
      file,
      where
    )
  )
}

open_or_go_to <- function(where,
                          open) {
  if (
    rstudioapi::isAvailable() &&
      open &&
      rstudioapi::hasFun("navigateToFile")
  ) {
    rstudioapi::navigateToFile(where)
  } else {
    cat_red_bullet(
      sprintf(
        "Go to %s",
        where
      )
    )
  }
}

desc_exist <- function(pkg) {
  file_exists(
    paste0(pkg, "/DESCRIPTION")
  )
}


file_created_dance <- function(where,
                               fun,
                               pkg,
                               dir,
                               name,
                               open) {
  cat_created(where)

  fun(pkg, dir, name)

  open_or_go_to(where, open)
}

if_not_null <- function(x, ...) {
  if (!is.null(x)) {
    force(...)
  }
}

set_name <- function(x, y) {
  names(x) <- y
  x
}

# FROM tools::file_path_sans_ext() & tools::file_ext
file_path_sans_ext <- function(x) {
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

file_ext <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

#' @importFrom fs dir_exists file_exists
dir_not_exist <- Negate(dir_exists)
file_not_exist <- Negate(file_exists)
