#'
#' make_double_pipe: Pipe function generator
#'
#' Create a pipe function for mapping a module output to a module input
#'
#' @param l Left module.
#' @param r Right module.
#' @param f fast boolean. Used with the '>>' or '<<' signs to return the item at the opposite of the arrow.
#' @param rev Reverse operation. Boolean that indicates if this is a reverse operation, i.e. '<' or '<<'.
#'
#' @keywords internal
make_double_pipe <- function(l, r, f = FALSE, rev = FALSE) {
  left_id <- l
  right_id <- r
  fast <- f
  reverse <- rev
  return(
    function(l_mod, r_mod) {
      if (!is(l_mod, "TidyModule")) {
        stop(paste0(deparse(substitute(l_mod)), " is not a Module"))
      }
      if (!is(r_mod, "TidyModule")) {
        stop(paste0(deparse(substitute(r_mod)), " is not a Module"))
      }

      # Make sure we don't use mods from the global session
      l_mod <- getMod(l_mod$module_ns)
      r_mod <- getMod(r_mod$module_ns)

      from <- list(mod = l_mod, port = left_id)
      to <- list(mod = r_mod, port = right_id)
      if (reverse) {
        to <- list(mod = l_mod, port = left_id)
        from <- list(mod = r_mod, port = right_id)
      }

      to$mod$updateInputPort(
        id = to$port,
        input = from$mod$getOutput(id = from$port)
      )

      (to$mod$getStore())$addEdge(
        from = list(type = "output", m = from$mod, port = from$port),
        to = list(type = "input", m = to$mod, port = to$port)
      )

      if (fast) {
        from$mod
      } else {
        to$mod
      }
    }
  )
}

#' make_single_pipe: Pipe function generator
#'
#' Create a pipe function for mapping a reactive expression/value to a module input
#'
#' @inheritParams make_double_pipe
#'
#' @keywords internal
make_single_pipe <- function(p = NULL, f = FALSE, rev = FALSE) {
  port_id <- p
  fast <- f
  reverse <- rev
  return(
    function(left, right) {
      from <- left
      to <- list(mod = right, port = port_id)
      if (reverse) {
        to <- list(mod = left, port = port_id)
        from <- right
      }


      if (!is.reactivevalues(from) &&
        !is.reactive(from)) {
        stop(paste0(deparse(substitute(from)), " is not reactive"))
      }
      if (!is(to$mod, "TidyModule")) {
        stop(paste0(deparse(substitute(to$mod)), " is not a Module"))
      }

      # getMod function below enforce the use of modules from the user session
      # even if the reference object was created in the global session
      to$mod <- getMod(to$mod$module_ns)

      to$mod$updateInputPort(
        id = to$port,
        input = from
      )

      (to$mod$getStore())$addEdge(
        from = list(type = NA, m = from, port = NA),
        to = list(type = "input", m = to$mod, port = to$port)
      )

      if (fast) {
        from
      } else {
        to$mod
      }
    }
  )
}


#' multi_port_map
#'
#' Pipe function for sequentially mapping left module outputs to
#' right module inputs
#'
#' @param l_mod Left module.
#' @param r_mod Right module.
#' @param f fast boolean. Used with the '>>' or '<<' signs to return the item at the opposite of the arrow.
#' @param t mapping type. Default to NULL. Could also be 'input' for mapping input to input.
#'
#' @keywords internal
multi_port_map <- function(l_mod, r_mod, f = FALSE, t = NULL) {
  if (!is(l_mod, "TidyModule")) {
    stop(paste0(deparse(substitute(l_mod)), " is not a Module"))
  }
  if (!is(r_mod, "TidyModule")) {
    stop(paste0(deparse(substitute(r_mod)), " is not a Module"))
  }

  # Make sure we don't use mods from the global session
  l_mod <- getMod(l_mod$module_ns)
  r_mod <- getMod(r_mod$module_ns)

  outs <- l_mod$getOutputPorts()
  if (!is.null(t) && t == "input") {
    outs <- l_mod$getInputPorts()
  }
  ins <- r_mod$getInputPorts()
  if (!is.null(t) && t == "output") {
    ins <- l_mod$getOutputPorts()
  }
  if (!is.null(t)) {
    if (t == "input" && !is.null(outs)) {
      r_mod$updateInputPorts(outs, is_parent = TRUE)
      for (n in names(outs)) {
        if (outs[[n]]$inherit) {
          (r_mod$getStore())$addEdge(
            from = list(type = "input", m = l_mod, port = n),
            to =   list(type = "input", m = r_mod, port = n)
          )
        }
      }
    }
  } else {
    if (length(outs) != length(ins)) {
      stop(paste0(
        "Modules ports length error: ",
        deparse(substitute(l_mod)), " [", length(names(outs)), "] / ",
        deparse(substitute(r_mod)), " [", length(names(ins)), "]"
      ))
    }

    for (n in names(outs)) {
      r_mod$updateInputPort(
        id = n,
        input = l_mod$getOutputPort(id = n)
      )
      (r_mod$getStore())$addEdge(
        from = list(type = "output", m = l_mod, port = n),
        to = list(type = "input", m = r_mod, port = n)
      )
    }
  }


  if (f) {
    l_mod
  } else {
    r_mod
  }
}


pipes <- list(
  maxPort = 10, # "maxPort" as maximum number of port
  forward = list(
    normal = list(
      simple = c(),
      double = c()
    ),
    fast = list(
      simple = c(),
      double = c()
    )
  ),
  reverse = list(
    normal = list(
      simple = c(),
      double = c()
    ),
    fast = list(
      simple = c(),
      double = c()
    )
  )
)

for (rp in 1:pipes$maxPort) {
  p <- paste0("%>", rp, "%")
  pipes$forward$normal$simple <- c(pipes$forward$normal$simple, p)
  assign(p, make_single_pipe(rp))

  p <- paste0("%>>", rp, "%")
  pipes$forward$fast$simple <- c(pipes$forward$fast$simple, p)
  assign(p, make_single_pipe(rp, f = TRUE))

  for (lp in 1:pipes$maxPort) {
    p <- paste0("%", lp, ">", rp, "%")
    pipes$forward$normal$double <- c(pipes$forward$normal$double, p)
    assign(p, make_double_pipe(lp, rp))

    p <- paste0("%", lp, ">>", rp, "%")
    pipes$forward$fast$double <- c(pipes$forward$fast$double, p)
    assign(p, make_double_pipe(lp, rp, f = TRUE))

    p <- paste0("%", lp, "<", rp, "%")
    pipes$reverse$normal$double <- c(pipes$reverse$normal$double, p)
    assign(p, make_double_pipe(lp, rp, rev = TRUE))

    p <- paste0("%", lp, "<<", rp, "%")
    pipes$reverse$fast$double <- c(pipes$reverse$fast$double, p)
    assign(p, make_double_pipe(lp, rp, f = TRUE, rev = TRUE))
  }
}

#' port_map
#'
#' port mapping function.
#'
#' @param lp Left port
#' @param rp Right port
#' @param f Forward operartion. Boolean.
#'
#' @keywords internal
port_map <- function(lp, rp, f = FALSE) {
  isolate({
    # ---- Checks for rp - the right port

    # Make sure rp is a tidymodules input port
    if (is.null(attr(rp, "tidymodules")) || attr(rp, "tidymodules_port_type") != "input") {
      stop(paste0(deparse(substitute(rp)), " is not a tidymodules input port"))
    }
    # Make sure it is an actual port definition or port slot
    if (!is.null(attr(rp, "tidymodules_port_slot")) && attr(rp, "tidymodules_port_slot")) {
      warning(paste0(deparse(substitute(rp)), " is a port slot. You should rather use one of the port function to retrieve the right port.\nSee iport(...), m$iport(...) and m$getInputPort(...)."))
    }
    mod <- isolate(getMod(attr(rp, "tidymodules_module_ns")))
    port_id <- attr(rp, "tidymodules_port_id")

    # ---- Checks for lp - the left port

    # Make sure lp is a tidymodules objects (output port or derived port)
    if (!is.null(attr(lp, "tidymodules")) && attr(lp, "tidymodules")) {
      if (!is.null(attr(lp, "tidymodules_port_type")) && attr(lp, "tidymodules_port_type") != "output") {
        stop(paste0(deparse(substitute(lp)), " is not a tidymodules output port"))
      }
      # Or a reactive function
    } else if (!is.reactive(lp)) {
      stop(paste0(deparse(substitute(lp)), " is not a reactive function"))
    }
  })

  fct <- make_single_pipe(port_id)
  fct(lp, mod)

  if (f) {
    invisible(lp)
  } else {
    invisible(rp)
  }
}


#'
#' @title Port mapping function (port level)
#'
#' @description This pipe works at the port level where left and right object are ports not modules.
#' Take the left port and maps it to the right port.
#'
#' @param lp Left port.
#' @param rp Right port.
#'
#' @return The right port
#'
#' @export
"%->%" <- function(lp, rp) {
  return(port_map(lp, rp))
}

#'
#' @title Port mapping function (port level) - forward version
#'
#' @description This pipe works at the port level where left and right object are ports not modules.
#' Take the left port and maps it to the right port. This is a forward version, i.e. return the left item.
#'
#' @param lp Left port.
#' @param rp Right port.
#'
#' @return The left port
#'
#' @export
"%->>%" <- function(lp, rp) {
  return(port_map(lp, rp, TRUE))
}

#'
#' @title Multi-port mapping function
#'
#' @description This pipe maps all the left output ports to the right input ports.
#'
#' @param l left module.
#' @param r right module.
#'
#' @return The right module
#'
#' @export
"%:>:%" <- function(l, r) {
  return(multi_port_map(l, r))
}

#'
#' @title Multi-port mapping function
#'
#' @description This pipe maps all the left output ports to the right input ports.
#'
#' @param l left module.
#' @param r right module.
#'
#' @return The left module.
#'
#' @export
"%:>>:%" <- function(l, r) {
  return(multi_port_map(l, r, f = TRUE))
}

#'
#' @title Multi-port mapping function
#'
#' @description This pipe maps all the left input ports to the right input ports.
#'
#' @param l left module.
#' @param r right module.
#'
#' @return The right module.
#'
#' @export
"%:i:%" <- function(l, r) {
  return(multi_port_map(l, r, t = "input"))
}

ns_export <- function(pnames) {
  paste0(sprintf("export(\"%s\")", pnames), collapse = "\n")
}

rd_gen <- function(pnames, pipesFamily) {
  description <- switch(pipesFamily,
    "%x>y%" = "This pipe works at the module level.
    It maps the left module's output port defined by the left
    number (x) of the pipe operator to the right module's
    input port defined by the right number (y).",
    "%x>>y%" = "This pipe works at the module level.
    It maps the left module's output port defined by the left number (x)
    of the pipe operator to the right module's input port defined by the
    right number (y).",
    "%x<y%" = "This pipe works at the module level.
    It maps the right module's output port defined by the right number (y)
    of the pipe operator to the left module's input port defined by the
    left number (x).",
    "%x<<y%" = "This pipe works at the module level.
    It maps the right module's output port defined by the
    right number (y) of the pipe operator to the left module's
    input port defined by the left number (x).",
    "%>y%" = "This pipe maps the left object (must be a reactive function
    or a reactivevalues object) to the right module's input port
    defined by the number in the operator (y).",
    "%>>y%" = "This pipe maps the left object (must be a reactive function
    or a reactivevalues object) to the right module's input port defined by
    the number in the operator (y)."
  )

  title <- switch(pipesFamily,
    "%x>y%" = "Single-port mapping function",
    "%x>>y%" = "Single-port mapping function",
    "%x<y%" = "Single-port mapping function (Reverse version)",
    "%x<<y%" = "Single-port mapping function (Reverse version)",
    "%>y%" = "Input port mapping function",
    "%>>y%" = "Input port mapping function"
  )

  rd <- c(
    sprintf("@title %s", title),
    sprintf("@aliases %s", paste(pnames)),
    sprintf("@description %s", description),
    sprintf("@name %s", pipesFamily)
  )

  rd
}

#' @eval rd_gen(pipes$forward$normal$double, "%x>y%")
#' @evalNamespace  ns_export(pipes$forward$normal$double)
NULL

#' @eval rd_gen(pipes$forward$fast$double, "%x>>y%")
#' @evalNamespace  ns_export(pipes$forward$fast$double)
NULL

#' @eval rd_gen(pipes$forward$fast$simple, "%>>y%")
#' @evalNamespace  ns_export(pipes$forward$fast$simple)
NULL

#' @eval rd_gen(pipes$forward$normal$simple, "%>y%")
#' @evalNamespace  ns_export(pipes$forward$normal$simple)
NULL

#' @eval rd_gen(pipes$reverse$fast$double, "%x<<y%")
#' @evalNamespace  ns_export(pipes$reverse$fast$double)
NULL

#' @eval rd_gen(pipes$reverse$normal$double, "%x<y%")
#' @evalNamespace  ns_export(pipes$reverse$normal$double)
NULL
