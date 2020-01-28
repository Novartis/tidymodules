
#' mkDoublePipe: Pipe function generator
#' 
#' Create a pipe function for mapping module output to module input
#' 
#' @keywords internal
mkDoublePipe <- function(l,r, f = TRUE , rev = FALSE){
  left_id   <- l
  right_id  <- r
  forward   <- f
  reverse   <- rev
  return(
    function(l_mod,r_mod){
      if(!is(l_mod,"TidyModule"))
        stop(paste0(deparse(substitute(l_mod))," is not a Module"))
      if(!is(r_mod,"TidyModule"))
        stop(paste0(deparse(substitute(r_mod))," is not a Module"))
      
      # Make sure we don't use mods from the global session
      l_mod <- getMod(l_mod$module_ns)
      r_mod <- getMod(r_mod$module_ns)
      
      from <- list( mod = l_mod, port = left_id)
      to <- list(mod = r_mod, port = right_id)
      if(!f){
        to <- list( mod = l_mod, port = left_id)
        from <- list(mod = r_mod, port = right_id)
      }
      
      to$mod$updateInputPort(
        id = to$port,
        input = from$mod$getOutput(id = from$port))
      
      (to$mod$getStore())$addEdge(
        from = list(type = "output", m = from$mod, port = from$port),
        to =   list(type = "input", m = to$mod, port = to$port)
      )
      
      if(reverse)
        from$mod
      else
        to$mod
    }
  )
}

#' mkSinglePipe: Pipe function generator
#' 
#' Create a pipe function for mapping a reactive expression/value to a module input
#' 
#' @keywords internal
mkSinglePipe <- function(p = NULL, f = TRUE , rev = FALSE){
  port_id   <- p
  forward   <- f
  reverse   <- rev
  return(
    function(left,right){
      
      from <- left
      to <- list(mod = right, port = port_id)
      if(!f){
        to <- list( mod = left, port = port_id)
        from <- right
      }
      
      
      if(!shiny::is.reactivevalues(from) &&
         !shiny::is.reactive(from) )
        stop(paste0(deparse(substitute(from))," is not reactive"))
      if(!is(to$mod,"TidyModule"))
        stop(paste0(deparse(substitute(to$mod))," is not a Module"))
      
      # getMod function below enforce the use of modules from the user session
      # even if the reference object was created in the global session
      to$mod <- getMod(to$mod$module_ns)
      
      to$mod$updateInputPort(
        id = to$port,
        input = from)
      
      (to$mod$getStore())$addEdge(
        from = list(type = NA, m = from,  port = NA),
        to =   list(type = "input", m = to$mod, port = to$port)
      )
      
      if(reverse)
        from
      else
        to$mod
    }
  )
}


#' multiPipeFunc
#' 
#' Pipe function for sequentially mapping left module outputs to 
#' right module inputs
#' 
#' 
#' @keywords internal
multiPipeFunc <- function(l_mod,r_mod,rev = FALSE, t = NULL){
  if(!is(l_mod,"TidyModule"))
    stop(paste0(deparse(substitute(l_mod))," is not a Module"))
  if(!is(r_mod,"TidyModule"))
    stop(paste0(deparse(substitute(r_mod))," is not a Module"))
  
  # Make sure we don't use mods from the global session
  l_mod <- getMod(l_mod$module_ns)
  r_mod <- getMod(r_mod$module_ns)
  
  outs <- l_mod$getOutputPorts()
  if(!is.null(t) && t == "input")
    outs <- l_mod$getInputPorts()
  ins <- r_mod$getInputPorts()
  if(!is.null(t) && t == "output")
    ins <- l_mod$getOutputPorts()
  if(!is.null(t)){
    if(t == "input" && !is.null(outs)){
      r_mod$updateInputPorts(outs)
      for (idx in 1:length(reactiveValuesToList(outs)))
        (r_mod$getStore())$addEdge(
          from = list(type = "input", m = l_mod, port = idx),
          to =   list(type = "input", m = r_mod, port = idx)
        )
    }
  }else{
    if(length(outs) != length(ins))
      stop(paste0("Modules ports length error: ",
                  deparse(substitute(l_mod))," [",length(outs),"] / ",
                  deparse(substitute(r_mod))," [",length(ins),"]"))
    
    for (idx in 1:length(outs)) {
      r_mod$updateInputPort(
        id = idx,
        input = l_mod$getOutputPort(id = idx))
      (r_mod$getStore())$addEdge(
        from = list(type = "output", m = l_mod, port = idx),
        to =   list(type = "input",m = r_mod, port = idx)
      )
    }
  }
  
  
  if(rev)
    l_mod
  else
    r_mod
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
  
  p <- paste0("%>",rp,"%")
  pipes$forward$normal$simple <- c(pipes$forward$normal$simple,p)
  assign(p,mkSinglePipe(rp))
  
  p <- paste0("%>>",rp,"%")
  pipes$forward$fast$simple <- c(pipes$forward$fast$simple,p)
  assign(p,mkSinglePipe(rp,rev = TRUE))
  
  for (lp in 1:pipes$maxPort) {
    
    p <- paste0("%",lp,">",rp,"%")
    pipes$forward$normal$double <- c(pipes$forward$normal$double,p);
    assign(p,mkDoublePipe(lp,rp))
    
    p <- paste0("%",lp,">>",rp,"%")
    pipes$forward$fast$double <- c(pipes$forward$fast$double,p);
    assign(p,mkDoublePipe(lp,rp,rev = TRUE))
    
    p <- paste0("%",lp,"<",rp,"%")
    pipes$reverse$normal$double <- c(pipes$reverse$normal$double,p);
    assign(p,mkDoublePipe(lp,rp,f = FALSE))
    
    p <- paste0("%",lp,"<<",rp,"%")
    pipes$reverse$fast$double <- c(pipes$reverse$fast$double,p);
    assign(p,mkDoublePipe(lp,rp,f = FALSE,rev = TRUE))
    
  }
}



# paste0(pipes$forward$normal$simple,collapse = " ")
# paste0(pipes$forward$fast$simple,collapse = " ")
# 
# paste0(pipes$forward$normal$double,collapse = " ")
# paste0(pipes$forward$fast$double,collapse = " ")
# paste0(pipes$reverse$normal$double,collapse = " ")
# paste0(pipes$reverse$fast$double,collapse = " ")

#' 
#' @title Port mapping function (port level)
#' 
#' @description This pipe works at the port level where left and right object are ports not modules.
#' Take the left port and maps it to the right port.
#' 
#' @return The module of the right port
#' 
#' @export
"%->%" <- function(lp,rp) {
  shiny::isolate({
  # Make sure rp is a tidymodules input port
    if(!attr(rp,"tidymodules") || attr(rp,"tidymodules_port_type") != "input")
      stop(paste0(deparse(substitute(rp))," is not a tidymodules input port" ))
    mod <- shiny::isolate(getMod(attr(rp,"tidymodules_module_ns")))
    port_id <- attr(rp,"tidymodules_port_id")
  })
  
  fct <- mkSinglePipe(port_id)
  
  return(fct(lp,mod)); 
}

#' 
#' @title Multi-port mapping function
#' 
#' @description This pipe maps all the left output ports to the right input ports.
#' 
#' @return The right module
#' 
#' @export
"%:>:%" <- function(l,r) { return(multiPipeFunc(l,r)); }


#' 
#' @title Multi-port mapping function
#' 
#' @description This pipe maps all the left input ports to the right input ports.
#' 
#' @return The right module
#' 
#' @export
"%:i:%" <- function(l,r) { return(multiPipeFunc(l,r,t="input")); }

# #' Forward input/input multi-port pipe
# #' 
# #' @rdname pipes
# #' @export %:o:%
# "%:o:%" <- function(l,r) { return(multiPipeFunc(l,r,t="output")); }

#' 
#' @title Multi-port mapping function
#' 
#' @description This pipe maps all the left output ports to the right input ports.
#' 
#' @return The left module
#' 
#' @export
"%:>>:%" <- function(l,r) { return(multiPipeFunc(l,r,rev = TRUE)); }

#' 
#' @title Single-port mapping function
#' 
#' @name %x>y%
#'
#' @description This pipe works at the module level.
#' It maps the left module's output port defined by the left number (x) of the pipe operator to the right module's input port defined by the right number (y).
#' 
#' @return The right module
#' 
#' @export %1>1% %1>2% %1>3% %1>4% %1>5% %1>6% %1>7% %1>8% %1>9% %1>10% %2>1% %2>2% %2>3% %2>4% %2>5% %2>6% %2>7% %2>8% %2>9% %2>10% %3>1% %3>2% %3>3% %3>4% %3>5% %3>6% %3>7% %3>8% %3>9% %3>10% %4>1% %4>2% %4>3% %4>4% %4>5% %4>6% %4>7% %4>8% %4>9% %4>10% %5>1% %5>2% %5>3% %5>4% %5>5% %5>6% %5>7% %5>8% %5>9% %5>10% %6>1% %6>2% %6>3% %6>4% %6>5% %6>6% %6>7% %6>8% %6>9% %6>10% %7>1% %7>2% %7>3% %7>4% %7>5% %7>6% %7>7% %7>8% %7>9% %7>10% %8>1% %8>2% %8>3% %8>4% %8>5% %8>6% %8>7% %8>8% %8>9% %8>10% %9>1% %9>2% %9>3% %9>4% %9>5% %9>6% %9>7% %9>8% %9>9% %9>10% %10>1% %10>2% %10>3% %10>4% %10>5% %10>6% %10>7% %10>8% %10>9% %10>10%
NULL

#' 
#' @title Single-port mapping function
#' 
#' @name %x>>y%
#'
#' @description This pipe works at the module level.
#' It maps the left module's output port defined by the left number (x) of the pipe operator to the right module's input port defined by the right number (y).
#' 
#' @return The left module
#' 
#' @export %1>>1% %2>>1% %3>>1% %4>>1% %5>>1% %6>>1% %7>>1% %8>>1% %9>>1% %10>>1% %1>>2% %2>>2% %3>>2% %4>>2% %5>>2% %6>>2% %7>>2% %8>>2% %9>>2% %10>>2% %1>>3% %2>>3% %3>>3% %4>>3% %5>>3% %6>>3% %7>>3% %8>>3% %9>>3% %10>>3% %1>>4% %2>>4% %3>>4% %4>>4% %5>>4% %6>>4% %7>>4% %8>>4% %9>>4% %10>>4% %1>>5% %2>>5% %3>>5% %4>>5% %5>>5% %6>>5% %7>>5% %8>>5% %9>>5% %10>>5% %1>>6% %2>>6% %3>>6% %4>>6% %5>>6% %6>>6% %7>>6% %8>>6% %9>>6% %10>>6% %1>>7% %2>>7% %3>>7% %4>>7% %5>>7% %6>>7% %7>>7% %8>>7% %9>>7% %10>>7% %1>>8% %2>>8% %3>>8% %4>>8% %5>>8% %6>>8% %7>>8% %8>>8% %9>>8% %10>>8% %1>>9% %2>>9% %3>>9% %4>>9% %5>>9% %6>>9% %7>>9% %8>>9% %9>>9% %10>>9% %1>>10% %2>>10% %3>>10% %4>>10% %5>>10% %6>>10% %7>>10% %8>>10% %9>>10% %10>>10%
NULL

#' 
#' @title Single-port mapping function (Reverse version)
#' 
#' @name %x<y%
#'
#' @description This pipe works at the module level.
#' It maps the right module's output port defined by the right number (y) of the pipe operator to the left module's input port defined by the left number (x).
#' 
#' @return The left module
#' 
#' @export %1<1% %1<2% %1<3% %1<4% %1<5% %1<6% %1<7% %1<8% %1<9% %1<10% %2<1% %2<2% %2<3% %2<4% %2<5% %2<6% %2<7% %2<8% %2<9% %2<10% %3<1% %3<2% %3<3% %3<4% %3<5% %3<6% %3<7% %3<8% %3<9% %3<10% %4<1% %4<2% %4<3% %4<4% %4<5% %4<6% %4<7% %4<8% %4<9% %4<10% %5<1% %5<2% %5<3% %5<4% %5<5% %5<6% %5<7% %5<8% %5<9% %5<10% %6<1% %6<2% %6<3% %6<4% %6<5% %6<6% %6<7% %6<8% %6<9% %6<10% %7<1% %7<2% %7<3% %7<4% %7<5% %7<6% %7<7% %7<8% %7<9% %7<10% %8<1% %8<2% %8<3% %8<4% %8<5% %8<6% %8<7% %8<8% %8<9% %8<10% %9<1% %9<2% %9<3% %9<4% %9<5% %9<6% %9<7% %9<8% %9<9% %9<10% %10<1% %10<2% %10<3% %10<4% %10<5% %10<6% %10<7% %10<8% %10<9% %10<10%
NULL

#' 
#' @title Single-port mapping function (Reverse version)
#' 
#' @name %x<<y%
#'
#' @description This pipe works at the module level.
#' It maps the right module's output port defined by the right number (y) of the pipe operator to the left module's input port defined by the left number (x).
#' 
#' @return The right module
#' 
#' @export %1<<1% %2<<1% %3<<1% %4<<1% %5<<1% %6<<1% %7<<1% %8<<1% %9<<1% %10<<1% %1<<2% %2<<2% %3<<2% %4<<2% %5<<2% %6<<2% %7<<2% %8<<2% %9<<2% %10<<2% %1<<3% %2<<3% %3<<3% %4<<3% %5<<3% %6<<3% %7<<3% %8<<3% %9<<3% %10<<3% %1<<4% %2<<4% %3<<4% %4<<4% %5<<4% %6<<4% %7<<4% %8<<4% %9<<4% %10<<4% %1<<5% %2<<5% %3<<5% %4<<5% %5<<5% %6<<5% %7<<5% %8<<5% %9<<5% %10<<5% %1<<6% %2<<6% %3<<6% %4<<6% %5<<6% %6<<6% %7<<6% %8<<6% %9<<6% %10<<6% %1<<7% %2<<7% %3<<7% %4<<7% %5<<7% %6<<7% %7<<7% %8<<7% %9<<7% %10<<7% %1<<8% %2<<8% %3<<8% %4<<8% %5<<8% %6<<8% %7<<8% %8<<8% %9<<8% %10<<8% %1<<9% %2<<9% %3<<9% %4<<9% %5<<9% %6<<9% %7<<9% %8<<9% %9<<9% %10<<9% %1<<10% %2<<10% %3<<10% %4<<10% %5<<10% %6<<10% %7<<10% %8<<10% %9<<10% %10<<10%
NULL

#' 
#' @title Input port mapping function
#' 
#' @name %>y%
#'
#' @description This pipe maps the left object (must be a reactive function or a reactivevalues object) 
#' to the right module's input port defined by the number in the operator (y).
#' 
#' @return The right module
#' 
#' @export %>1% %>2% %>3% %>4% %>5% %>6% %>7% %>8% %>9% %>10%
NULL

#' 
#' @title Input port mapping function
#' 
#' @name %>>y%
#'
#' @description This pipe maps the left object (must be a reactive function or a reactivevalues object) 
#' to the right module's input port defined by the number in the operator (y).
#' 
#' @return The left object
#' 
#' @export %>>1% %>>2% %>>3% %>>4% %>>5% %>>6% %>>7% %>>8% %>>9% %>>10%
NULL

