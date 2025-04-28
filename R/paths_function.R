
# read this carefully!!!
# https://allancameron.github.io/geomtextpath/


#' @title Multiple Function Geoms
#' 
#' @description
#' Multiple function ***geom***etric objects 
#' (\link[ggplot2]{geom_function}s) 
#' with different parameters.
#' 
#' @param fun a continuous \link[base]{function}, see usage in function \link[ggplot2]{stat_function}
#' 
#' @param dots user-friendly \link[base]{list} of parameters,
#' e.g., `list(mean=0, sd=1:2)` for two normal distributions
#' \eqn{N(0,1)} and \eqn{N(0,2)}
#' 
#' @param args \link[base]{list} of parameters in a programmer-friendly manner,
#' e.g., `list(list(mean=0, sd=1), list(mean=0, sd=2))`
#' for two normal distributions
#' \eqn{N(0,1)} and \eqn{N(0,2)}
#' 
#' @param label \link[base]{character} \link[base]{vector},
#' user-friendly text description of the parameters
#' 
#' @param hjust \link[base]{double} scalar or \link[base]{vector}
#' 
#' @param parse \link[base]{logical} scalar
#' 
#' @param aes_ \link[base]{character} scalar or \link[base]{vector},
#' \link[ggplot2]{aes}thetic mapping(s) to be activated. 
#' Default `'colour'`, i.e., parameters are discriminated using different colors. 
#' Another good choice is `c('colour', 'linetype')`, i.e., parameters are discriminated using different colors and line-types.
#' 
#' @param ... argument of function \link[ggplot2]{stat_function}. 
#' Also see parameter `params` of function \link[ggplot2]{layer}
#' 
#' @returns 
#' Function [paths_function()] returns a \link[base]{list} of 
#' \link[ggplot2]{layer}s.
#' 
#' @examples
#' library(ggplot2)
#' library(geomtextpath)
#' library(latex2exp)
#' ggplot() + 
#'  paths_function(
#'   fun = dnorm, dots = list(mean = 1:0), hjust = .4,
#'   label = TeX(c('$mu_1=1$', '$mu_0=0$')) # bug; prefer unicode
#'  ) + 
#'  xlim(-3, 4)
#' 
#' @keywords internal
#' @importFrom ggplot2 stat_function
#' @importFrom geomtextpath textpathGrob GeomTextpath
#' @importFrom stats setNames
#' @export
paths_function <- function(
    fun, 
    dots = list(),
    args = dots |> .mapply(FUN = list, MoreArgs = NULL),
    label,
    hjust = .5, # seems like the default in \pkg{geomtextpath}; not sure which function!!! 
    parse = is.expression(label),
    aes_ = 'colour', 
    #aes_ = c('colour', 'linetype'),
    ...
) {
  
  if (!is.function(fun)) stop('input must be \'function\'')
  
  if (!length(args)) stop('no `args` provided')
  
  if (missing(label)) {
    # do not want un-exported function [getval_()] show in use-interface
    label <- args |> 
      unname() |>
      vapply(FUN = getval_, FUN.VALUE = '')
  }
  
  if (is.character(label)) {
    # do nothing
  } else if (is.expression(label)) {
    # do nothing
  } else sprintf(fmt = '%s `label` not supported', sQuote(class(label)[1L])) |> stop()
  
  force(parse) # before converting `label` to 'character'
 
  label <- label |> as.character() # must for expression!
  
  mp <- args |> # `args` should be the longest (`label` may be recycled) 
    seq_along() |>
    sprintf(fmt = '%02d') |> # so that '10' is after '09'
    lapply(FUN = \(i) {
      i |> 
        list() |>
        rep(times = length(aes_)) |> 
        setNames(nm = aes_) |>
        do.call(what = 'aes')
    })
  
  lyr <- .mapply(
    FUN = stat_function, 
    dots = list(
      mapping = mp, 
      args = args,
      label = label, # recycled
      hjust = hjust # recycled
    ), 
    MoreArgs = list(
      fun = fun, 
      geom = 'textpath', # must Depends geomtextpath; Imports does not work. why??
      show.legend = FALSE, 
      parse = parse,
      ...
    )
  )
  
  return(c(lyr, list(
    labs(
      y = fun |> substitute() |> deparse1()
    )
  )))
  
}





getval_ <- function(x) {
  
  ret <- sprintf(fmt = '%s = %.3g', names(x), x) |>
    sub(pattern = '([-]?)0[.]', replacement = '\\1.') |> # remove leading zero
    paste0(collapse = '; ')
  
  if (getOption('use_unicode')) {
    ret <- ret |> 
      gsub(pattern = 'Inf', replacement = '\u221e') |>
      gsub(pattern = 'alpha', replacement = '\u03b1') |>
      gsub(pattern = 'lambda', replacement = '\u03bb') |>
      gsub(pattern = 'nu', replacement = '\u03bd') |>
      gsub(pattern = 'xi', replacement = '\u03be') |>
      gsub(pattern = 'omega', replacement = '\u03c9')
  }
  
  return(ret)
  
}




