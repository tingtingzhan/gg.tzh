
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
#' @param dots \link[base]{list} of parameters in a user-friendly manner,
#' e.g., `list(mean=0, sd=1:2)` for two normal distributions
#' \eqn{N(0,1)} and \eqn{N(0,2)}
#' 
#' 
#' @param args \link[base]{list} of parameters in a programmer-friendly manner,
#' e.g., `list(a=list(mean=0, sd=1), b=list(mean=0, sd=2))`
#' for two normal distributions
#' \eqn{N(0,1)} and \eqn{N(0,2)} labeled as `'a'` and `'b'`, respectively
#' 
#' @param label \link[base]{character} \link[base]{vector},
#' user-friendly text description of the parameters
#' 
#' @param parse ..
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
#' library(latex2exp)
#' 
#' ggplot() + stat_function(fun = dnorm, args = list(mean=0, sd=1)) + xlim(-3,3)
#' 
#' ggplot() + paths_function(dnorm, dots = list(mean=1:0), 
#'   label = TeX(c('$mu_1=1$', '$mu_0=0$'))) + xlim(-3,4)
#' 
#' ggplot() + paths_function(
#'   fun = dnorm, dots = list(mean = 0, sd = c(1, 1.3)), 
#'   aes_ = c('color', 'linetype'), size = 2
#' ) + xlim(-5, 5)
#' 
#' ggplot() + paths_function(dt, dots = list(df=c(1,2,5,Inf)), size = 2.5) + xlim(-4,4)
#' ggplot() + paths_function(dchisq, dots = list(df = c(1,3,5,10)), size = 2) + xlim(.2,8)
#' ggplot() + paths_function(dgamma, dots = list(shape = 1:4), size = 2) + xlim(0,5)
#' 
#' # developer's use
#' ggplot() + paths_function(dnorm, args = list(
#'  list(mean=0, sd=1), 
#'  list(mean=1, sd=1.3)
#' ), label = c('beta', 'alpha'), parse = TRUE) + xlim(-5,5)
#' 
#' # manually, without function [paths_function()]
#' ggplot() + 
#'  stat_function(geom = 'textpath', fun = dnorm, args = list(mean=0, sd=1), 
#'   mapping = aes(color = '1'), label = 'a', show.legend = FALSE) +
#'  stat_function(geom = 'textpath', fun = dnorm, args = list(mean=1, sd=1.3), 
#'   mapping = aes(color = '2'), label = 'b', show.legend = FALSE) +
#'  xlim(-5,5)
#'
#' @importFrom ggplot2 stat_function
#' @importFrom geomtextpath GeomTextpath
#' @importFrom stats setNames
#' @export
paths_function <- function(
    fun, 
    dots = list(),
    args = dots |> .mapply(FUN = list, MoreArgs = NULL),
    #label = getval_OLD(args),
    label = args |> vapply(FUN = getval_, FUN.VALUE = ''),
    parse = is.expression(label),
    aes_ = 'colour', 
    #aes_ = c('colour', 'linetype'),
    ...
) {
  
  if (!is.function(fun)) stop('input must be \'function\'')
  
  if (missing(args)) {
    if (!length(dots)) .Defunct(new = 'use ggplot2::stat_function')
    force(args) 
  }
  
  if (is.character(label)) {
    # do nothing
    # .. allow (anyDuplicated(label))
  } else if (is.expression(label)) {
    # do nothing
  } else sprintf(fmt = '%s `label` not supported', sQuote(class(label)[1L])) |> stop()
  
  mp <- args |>
    seq_along() |>
    lapply(FUN = \(i) {
      i |> 
        rep(times = length(aes_)) |> 
        as.character() |> 
        as.list() |>
        setNames(nm = aes_) |>
        do.call(what = 'aes')
    })
  
  # return below
  .mapply(
    FUN = stat_function, 
    dots = list(
      mapping = mp, 
      args = args,
      label = label |> as.character() # must for expression!
    ), 
    MoreArgs = list(
      fun = fun, 
      geom = 'textpath', # must Depends geomtextpath; Imports does not work. why??
      show.legend = FALSE, 
      parse = parse,
      ...
    )
  )
  
}








#' @title Values of Argument List
#' 
#' @description
#' ..
#' 
#' @param x ..
#' 
#' @param use_unicode \link[base]{logical} scalar
#' 
#' @returns 
#' Function [getval_()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @export
getval_ <- function(x, use_unicode = getOption('use_unicode')) {
  
  x <- x |> as.list() # already ?base::is.list, does not matter
  
  if (use_unicode) {
    names(x) <- x |> 
      names() |> 
      gsub(pattern = 'alpha', replacement = '\u03b1') |>
      gsub(pattern = 'lambda', replacement = '\u03bb') |>
      gsub(pattern = 'nu', replacement = '\u03bd') |>
      gsub(pattern = 'xi', replacement = '\u03be') |>
      gsub(pattern = 'omega', replacement = '\u03c9')
  }
  
  sprintf(fmt = '%s = %.3g', names(x), x) |>
    paste0(collapse = '; ')
  
}




