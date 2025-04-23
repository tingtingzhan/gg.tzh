
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
#' @param ... argument of function \link[ggplot2]{geom_function}. 
#' Also see parameter `params` of function \link[ggplot2]{layer}
#' 
#' @returns 
#' Function [geoms_function()] returns a \link[base]{list} of 
#' \link[ggplot2]{geom_function}
#' \link[ggplot2]{layer}s.
#' 
#' @examples 
#' library(latex2exp)
#' 
#' ggplot() + geom_function(fun = dnorm, args = list(mean=0, sd=1)) + xlim(-3,3)
#' 
#' ggplot() + geoms_function(dnorm, dots = list(mean=1:0), 
#'   label = TeX(c('$mu_1=1$', '$mu_0=0$'))) + xlim(-3,4)
#' 
#' ggplot() + geoms_function(dnorm, dots = list(mean=0, sd=c(1,1.3)), n=501L, size=2) + xlim(-5,5)
#' ggplot() + geoms_function(dnorm, dots=list(mean=0:1, sd=c(1,.9)), 
#'   label = letters[1:2], aes_ = c('color', 'linetype')) + xlim(-5,5)
#' ggplot() + geoms_function(dt, dots = list(df=c(1,2,5,Inf)), size = 2.5) + xlim(-4,4)
#' ggplot() + geoms_function(dchisq, dots = list(df = c(1,3,5,10)), size = 2) + xlim(.2,8)
#' ggplot() + geoms_function(dgamma, dots = list(shape = 1:4), size = 2) + xlim(0,5)
#' 
#' # developer's use
#' ggplot() + geoms_function(dnorm, args = list(
#'  list(mean=0, sd=1), 
#'  list(mean=1, sd=1.3)
#' ), label = c('beta', 'alpha'), parse = TRUE) + xlim(-5,5)
#' 
#' # manually, without function [geoms_function()]
#' library(geomtextpath)
#' ggplot() + 
#'  stat_function(geom = 'textpath', fun = dnorm, args = list(mean=0, sd=1), 
#'   mapping = aes(color = '1'), label = 'beta', 
#'   parse = TRUE, show.legend = FALSE) +
#'  stat_function(geom = 'textpath', fun = dnorm, args = list(mean=1, sd=1.3), 
#'   mapping = aes(color = '2'), label = 'alpha', 
#'   parse = TRUE, show.legend = FALSE) +
#'  xlim(-5,5)
#'
#' @importFrom ggplot2 geom_function stat_function labs scale_colour_discrete scale_linetype_discrete
#' @importFrom geomtextpath geom_textpath
#' @importFrom stats setNames
#' @export
geoms_function <- function(
    fun, 
    dots = list(),
    args = split_list_(dots),
    label = getval_(args),
    parse = is.expression(label),
    aes_ = 'colour', 
    #aes_ = c('colour', 'linetype'),
    ...
) {
  
  if (!is.function(fun)) stop('input must be \'function\'')
  
  if (missing(args)) {
    if (!length(dots)) {
      cl <- match.call()
      cl[[1L]] <- quote(geom_function)
      .Defunct(new = deparse1(cl))
    }
    force(args) 
  }
  
  if (is.character(label)) {
    # do nothing
    # .. allow (anyDuplicated(label))
  } else if (is.expression(label)) {
    # do nothing
  } else sprintf(fmt = '%s `label` not supported', sQuote(class(label)[1L])) |> stop()
  
  amap <- args |>
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
  .mapply(FUN = stat_function, dots = list(
      mapping = amap, 
      args = args,
      label = label |> as.character() # must for expression!
    ), MoreArgs = list(
      fun = fun, 
      geom = 'textpath', # must Depends geomtextpath; Imports does not work. why??
      show.legend = FALSE, 
      parse = parse,
      ...
    )
  )
  
}







#' @title Re-organize a \link[base]{list} of Arguments
#' 
#' @description ..
#' 
#' @param x \link[base]{list} of \link[base]{vector}s
#' 
#' @returns 
#' Function [split_list_()] returns a \link[base]{list}.
#' 
#' @examples 
#' list(mean = 1:2) |> split_list_() |> getval_()
#' list(mean = c(exp(1), pi), sd = 1:2) |> split_list_() |> getval_()
#' list(df1 = 1, df2 = c(2, 3), ncp = c(.1, .2)) |> split_list_() |> getval_()
#' @keywords internal
#' @export
split_list_ <- function(x) {
  
  if (!length(nm <- names(x)) || !all(nzchar(nm))) stop('all elements must be named')
  if (!identical(make.names(nm), nm)) stop('some element names are illegal')
  
  d <- x |>
    as.data.frame.list() 
  # .. recycle length
  # .. (no longer used) the *1st* named element of `x` will provide row.names of `d`
  
  # essentially base::split.data.frame by individual rows
  d |>
    .row_names_info(type = 2L) |> 
    seq_len() |>
    lapply(FUN = \(i) {
      d[i, , drop = FALSE] |> 
        as.list.data.frame()
    })
  
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
#' Function [getval_()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @export
getval_ <- function(x, use_unicode = getOption('use_unicode')) {
  
  # `x` is return of [split_list_()]
  x <- lapply(x, FUN = as.list) # already ?base::is.list, does not matter
  
  pnms <- lapply(x, FUN = names)
  if (!all(duplicated.default(pnms)[-1L])) stop('parameter names must be all-equal')
  
  pnm <- pnms[[1L]]
  if (use_unicode) {
    pnm <- pnm |> 
      gsub(pattern = 'alpha', replacement = '\u03b1') |>
      gsub(pattern = 'lambda', replacement = '\u03bb') |>
      gsub(pattern = 'nu', replacement = '\u03bd') |>
      gsub(pattern = 'xi', replacement = '\u03be') |>
      gsub(pattern = 'omega', replacement = '\u03c9')
  }

  x |>
    vapply(FUN = \(i) {
      sprintf(fmt = '%s = %.3g', pnm, i) |>
        #paste0(collapse = '\n')
        paste0(collapse = '; ')
    }, FUN.VALUE = NA_character_)
  
}




