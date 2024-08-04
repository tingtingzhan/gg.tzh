
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
#' @param labels \link[base]{character} \link[base]{vector},
#' user-friendly text description of the parameters
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
#' Function [geoms_function] returns a \link[base]{list} of 
#' \link[ggplot2]{geom_function}
#' \link[ggplot2]{layer}s.
#' 
#' @examples 
#' library(ggplot2)
#' theme_set(theme(
#'  legend.position = 'inside',
#'  legend.position.inside = c(.15, .6),
#'  legend.key.spacing.y = grid::unit(.01, units = 'npc'),
#'  legend.background = element_rect(color = 'grey95')
#' ))
#' 
#' ggplot() + geom_function(fun = dnorm, args = list(mean=0, sd=1)) + xlim(-3,3)
#' ggplot() + geoms_function(dnorm, dots = list(mean=1:0), 
#'   labels = latex2exp::TeX(c('$mu_1=1$', '$mu_0=0$')), aes_ = c('colour','linetype')) + xlim(-3,4)
#' ggplot() + geoms_function(dnorm, dots = list(mean=0, sd=c(1,1.3)), n=501L) + xlim(-5,5)
#' ggplot() + geoms_function(dnorm, dots=list(mean=0:1, sd=c(1,.9)), 
#'   labels = letters[1:2], aes_ = 'linetype') + xlim(-5,5)
#' ggplot() + geoms_function(dt, dots = list(df=c(1,2,5,Inf))) + 
#'   xlim(-4,4)
#' ggplot() + geoms_function(dchisq, dots = list(df=c(1,3,5,10))) + 
#'   xlim(0,5) + ylim(0,1) + theme(legend.position.inside = c(.6,.6)) 
#' ggplot() + geoms_function(dgamma, dots = list(shape=1:4)) + 
#'   xlim(0,5) + theme(legend.position.inside = c(.8,.6))
#' 
#' # developer's use
#' ggplot() + geoms_function(dnorm, args = list(
#'  list(mean=0, sd=1), 
#'  list(mean=1, sd=1.3)
#' ), labels = c('beta', 'alpha')) + xlim(-5,5)
#' 
#' # manually, without function [geoms_function]
#' ggplot() + 
#'  geom_function(fun = dnorm, args = list(mean=0, sd=1), 
#'   mapping = aes(color = 'beta')) +
#'  geom_function(fun = dnorm, args = list(mean=1, sd=1.3), 
#'   mapping = aes(color = 'alpha')) +
#'  xlim(-5,5)
#' # but the order of labels are wrong 
#' # .. i.e., automatically alphabetical, not what we want
#' @importFrom ggplot2 geom_function labs scale_colour_discrete scale_linetype_discrete
#' @export
geoms_function <- function(
    fun, 
    dots = list(),
    args = split_list_(dots),
    labels = getval_(args),
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
  
  # @importFrom latex2exp TeX
  #labels <- unname(TeX(labels))
  # very tedius to have linebreak in base::expression
  
  nag <- length(args)
  brk <- seq_len(nag)
  if (length(labels) != nag) stop('`labels` and `args` must be of same length')
  # okay for `labels` being 'character' or 'expression'! 
  
  if (is.character(labels)) {
    if (anyDuplicated(labels)) stop('`labels` must be unique')
    attr(brk, which = 'levels') <- labels
  } else if (is.expression(labels)) {
    labels <- unname(labels) # must! otherwise ?ggplot2::scale_colour_discrete will ignore!!
    attr(brk, which = 'levels') <- as.character(brk)
    #attr(brk, which = 'levels') <- labels # error for 'expression' `labels`
    expr_labels_ <- lapply(paste0('scale_', aes_, '_discrete'), FUN = function(i) {
      do.call(what = i, args = list(labels = labels))
    })
  } else stop('`labels` must either be character or expression')
  
  class(brk) <- 'factor' # 'factor' is malformed without levels!!
  
  n_aes <- length(aes_)
  
  amap <- lapply(brk, FUN = function(i) { # (i = brk[1L])
    ag <- rep(list(i), times = n_aes)
    names(ag) <- aes_
    do.call(what = 'aes', args = ag)
  })
  
  lab_aes <- rep(list(deparse1(substitute(fun))), times = n_aes)
  names(lab_aes) <- aes_
  
  return(c(
    
    .mapply(FUN = geom_function, dots = list(
      mapping = amap, 
      args = args
    ), MoreArgs = list(fun = fun, ...)), 
    
    if (is.expression(labels)) expr_labels_,
    
    list(
      do.call(what = 'labs', c(lab_aes, list(y = NULL)))
    )
    
  ))
  
}







#' @title Re-organize a \link[base]{list} of Arguments
#' 
#' @description ..
#' 
#' @param x \link[base]{list} of \link[base]{vector}s
#' 
#' @returns 
#' Function [split_list_] returns a \link[base]{list}.
#' 
#' @examples 
#' getval_(split_list_(list(mean = 1:2)))
#' getval_(split_list_(list(mean = c(exp(1), pi), sd = 1:2)))
#' getval_(split_list_(list(df1 = 1, df2 = c(2, 3), ncp = c(.1, .2))))
#' 
#' @export
split_list_ <- function(x) {
  if (!length(nm <- names(x)) || !all(nzchar(nm))) stop('all elements must be named')
  if (!identical(make.names(nm), nm)) stop('some element names are illegal')
  
  d <- as.data.frame.list(x) 
  # .. recycle length
  # .. (no longer used) the *1st* named element of `x` will provide row.names of `d`
  
  # essentially base::split.data.frame by individual rows
  lapply(seq_len(.row_names_info(d, type = 2L)), FUN = function(i) as.list.data.frame(d[i, , drop = FALSE]))
  
}


#' @title Values of Argument List
#' 
#' @description
#' ..
#' 
#' @param x ..
#' 
#' @returns 
#' Function [getval_] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @export
getval_ <- function(x) {
  # `x` is return of [split_list_]
  x <- lapply(x, FUN = as.list) # already ?base::is.list, does not matter
  
  pnms <- lapply(x, FUN = names)
  if (!all(duplicated.default(pnms)[-1L])) stop('parameter names must be all-equal')
  pnm <- pnms[[1L]]
  pnm <- gsub(pattern = 'alpha', replacement = '\u03b1', x = pnm)
  pnm <- gsub(pattern = 'lambda', replacement = '\u03bb', x = pnm)
  pnm <- gsub(pattern = 'nu', replacement = '\u03bd', x = pnm)
  pnm <- gsub(pattern = 'xi', replacement = '\u03be', x = pnm)
  pnm <- gsub(pattern = 'omega', replacement = '\u03c9', x = pnm)

  vapply(unname(x), FUN = function(i) {
    paste0(sprintf(fmt = '%s = %.3g', pnm, i), collapse = '\n')
  }, FUN.VALUE = NA_character_)

}




