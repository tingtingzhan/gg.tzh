

#' @title Bars of Discrete Function using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param fun a discrete \link[base]{function}
#' 
#' @param labels ..
#' 
#' @param ... argument of \link[base]{function} `fun` (not of \link[ggplot2]{ggplot})
#' 
#' @param xlim \link[base]{length} \eqn{\geq 2} \link[base]{integer} \link[base]{vector}, limit on \eqn{x}-axis.
#' 
#' @details ..
#' 
#' @returns
#' Function [geom_bars()] returns a \link[base]{list} of 
#' \link[ggplot2]{geom_bar}
#' \link[ggplot2]{layer}s.
#' 
#' @note 
#' Be aware of potential name clash with function `ggpubr::ggbarplot`.
#' 
#' @examples 
#' options(use_unicode = FALSE)
#' theme_set(theme(
#'  legend.position = 'inside',
#'  legend.position.inside = c(.8, .8),
#'  legend.key.spacing.y = unit(.01, units = 'npc'),
#'  legend.background = element_rect(color = 'grey95')
#' ))
#' 
#' ggplot() + geom_bars(dpois, lambda = 1, xlim = 0:6)
#' 
#' ggplot() + geom_bars(dnbinom, size = 5.2, prob = c(.5, .4), xlim = 0:15)
#' 
#' ggplot() + geom_bars(dpois, lambda = c(1, 1.3), labels = c('Treatment', 'Control'), 
#'  xlim = 0:4) +
#'  labs(x = 'Number of Re-Admissions', y = 'Conditional Probabilities', 
#'   fill = 'Poisson\nDistribution',
#'   caption = 'Illustration showing mean-ratio = .77 (Treatment vs. Control)')
#'  
#' # how to make this beautifully?
#' ggplot() + 
#'  geom_bars(dbinom, size = c(5L, 10L, 20L, 40L), prob = c(.8, .4, .2, .1), xlim = c(0L, 10L)) +
#'  geom_bars(dpois, lambda = 4, xlim = c(0L, 10L))
#' 
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_continuous scale_y_continuous labs
#' @export
geom_bars <- function(
    fun, ...,
    labels = getval_(arg),
    xlim = stop('must provide `xlim` explicitly')
) {
  
  if (!is.integer(xlim) || anyNA(xlim) || (length(xlim) < 2L)) stop('`xlim` must be len-2 integer')
  if (any(xlim < 0L)) stop('`xlim` must be non-negative')
  
  xs <- min(xlim):max(xlim)
  arg <- list(...) |> split_list_()
  narg <- length(arg)
  ys <- lapply(arg, FUN = \(i) do.call(fun, args = c(list(xs), i)))
  
  return(list(
    
    geom_bar(mapping = aes(
      x = rep(xs, times = narg), 
      y = ys |> unlist(use.names = FALSE), 
      fill = rep(labels, times = lengths(ys))
    ), stat = 'identity', colour = 'white', position = 'dodge'),
    
    scale_x_continuous(breaks = xs), # all (integer) `x` values printed
      
    scale_y_continuous(labels = function(x) sprintf(fmt = '%.0f%%', 1e2*x)),
    
    labs(x = NULL, y = NULL, fill = deparse1(substitute(fun)))
    
  ))
  
}


