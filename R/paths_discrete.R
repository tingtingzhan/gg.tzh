

#' @title Bars of Discrete Function using \CRANpkg{ggplot2}
#' 
#' @description ..
#' 
#' @param fun a discrete \link[base]{function}
#' 
#' @param dots argument of \link[base]{function} `fun`
#' 
#' @param args ..
#' 
#' @param label ..
#' 
#' @param ... parameters of \link[geomtextpath]{geom_textpath}
#' 
#' @param xlim \link[base]{length} \eqn{\geq 2} \link[base]{integer} \link[base]{vector}, limit on \eqn{x}-axis.
#' 
#' @details ..
#' 
#' @returns
#' Function [paths_discrete()] returns a \link[base]{list} of 
#' \link[ggplot2]{layer}s.
#' 
#' @note 
#' Be aware of potential name clash with function `ggpubr::ggbarplot`.
#' 
#' @examples 
#' library(ggplot2)
#' ggplot() + paths_discrete(
#'  fun = dpois, dots = list(lambda = c(1, 1.3)), 
#'  label = c('Treatment', 'Control'), xlim = 0:4
#' ) + labs(
#'  x = 'Number of Re-Admissions', y = 'Conditional Probabilities', 
#'  caption = 'Illustration showing mean-ratio = .77 (Treatment vs. Control)')
#'  
#' @keywords internal
#' @importFrom ggplot2 ggplot aes geom_point scale_x_continuous scale_y_continuous labs
#' @importFrom geomtextpath geom_textpath
#' @importFrom rlang .data
#' @importFrom scales label_percent
#' @export
paths_discrete <- function(
    fun, 
    dots = list(),
    args = dots |> .mapply(FUN = list, MoreArgs = NULL),
    label,
    xlim = stop('must provide `xlim` explicitly'),
    ...
) {
  
  if (!is.integer(xlim) || !length(xlim) || anyNA(xlim)) stop('`xlim` must be integer')
  if (any(xlim < 0L)) stop('`xlim` must be non-negative')
  
  if (missing(label)) {
    # do not want un-exported function [getval_()] show in use-interface
    label <- args |> 
      unname() |>
      vapply(FUN = getval_, FUN.VALUE = '')
  }
  
  xs <- if (length(xlim) == 1L) {
    if (xlim == 0L) stop('upper end of `xlim` needs to be positive')
    0:xlim
  } else min(xlim):max(xlim)
  
  ys <- args |>
    lapply(FUN = \(i) do.call(what = fun, args = c(list(xs), i)))
  
  aes_d_ <- data.frame(
    x = xs,
    y = ys |> unlist(use.names = FALSE),
    id = label |> seq_along() |> as.character() |> rep(each = length(xs)),
    label = label |> rep(each = length(xs))
  )
  
  mp <- if (length(args) > 1L) {
    aes(x = .data$x, y = .data$y, color = .data$id, label = .data$label)
  } else aes(x = .data$x, y = .data$y, label = .data$label)
    
  return(list(
    
    # (optional)
    # geom_point(data = aes_d_, mapping = aes(x = .data$x, y = .data$y, color = .data$label), alpha = .5, ...),
    
    geom_textpath(data = aes_d_, mapping = mp, show.legend = FALSE, ...),
    
    scale_x_continuous(breaks = xs), # all (integer) `x` values printed
      
    scale_y_continuous(labels = label_percent()),
    
    labs(x = NULL, y = deparse1(substitute(fun)))
    # write individual ?ggplot2::labs, instead of `name` in scale_*_continuous
    # otherwise later user cannot use ?ggplot2::labs to overwrite (do not understand why..)
    
  ))
  
}


