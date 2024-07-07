
#' @title \link[gridExtra]{arrangeGrob} and Share Legend
#' 
#' @description ..
#' 
#' @param grobs a \link[base]{list} of 
#' \link[grid]{grob}s, \link[gtable]{gtable}s, \link[ggplot2]{ggplot}s or trellis objects
#' 
#' @param position \link[base]{character} scalar, location of the shared legend.
#' Currently there is no way to determine whether the `grobs` *should* share the same legend.
#' 
#' @param top,bottom,left,right \link[base]{character} scalars, \link[base]{expression}s or 
#' \link[grid]{grob}s, see details in \link[gridExtra]{arrangeGrob}
#' 
#' @param ... additional parameters of \link[gridExtra]{arrangeGrob}, 
#' such as `nrow`, `ncol`, `widths`, `heights`
#' 
#' @returns 
#' 
#' Function [shareLegend] returns a `'gtable'` object (returned value of \link[gridExtra]{arrangeGrob})
#' 
#' @seealso
#' 
#' Function `lemon::grid_arrange_shared_legend` also fetches 
#' (via `lemon::g_legend`) 
#' the legend from 1st element of `grobs`.
#' This function does not have arguments `top`, `botom`, `left`, `right`.
#' 
#' Read more about Function `ggpubr::ggarrange`.
#' 
#' Read \url{https://plotly.com/r/subplots/} 
#' on how to stack `'plotly'` objects!
#' 
#' @references 
#' \url{https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs}
#' 
#' @examples 
#' library(ggplot2)
#' (p1 = ggplot(data=iris) + geom_point(aes(x=Sepal.Length, y=Petal.Length, color=Species)))
#' (p2 = ggplot(data=iris) + geom_point(aes(x=Petal.Length, y=Petal.Width, color=Species)))
#' 
#' p = list(p1, p2)
#' (p3 = shareLegend(p, ncol = 2L, top = 'top_text', left = 'left_text'))
#' shareLegend(p, ncol = 1L, top = 'top_text', left = 'left_text', position = 'left')
#' shareLegend(list(p3, p3))
#' tryCatch(shareLegend(list(p3, p3), position = 'left'), error = identity)
#' 
#' ## \CRANpkg{patchwork} is promising, but I dont know how to do what I want, yet
#' # library(patchwork)
#' # (tmp = p1 + p2 + plot_layout(guides = 'collect', nrow = 2, widths = 1)) # great
#' # tmp + labs(title = 'aa') # not what I want!!
#' @importFrom ggplot2 ggplotGrob theme
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid grid.text editGrob
#' @export
shareLegend <- function(
    grobs, 
    position = c('bottom', 'top', 'left', 'right'), 
    top = NULL, bottom = NULL, left = NULL, right = NULL,
    ...
) {
  
  grobs <- grobs[lengths(grobs) > 0L]
  
  if (!(ng <- length(grobs))) stop('input figures cannot be length-0')
  if (ng == 1L) stop('`shareLegend` not applicable for only 1 figure') # return(grobs[[1L]]) # `top`, `left`, `bottom`, `right` not used here
  
  if (!all(vapply(grobs, FUN = inherits, what = c('ggplot', 'gDesc', 'gList'), FUN.VALUE = NA))) stop('input must be a list of gg plots')
  
  if (length(top)) {
    if (is.expression(top)) top <- grid.text(top, draw = FALSE)
  }
  
  if (length(bottom)) {
    if (is.expression(bottom)) bottom <- grid.text(bottom, draw = FALSE)
  }
  
  if (length(left)) {
    if (is.expression(left)) left <- editGrob(grid.text(left, draw = FALSE), rot = 90)
  }
  
  if (length(right)) {
    if (is.expression(right)) right <- editGrob(grid.text(right, draw = FALSE), rot = 90)
  }
  
  position <- match.arg(position)
  
  legds <- lapply(grobs, FUN = function(i) { # i = grobs[[1L]]
    if (inherits(i, what = 'ggplot')) {
      g <- ggplotGrob(i + theme(legend.position = position))$grobs # 'list'
      id <- (vapply(g, FUN = function(i) i$name, FUN.VALUE = '') == 'guide-box')
      if (any(id)) return(g[id][[1L]]) # else NULL
    } else if (inherits(i, what = 'gDesc')) {
      # my return from previous [shareLegend]
      if (position != attr(i, which = 'position')) stop('position does not match')
      attr(i, which = 'legend', exact = TRUE)
    }
  })
  
  has_legds <- (lengths(legds, use.names = FALSE) > 0L)
  if (!any(has_legds)) stop('none of `grobs` has legend; use gridExtra::arrangeGrob')
  
  # remove legends from individual `grobs` before stacking
  grobs0 <- lapply(grobs, FUN = function(i) { # i = grobs[[1L]]
    if (inherits(i, what = 'ggplot')) {
      i + theme(legend.position = 'none')
    } else i # my return from previous [shareLegend]
  })
  
  p0 <- arrangeGrob(grobs = grobs0, top = top, bottom = bottom, left = left, right = right, ...)
  
  legd1 <- legds[has_legds][[1L]]
  # do not know how to check if legends are actually the same!
  
  attr(p0, which = 'legend') <- legd1
  attr(p0, which = 'position') <- position
  class(p0) <- c('shareLegend', class(p0))
  return(p0)
  
}








#' @importFrom grid grid.draw grid.newpage unit unit.c
# @export print.shareLegend
#' @export
print.shareLegend <- function(x, ...) {
  
  legd <- attr(x, which = 'legend', exact = TRUE)
  position <- attr(x, which = 'position', exact = TRUE)
    
  lh <- sum(legd$height)
  lw <- sum(legd$width)
  npc1 <- unit(1, units = 'npc') # 'Normalised Parent Coordinates'
  
  ret <- switch(position, top = {
    arrangeGrob(grobs = list(legd, x), ncol = 1L, heights = unit.c(lh, npc1 - lh))
  }, bottom = {
    arrangeGrob(grobs = list(x, legd), ncol = 1L, heights = unit.c(npc1 - lh, lh))
    # arrangeGrob(grobs = x, bottom = legd) # error
  }, left = {
    arrangeGrob(grobs = list(legd, x), ncol = 2L, widths = unit.c(lw, npc1 - lw))
  }, right = {
    arrangeGrob(grobs = list(x, legd), ncol = 2L, widths = unit.c(npc1 - lw, lw))
  })
  
  grid.newpage()
  grid.draw(ret)
  
  return(invisible(ret))
  
} 










