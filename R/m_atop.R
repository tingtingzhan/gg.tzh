

#' @title Extension of `demo(plotmath)` of Package \pkg{grDevices}
#'
#' @description .. 
#' 
#' @param x ..
#' 
#' @param size \link[base]{character} scalar, font size, currently supported are 
#' `'displaystyle'`, `'textstyle'` (default), `'scriptstyle'` and `'scriptscriptstyle'`
#' 
#' @param right,accumulate see \link[base]{Reduce}
#' 
#' @returns 
#' 
#' Function [m_atop] returns a \link[base]{call} to be interpreted as a mathematical expression by 
#' text-drawing functions (e.g., \link[graphics]{title})
#' 
#' @references 
#' \url{https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html}.
#' 
#' Inspiration for function [m_atop]: \url{https://stackoverflow.com/questions/21067600/multiple-line-breaks-in-expression}
#' 
#' @seealso 
#' `demo(plotmath)`
#' 
#' The expressions are processed by \link[graphics]{title}.
#' 
#' 
#' @examples 
#' txt = c(
#' 'Lower: $log(-\\hat{g}(\\hat{A} - x_p)/(exp(\\hat{g}z_p) - 1))=log(B) + h\\cdot z_p^2/2$',
#' 'Upper: $log(\\hat{g}(x_{1-p} - \\hat{A})/(exp(-\\hat{g}z_p) - 1))=log(B) + h\\cdot z_p^2/2$',
#' 'Combo: $log(\\hat{g}(x_{1-p} - x_p)/(exp(-\\hat{g}z_p)-exp(\\hat{g}z)))=log(B) + h\\cdot z_p^2/2$'
#' )
#' 
#' library(latex2exp)
#' (mm <- m_atop(TeX(txt), size = 'scriptstyle'))
#' stopifnot(identical(mm, m_atop(TeX(txt, output = 'character'), size = 'scriptstyle')))
#' plot(1, main = mm)
#' 
#' @name m_atop
#' @export
m_atop <- function(x, size, right, accumulate) UseMethod('m_atop')

# `atop` only takes 2 arguments
# even with `textstyle`, the line space will still retain the hierarchy.


#' @rdname m_atop
#' @export m_atop.expression
#' @export
m_atop.expression <- function(x, size = c('textstyle', 'displaystyle', 'scriptstyle', 'scriptscriptstyle'), right = FALSE, accumulate = FALSE) {
  if (!(nx <- length(x))) stop('input expression cannot be len-0')
  names(x) <- NULL # names will not be used
  if (nx == 1L) return(x) # `size` not activated
  size <- match.arg(size)
  Reduce(f = function(e1, e2) as.call(list(quote(atop), e1, e2)), 
         x = lapply(x, FUN = \(ix) call(name = size, ix)),
         right = right, accumulate = accumulate)
}


#' @rdname m_atop
#' @export m_atop.character
#' @export
m_atop.character <- function(x, size = c('textstyle', 'displaystyle', 'scriptstyle', 'scriptscriptstyle'), right = FALSE, accumulate = FALSE) {
  if (!(nx <- length(x))) stop('input character cannot be len-0')
  names(x) <- NULL # names will not be used
  if (nx == 1L) return(str2lang(x)) # `size` not activated
  size <- match.arg(size)
  Reduce(f = function(e1, e2) as.call(list(quote(atop), e1, e2)), 
         x = lapply(paste0(size, '(', x, ')'), FUN = str2lang), 
         right = right, accumulate = accumulate)
}

