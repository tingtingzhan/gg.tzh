
#' @title \link[ggplot2]{ggplot} \link[ggplot2]{labeller} to support \link[latex2exp]{TeX}
#' 
#' @description 
#' A new member of the \link[ggplot2]{ggplot} \link[ggplot2]{labellers} 
#' to support \link[latex2exp]{TeX}.
#' 
#' @param labels \link[base]{data.frame} of labels, 
#' see details in the documentation \link[ggplot2]{labellers}
#' 
#' @param ... additional parameters of function \link[ggplot2]{label_parsed}
#' 
#' @details 
#' ..
#' 
#' 
#' @returns
#' 
#' Function [label_TeX()] returns the same object returned by \link[ggplot2]{label_parsed}.
#' 
#' @examples
#' tex_lab = c('$\\hat{alpha}$', '$\\bar{beta}$')
#' d = data.frame(
#'   x = c(rnorm(20L, sd = 1), rnorm(20L, mean = 1, sd = 1.5)),
#'   y = c(rnorm(20L, sd = 1), rnorm(20L, mean = 1, sd = 1.5)),
#'   grp0 = structure(rep(1:2, each = 20L), levels = letters[1:2], class = 'factor'),
#'   grp = structure(rep(1:2, each = 20L), levels = tex_lab, class = 'factor')
#' )
#' (a0 = ggplot() + geom_point(data = d, mapping = aes(x = x, y = y, colour = grp0)))
#' (a = ggplot() + geom_point(data = d, mapping = aes(x = x, y = y, colour = grp)))
#' a + facet_grid(~ grp)
#' a + facet_grid(~ grp, labeller = label_TeX) + 
#'   scale_color_discrete(name = 'test', labels = latex2exp::TeX(tex_lab))
#'   
#' # all fine for non-TeX input
#' a0 + facet_grid(~ grp0, labeller = label_TeX) 
#' @importFrom ggplot2 label_parsed
#' @importFrom latex2exp TeX
#' @export
label_TeX <- function(labels, ...) {
  if (!is.data.frame(labels)) stop('ggplot2 package updated?')
  lab0 <- lapply(as.list.data.frame(labels), FUN = as.character)
  lab1 <- lapply(lab0, FUN = \(i) as.character(TeX(i)))
  if (identical(lab0, lab1)) return(lab1) # i.e., no TeX symbol
  label_parsed(lab1, ...)
}

