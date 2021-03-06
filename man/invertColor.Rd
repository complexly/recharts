% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{invertColor}
\alias{invertColor}
\title{Invert A Color to Its Conplementary Color}
\usage{
invertColor(color, mode = c("bw", "opposite", "hue", "saturation",
  "lumination", ""), ...)
}
\arguments{
\item{color}{A hex or named color, or color in 'rgba(R, G, B, A)' string.}

\item{mode}{One or a vector of modes combined. You can only input the first letter.
Default 'bw', which is most useful in textStyles.
\describe{
 \item{\code{bw}}{black and white invertion}
 \item{\code{opposite}}{complete invertion to get an opposite color}
 \item{\code{hue}}{only invert hue in terms of \code{\link{hsv}}}
 \item{\code{saturation}}{only invert saturation in terms of \code{\link{hsv}}}
 \item{\code{lumination}}{only invert lumination in terms of \code{\link{hsv}}}
}}

\item{...}{Elipsis}
}
\value{
Inverted hex color
}
\description{
Invert A Color to Its Conplementary Color
}
\examples{
col <- sapply(list('o', 'h', 'l', 's', 'b', c('h', 'l'), c('h', 's'),
              c('l', 's'), c('h', 's', 'l')), function(mode) {
              return(invertColor('darkred', mode))
        })
library(scales)
show_col(c('darkred', unlist(col)))

}
\seealso{
\code{\link{hsv}}, \code{\link{rgb2hsv}}, \code{\link{rgb}},
}

