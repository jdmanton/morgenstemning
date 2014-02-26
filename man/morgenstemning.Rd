\docType{package}
\name{morgenstemning}
\alias{morgenstemning}
\alias{morgenstemning-package}
\title{morgenstemning}
\usage{
morgenstemning(n = 256, mincolor = NULL, maxcolor = NULL,
  invert = FALSE, gamma = 1.8, alpha = 1)
}
\arguments{
  \item{n}{the number of colors to be in the palette.}

  \item{mincolor}{a color with which to replace the lower
  end of the scale.}

  \item{maxcolor}{a color with which to replace the upper
  end of the scale.}

  \item{invert}{logical indicating whether the palette
  should be inverted.}

  \item{alpha}{the alpha transparency for the palette.}
}
\value{
A character vector of color names. This can be used either
to create a user-defined color palette for subsequent
graphics by \code{\link{palette}(cv)}, a \code{col =}
specification in graphics functions or in \code{par}.
}
\description{
morgenstemning

Create a colorblind-safe vector of \code{n} contiguous
colors.
}
\details{
The colormap increases linearly in lightness (such as a
pure black to white map) but incorporates additional colors
that help to emphasise the transitions and hence enhance
the perception of the data. It is designed to be
printer-friendly both for color printers and black & white
printers.
}
\examples{
require(graphics)
# A color wheel
pie(rep(1,12), col=morgenstemning(12))
}
\seealso{
\code{\link{palettes}} and \code{\link{color}.}
}

