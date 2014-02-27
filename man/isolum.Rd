\name{isolum}
\alias{isolum}
\title{Create a colorblind-safe vector of \code{n} isoluminent colors.}
\usage{
isolum(n = 256, mincolor = NULL, maxcolor = NULL, invert = FALSE,
  gamma = 1.8, alpha = 1)
}
\arguments{
  \item{n}{the number of colors to be in the palette.}

  \item{mincolor}{a color with which to replace the lower
  end of the scale.}

  \item{maxcolor}{a color with which to replace the upper
  end of the scale.}

  \item{invert}{logical indicating whether the palette
  should be inverted.}

  \item{gamma}{the exponent to use for each channel when
  converting to greyscale, such that \code{grey =
  (red^gamma + green^gamma + blue^gamma) ^ (1/gamma)}.}

  \item{alpha}{the alpha transparency for the palette.}
}
\value{
A character vector of color names. This can be used either
to create a user-defined color palette for subsequent
graphics by \code{\link{palette}(cv)}, a \code{col =}
specification in graphics functions or in \code{par}.
}
\description{
Create a colorblind-safe vector of \code{n} isoluminent
colors.
}
\details{
The colormap is isoluminent and perceived by those with a
red-green color perception deficiency as a linear ramp
between blue and yellow.  However, the colormap has been
enriched with a red control point for those with normal
color vision, with the shade carefully chosen to avoid
creating a non-linear ramp for those with red-green color
perception deficiency. As the color map is isoluminent, it
will appear as one shade of grey across the entire range
when printed on a black & white printer.
}
\examples{
require(graphics)
# A color wheel
pie(rep(1,12), col=isolum(12))
}
\seealso{
\code{\link{palettes}} and \code{\link{color}.}
}

