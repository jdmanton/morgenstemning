\name{ametrine}
\alias{ametrine}
\title{Create a colorblind-safe vector of \code{n} quasi-isoluminent colors.}
\usage{
ametrine(n = 256, mincolor = NULL, maxcolor = NULL, invert = FALSE,
  alpha = 1)
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
graphics by \code{\link[grDevices]{palette}(cv)}, a
\code{col =} specification in graphics functions or in
\code{par}.
}
\description{
Create a colorblind-safe vector of \code{n}
quasi-isoluminent colors.
}
\details{
The colormap is almost isoluminent and perceived by those
with a red-green color perception deficiency as a roughly
linear ramp between blue and yellow. However, the colormap
has been enriched with a red control point for those with
normal color vision. In order to improve contrast, this
colormap is slightly unbalanced in luminence, unlike
\code{\link{isolum}}.
}
\examples{
require(graphics)
# A color wheel
pie(rep(1,12), col=ametrine(12))
}
\seealso{
\code{\link[grDevices]{palettes}} and
\code{\link[grDevices]{colors}.}
}

