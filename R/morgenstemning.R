#' Create a colourblind-safe vector of \code{n} contiguous colours.
#' 
#' @details The colourmap increases linearly in lightness (such as a pure black
#'   to white map) but incorporates additional colors that help to emphasise the
#'   transitions and hence enhance the perception of the data. It is designed to
#'   be printer-friendly both for colour printers and black & white printers.
#' @param n the number of colours to be in the palette.
#' @param minColour a colour with which to replace the lower end of the scale.
#' @param maxColour a colour with which to replace the upper end of the scale.
#' @param invert logical indicating whether the palette should be inverted.
#' @param alpha the alpha transparency for the palette.
#' @return A character vector of color names. This can be used either to
#'   create a user-defined color palette for subsequent graphics by
#'   \code{\link{palette}(cv)}, a \code{col =} specification in graphics
#'   functions or in \code{par}.
#' @export
#' @seealso \code{\link{palettes}} and \code{\link{color}.}
#' @examples
#' require(graphics)
#' # A colour wheel
#' pie(rep(1,12), col=morgenstemning(12))
morgenstemning <- function(n=256, minColour=NULL, maxColour=NULL, invert=FALSE, gamma=1.8, alpha=1) {
  controlPoints <- matrix(c(
    0, 0, 0,
    25, 53, 95,  	# cyan
    192, 27, 111,	# reddish-magenta
    252, 229, 0, 	# yellow
    255, 255, 255),
    ncol=3, byrow=T)
  controlPoints <- controlPoints / 255
  
  allElemsDone <- FALSE
  n.prev <- dim(controlPoints)[1]
  n.curr <- n.prev * 2 - 1
  cmap.prev <- controlPoints
  
  while(!allElemsDone) {
    cmap <- apply(cmap.prev, 2, .pchip, xi=1:n.prev, x=seq(1, n.prev, length.out=n.curr))
    
    # Normalize by grey value
    checkIfAnyAbove1 <- TRUE
    while(checkIfAnyAbove1) {
      # Normalise via gamma-corrected grey-value
      tempgraymap <- apply(cmap ^ gamma, 1, mean)
      tempgraymap <- tempgraymap ^ (1/gamma)
      cmap <- cmap / tempgraymap * seq(0, 1, length.out=n.curr)
      # Fix division by zero
      cmap[is.na(cmap)] <- 0
      cmap <- round(10000 * cmap) / 10000
      
      # Check that we have not normalised any values to greater than 1
      adjDelta <- 0.025;
      if(any(cmap > 1)) {
        rAbove1 <- which(cmap[, 1] > 1)
        if(length(rAbove1) > 0) {
          cmap[rAbove1, 1] <- (1 - adjDelta) * cmap[rAbove1, 1]
          cmap[rAbove1, -1] <- (adjDelta / 2) * (1 - cmap[rAbove1, -1]) + cmap[rAbove1, -1]
        }
        bAbove1 <- which(cmap[, 2] > 1)
        if(length(bAbove1) > 0) {
          cmap[bAbove1, 2] <- (1 - adjDelta) * cmap[bAbove1, 2]
          cmap[rAbove1, -2] <- (adjDelta / 2) * (1 - cmap[rAbove1, -2]) + cmap[rAbove1, -2]
        }
        gAbove1 <- which(cmap[, 3] > 1)
        if(length(gAbove1) > 0) {
          cmap[gAbove1, 3] <- (1 - adjDelta) * cmap[gAbove1, 3]
          cmap[rAbove1, -3] <- (adjDelta / 2) * (1 - cmap[rAbove1, -3]) + cmap[rAbove1, -3]
        }
      } else {
        checkIfAnyAbove1 <- FALSE
      }			
    }
    
    
    n.prev <- n.curr
    n.curr <- n.prev * 2 - 1
    cmap.prev <- cmap
    if (n.prev > n) allElemsDone <- TRUE
  }
  cmap <- apply(cmap.prev, 2, function(y) approx(x=1:n.prev, y, xout=seq(1, n.prev, length.out=n))$y)
  
  # Flip colourmap if required
  if(invert) cmap <- cmap[dim(cmap)[1]:1, , drop=FALSE]
  
  # Convert RGB values to hex codes and replace min/max colours
  if(alpha != 1){
    colours <- rgb(cmap, alpha=alpha)
  } else {
    colours <- rgb(cmap)
  }
  if(!is.null(minColour)) colours[1] <- minColour
  if(!is.null(maxColour)) colours[length(colours)] <- maxColour
  
  colours
}
