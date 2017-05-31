###Function used to plot sensitivity values on the visual field
###Function for plotting a time series of data at each location on the visual field
#'
#' plot.Sensitivity
#'
#' Plots a heat map of the differential light sensitivity on the Humphrey Field
#' Analyzer-II visual field.
#'
#' @param Y a time series variable to be plotted.
#'
#' @param Location a variable corresponding to the location on the visual field
#'  that the time series variable was observed.
#'
#' @param Time a variable corresponding to the time that the time series variable
#'  was observed.
#'
#' @param main an overall title for the plot.
#'
#' @param xlab a title for the x axis.
#'
#' @param ylab a title for the y axis.
#'
#' @details \code{plot.VfTimeSeries} is used in the application of glaucaom progression.
#'  In each cell is the observed DLS at each location over visits, with the red line
#'  representing a linear regression trend.
#'
#' @examples
#' \dontrun{
#' data(VFSeries)
#' plot.VfTimeSeries(Y = VFSeries$DLS,
#'                   Location = VFSeries$Location,
#'                   Time = VFSeries$Time,
#'                   main = "Visual field sensitivity time series \n at each location",
#'                   xlab = "Days from baseline visit",
#'                   ylab = "Differential light sensitivity (dB)")
#'
#' }
#'
#' @author Samuel I. Berchuck
#'
#' @export
plot.Sensitivity <- function(Y = Y, main = "Sensitivity Estimate (dB) at each \nlocation on visual field",
                            zlim, bins = 100, border = TRUE, legend = TRUE,
                            color = c("yellow", "orange", "red")) {

  ##Note: Depends on library classInt

  ###Create Legend Cutoffs
  labs <- levels(cut(Y, bins))
  labs <- cbind(lower = as.numeric(sub("\\((.+),.*","\\1", labs)), upper = as.numeric(sub("[^,]*,([^]]*)\\]","\\1", labs)))
  legvals <- as.numeric(c(labs[1, 1], labs[ , 2]))
  legvals[1] <- -Inf
  legvals[length(legvals)] <- Inf

  ###Get color specification
  colbr <- colorRampPalette(color)
  colpal <- colbr(bins)
  fixed_obs <- suppressWarnings(classInt::classIntervals( Y[!is.na(Y)], style = "fixed", fixedBreaks = legvals))
  fixedcol_obs <- classInt::findColours(fixed_obs, colpal)

  ###Create plotting functions
  square <- function(x, y, col) symbols(x, y, squares = 1, fg = col, bg = col, inches = FALSE, add = TRUE)
  format2<-function(x) format(round(x,2),nsmall=2)
  format0<-function(x) format(round(x,0),nsmall=0)

  ###Get square coordinates
  Loc <- data.frame(x = c(4:7, 3:8, 2:9, 1:9, 1:9, 2:9, 3:8, 4:7), y = c(rep(1, 4), rep(2, 6), rep(3, 8), rep(4, 9), rep(5, 9), rep(6, 8), rep(7, 6), rep(8, 4)))
  Loc <- Loc[order(Loc$y, decreasing = TRUE),]
  rownames(Loc) <- 1 : 54
  Loc <- Loc[-c(26, 35), ] #remove blind spot

  ###Initiate figure with squares
  pardefault <- suppressWarnings(par(no.readonly = T))
  par(mfcol = c(1, 1), pty = "m", mai = c(0, 0, 0.75, 0))
  plot(1, 1, main = main, type = "n", yaxt = "n", xaxt = "n", bty = "n", xlim = c(-2, 14), ylim = c(2, 7), asp = 1, ylab = "", xlab = "")
  for (i in 1 : 52) {
    x <- Loc[i, 1] + 0.5
    y <- Loc[i ,2] + 0.5
    square(x, y, col = fixedcol_obs[i])
  }
  square(8 + 0.5, 5 + 0.5, col = "grey")
  square(8 + 0.5, 4 + 0.5, col = "grey")

  ###Add border
  if (border) {
    hloop<-list(4:7,c(3,8),c(2,9),1,NULL,1,c(2,9),c(3,8),4:7)
    vloop<-list(4:5,c(3,6),c(2,7),c(1,8),NULL,NULL,NULL,c(1,8),c(2,7),3:6)
    for (j in 1:9) {
      for (i in hloop[[j]]) {
        segments(i,j,i+1,j,lwd = 1.5)
      }
    }
    for (i in 1:10) {
      for (j in vloop[[i]]) {
        segments(i,j,i,j+1,lwd = 1.5)
      }
    }
  }

  ###Add legend
  if (legend) {
    if (missing(zlim)) zlim <- c(min(Y), max(Y))
    NColors <- length(colpal)
    Vertical <- seq(3, 7, length.out = NColors)
    for (i in 1 : NColors) segments(11, Vertical[i], 11.75, Vertical[i], col = colpal[i], lwd = 1.5)
    minx <- zlim[1]
    maxx <- zlim[2]
    LegendPV <- seq(minx, maxx, length.out = 5)
    segments(11.75, 3, 11.75, 7, lwd = 1.5)
    segments(11 ,3 ,11 ,7 , lwd = 1.5)
    segments(11 ,7 ,11.75, 7, lwd = 1.5)
    segments(11 ,3 ,11.75, 3, lwd = 1.5)
    for (i in 1 : length(LegendPV)) {
      text(12.75, (3:7)[i], format0(LegendPV[i]))
      segments(11.75, (3:7)[i], 12, (3:7)[i], lwd = 1.5)
    }
    text(11.5, 7.5, "DLS (dB)")
  }

  ###Return to default par setting
  par(pardefault)

###End function
}
