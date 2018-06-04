#' Plot Hydrograph
#'
#' hydrograph.plot creates a hydrograph plot for simulated, observed, and inflow 
#' hydrograph series, including precipitation if provided.
#'
#' @author Robert Chlumsky <rchlumsk@gmail.com>
#' 
#' This function creates a hydrograph plot using the supplied time series; any
#' series not supplied will not be plotted. If the precip time series is
#' supplied, the secondary y axis will be used to plot the precip time series.
#'
#' The function assumes that the supplied time series have the same length and
#' duration in time. If this is not true, then the defined period or period
#' calculated from the first available flow series will be used to determine
#' the plotting limits in time. If the data is used directly from Raven output,
#' this is not a concern. The supplied time series should be in xts format,
#' which again can be obtained directly by using the hyd.extract function.
#'
#' The winter.shading argument will add a transparent cyan shading for the
#' December 1st to March 31st period in each year that is plotted.
#'
#' The range.mult argument will increase the maximum value that is plotted in
#' the flows and the precip values. This is useful in preventing overlap if
#' precip is also plotted (i.e. with precip as well, range.mult=1.5 works
#' well). This value should not be less than 1.0, otherwise the values will be
#' cutoff in the plot.
#'
#' ylabel is the label on the y axis, defined using y.lab in the plot function.
#' This defaults to 'Flow [m3/s]' intended for plotting hydrographs.
#'
#' leg.pos is the position for the legend to be placed, e.g. 'topleft',
#' 'right', etc., and is consistent with the legend function options. If this
#' is left null, the function will place it either topleft or left, depending
#' on whether precip is also plotted (i.e. left if precip added, topleft
#' otherwise).
#'
#' leg.box is a boolean for whether to put the legend in an opaque. white box
#' or not. If left as NULL, the function will automatically not use a white box
#' and leave the background of the legend transparent.
#'
#' zero.axis can be used to set the min value of the y axis (or axes if precip
#' also plotted) to zero. Note that by default, R will plot the values with a
#' slight buffer for presentation. A warning that if this option is set to
#' TRUE, the minimum value is set to zero without checking if any flow values
#' are less than zero. This option should not be used for stage plotting, since
#' most reservoirs are not in the range of 'zero' stage for normal operations,
#' since stage is reported to elevation and not to stage bottom, typically.
#' 
#' plot.mode is used to indicate the plot type to output. Currently only 'base' 
#' plot type is available, 'ggplot' is under construction.
#'
#' Note that a plot title is purposely omitted in order to allow the automatic
#' generation of plot titles. 
#'
#' @param flows data frame of flows to plot
#' @param precip data frame of precipitation values to plot
#' @param prd period to use in plotting
#' @param winter.shading optionally adds shading for winter months (default
#' FALSE)
#' @param range.mult.flow range multiplier for max value in hydrograph
#' @param range.mult.precip range multiplier for max value in precipitation plot (default 1.5)
#' @param flow.labels string vector of labels for flow values
#' @param ylabel text label for y-axis of the plot (default 'Flow [m3/s]')
#' @param precip.label text label for precipitation y-axis (default 'Precipitation [mm]')
#' @param leg.pos string specifying legend placement on plot
#' @param leg.box boolean on whether to put legend in an opaque box
#' @param zero.axis fixes the y axis to start exactly at zero (default TRUE)
#' @param plot.mode plot mode as "base" or "ggplot"
#' @return \item{TRUE}{return TRUE if the function is executed properly}
#' @keywords plot hydrograph
#' @examples 
#' # example with randomly sampled data
#' dd <- seq.Date(as.Date("2010-10-01"),as.Date("2013-09-30"),by=1)
#' x <- abs(rnorm(length(dd)))
#' y <- abs(rnorm(length(dd)))*x
#' df <- data.frame("Date"=dd,x,y)
#' myprd <- "2011-10-01/2012-09-30"
#' 
#' precip <- data.frame("Date"=dd,"precip"=abs(rnorm(length(dd)))*10)
#'
#' # basic hydrograph plot
#' hydrograph.plot(flows=df,winter.shading=F) 
#' 
#' with different labels
#' hydrograph.plot(flows=df,winter.shading=F,flow.labels=c("simulated","observed")) 
#' 
#' # with a few more options turned on
#' hydrograph.plot(flows=df,precip=precip)
#' 
#' # increase the plot ranges to separate flows and precip; add a legend box
#' hydrograph.plot(flows=df,precip=precip,range.mult.flow=1.7,range.mult.precip=2,leg.box=T)
#'  
#' @export hydrograph.plot
hydrograph.plot <- function(flows=NULL,precip=NULL,prd=NULL,winter.shading=F,range.mult.flow=NULL,range.mult.precip=1.5,
  flow.labels=NULL,ylabel="Flow [m3/s]",precip.label="Precipitation [mm]",leg.pos=NULL,leg.box=NULL,zero.axis=T,
  plot.mode="base") {
  
  # check plot mode
  if (plot.mode == "ggplot") {
    warning("ggplot plotting is under construction, defaulting to base plot type.")
  }
  
  # check flows data frame
  if (is.null(flows) | class(flows) != "data.frame") {
    stop("flows data frame is required.")
  } else if (nrow(flows) == 0) {
    stop("flows data frame cannot be empty (zero rows).")
  } else if (ncol(flows) == 1) {
    stop("flows data frame cannot be empty (no data columns).")
  }
  if (is.null(flows$Date)) {
    stop("Date attribute is required in flows data frame.")
  }
  if (ncol(flows) >= 11) {
    stop("flows cannot have more than 10 data columns (excluding date).")
  }
  if (which(colnames(flows) == "Date") != 1) {
    stop("'Date' must be the first attribute of flows data frame.")
  }
  
  # check flow labels
  if (!(is.null(flow.labels))) {
    if (length(flow.labels) != ncol(flows)-1)  {
      stop("flow.labels must have the same number of labels as the flows data frame (not including Date attribute).")
    }
  } else {
    flow.labels <- colnames(flows)[2:ncol(flows)]
  }
  
  # check precip data frame
  if (!(is.null(precip))) {
    if (class(precip) != "data.frame") {
      stop("precip must be a data frame.")
    } 
    if (nrow(precip) == 0) {
      stop("precip data frame cannot be empty (zero rows).")
    } 
    if (ncol(precip) == 1) {
      stop("precip data frame cannot be empty (no data columns).")
    }
    if (which(colnames(precip) == "Date") != 1) {
      stop("'Date' must be the first attribute of precip data frame.")
    }    
    if (is.null(precip$Date)) {
      stop("Date attribute is required in precip data frame.")
    }
    if (ncol(precip) > 2) {
      stop("flows cannot have more than 1 data column (other than 'Date').")
    }
  }
  
  # check range.mult input
  if (!(is.null(range.mult.flow))) {
    if (range.mult.flow <= 0) {
      stop("range.mult.flow must be a positive value.")
    }
    if (range.mult.flow < 1) {
      warning("range.mult.flow is less than one, plot may be cut off.")
    }
  }
  if (!(is.na(range.mult.precip))) {
    if (range.mult.precip <= 0) {
      stop("range.mult.precip must be a positive value.")
    }
    if (range.mult.precip < 1) {
      warning("range.mult.precip is less than one, plot may be cut off.")
    }
  }
  
  # adjust range.mult.flow if precip is NULL
  if (is.null(range.mult.flow)) {
    if (is.null(precip)) {
      range.mult.flow <- 1.05
    } else {
      range.mult.flow <- 1.5
    }
  }
  
  # determine period ----
  if (!(is.null(prd))) {
    
    # period is supplied; check that it makes sense
    firstsplit <- unlist(strsplit(prd,"/"))
    if (length(firstsplit) != 2) {
      stop("Check the format of supplied period; should be two dates separated by '/'.")
    }
    if (length(unlist(strsplit(firstsplit[1],"-"))) != 3 || length(unlist(strsplit(firstsplit[2],"-"))) != 3
        || nchar(firstsplit[1])!= 10 || nchar(firstsplit[2]) != 10) {
      stop("Check the format of supplied period; two dates should be in YYYY-MM-DD format.")
    }
    if (nrow(date.subset(flows,prd)) == 0) {
      stop("prd does not overlap with flows; check prd and flows data frame.")
    }
    
  } else {
    # period is not supplied
    
    # define entire range as period
    N <- nrow(flows)
    prd <- sprintf("%d-%02d-%02d/%i-%02d-%02d",lubridate::year(flows$Date[1]),lubridate::month(flows$Date[1]),lubridate::day(flows$Date[1]),
                   lubridate::year(flows$Date[N]),lubridate::month(flows$Date[N]),lubridate::day(flows$Date[N]))
  }
  
  # subset data
  flows <- date.subset(flows,prd)
  
  if (!(is.null(precip))) {
    precip <- date.subset(precip,prd)
    if (nrow(precip) == 0) {
      warning("precip data does not overlap with prd; check data and prd arguments.")
      precip <- NULL
    }
  }

  # capture plotting parameters, restore afterwards
  .pardefault <- par(no.readonly = T)
  
  # set parameters for plotting; then plot
  if(!(is.null(precip))) {
    par(mar=c(5, 4, 4, 4) + 0.1)
  }
  if (zero.axis) {
    # sets the interval calculation in plotting to be right to specified limits
    # otherwise extends by 4% by default
    par(yaxs='i')
  }
  
  y.hmax <- max(flows[,2:ncol(flows)],na.rm=T)*range.mult.flow
  
  if (zero.axis) {
    y.hmin<-0
  } else {
    y.hmin <- min(flows[,2:(ncol(flows))],na.rm=T)
  }
  
  plot(flows$Date,flows[,2],xlab="Date",ylab=ylabel,
       col='white',type='l',ylim=c(y.hmin,y.hmax), panel.first=grid())
  if (winter.shading) {
    # shaded winter months
    temp <- flows[((lubridate::month(flows$Date) == 12) & (lubridate::day(flows$Date) == 1)) | 
                    ((lubridate::month(flows$Date) == 3) & (lubridate::day(flows$Date) == 31)),]
    ep <- match(lubridate::date(temp$Date),lubridate::date(flows$Date))
    if (lubridate::month(flows$Date[ep[1]])==3) {
      # ep <- ep[-1]
      ep <- c(1,ep)
    }
    if (lubridate::month(flows$Date[ep[length(ep)]])==12) {
      # ep <- ep[-length(ep)]
      ep <- c(ep,nrow(flows))
    }
    bc <- "#00FFFF32"
    for (k in seq(1,length(ep),2)) {
      cord.x <- c(lubridate::date(flows$Date[ep[k]]),lubridate::date(flows$Date[ep[k]]),
                  lubridate::date(flows$Date[ep[k+1]]),lubridate::date(flows$Date[ep[k+1]]))
      cord.y <- c(-1e3,y.hmax*1e3,y.hmax*1e3,-1e3)
      polygon(cord.x,cord.y,col=bc,border=NA)
    }
  }
  
  # define legend items
  NN <- ncol(flows)-1
  leg.items <- flow.labels
  # want to replace colour selection with a nice package that picks nice colours together, for any number of inputs
  leg.cols <- c('red','navyblue','black','orange','cyan','darkgreen','coral1','deeppink','blue','orangered3')[1:NN]
  leg.lty <- rep(seq(1,5,1),3)[1:NN]
  leg.lwd <- rep(1,NN)
  
  # add all flow data to plot
  for (i in 1:NN) {
    lines(flows$Date,flows[,(i+1)],lty=leg.lty[i],lwd=leg.lwd[i],col=leg.cols[i])
  }
  
  # add precip data if not null
  if (!(is.null(precip))) {
    par(new=T)
    precip.col <- "#0000FF64"
    plot(precip$Date,precip[,2],col=precip.col,lty=1,lwd=1,
         type='h',ylim=rev(c(0,max(precip[,2],na.rm=T)*range.mult.precip)),xaxt='n',yaxt='n',
         xlab="",ylab="")
    axis(4)
    mtext(sprintf("%s",precip.label),side=4,line=2.5)
    
    leg.items <- c(leg.items,precip.label)
    leg.cols <- c(leg.cols,precip.col)
    leg.lty <- c(leg.lty,1)
    leg.lwd <- c(leg.lwd,1)
  }
  
  if (is.null(leg.pos)) {
    if (!(is.null(precip))) {
      leg.pos <- 'left'
    } else {
      leg.pos <- 'topleft'
    }
  }
  if (is.null(leg.box)) {
    leg.box <- 'n'
  } else {
    if (leg.box) {
      leg.box <- 'o'
    } else {
      leg.box <- 'n'
    }
  }
  
  # add legend to plot
  legend(x=leg.pos,legend=leg.items,lty=leg.lty,col=leg.cols,
         lwd=leg.lwd,bty=leg.box,cex=0.8,inset=0.01)
  
  # restore plotting parameters
  par(.pardefault)
  
  return(TRUE)
}

