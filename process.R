#
# Process.R
#
# Routines to extract range of observed flows from USGS gauging station
# and write 
# 

read.flow <- function(site, start.date, end.date = NULL, gauge = F, file = NULL)
{
	## read.flow function from IHA package (http://r-forge.r-project.org/R/?group_id=185)
	## original author Author: Jason Law <jason.e.law@gmail.com>
    ## this function is GPL and reproduced here b/c currently IHA isn't building for 2.15
  if (is.null(file)) {
    parameter <- 'cb_00060=on'
    if (gauge) parameter <- paste( parameter, 'cb_00065=on', sep = '&')
    if (is.null(end.date)){
      end.date <- ""
    } else {
      end.date <- sprintf('&end_date=%s', as.Date(end.date))
    }
    input <- sprintf("http://waterdata.usgs.gov/nwis/dv?site_no=%s&%s&begin_date=%s%s&format=rdb",
        site, parameter, as.Date(start.date), end.date) 
  } else {
    input <- file 
  }
  cat(paste("Retrieving data from: \n", input, "\n",sep=""))
  flow <- read.delim(file = input, header = T, comment.char = "#", as.is = T)
  nms <- c("agency","site_no","date","discharge","discharge_qual")
  if (gauge) nms <- c(nms, "gauge", "gauge_qual")
  names(flow) <- nms
  flow <- flow[-1,]
  flow$date<- as.POSIXct(strptime(flow$date, format="%Y-%m-%d"))
  flow$discharge <- as.numeric(flow$discharge)
  if (gauge){
    flow$gauge <- as.numeric(flow$gauge)
  }
  flow <- as.data.frame(flow)
  attr(flow, 'url') <- input
  class(flow) <- c('flow', 'data.frame')
  cat("Finished!\n")
  return(flow)
}


readGauge <- function(name='11458000', start.date=ISOdate(1930,1,1)){
  flow <- read.flow(name, start.date=start.date)
  return(flow)
}

rangeGauge <- function(flow.df, density=100, date.min=NULL, date.max=NULL){
  # flow.df is class flow and/or data.frame
  # with data in slot $discharge and optionally date observations in slot $date
  if(is.null(flow.df$discharge))
    stop("no observations in slot discharge in flow.df")
  df <- flow.df
  if(!is.null(date.min) | !is.null(date.max)){
    if(is.null(df$date))
      stop("no date slot in flow.df")
    if(!is.null(date.min))
      df <- subset(df, date > date.min)
    if(!is.null(date.max))
      df <- subset(df, date < date.max)
  }
  regSample(df$discharge, density)
}

regSample <- function(observations, density){
  obs <- observations
  obs <- obs[obs!=0]
  rng<- range(obs)
  lo <- rng[1]
  hi <- rng[2]
  seq(lo, hi, length.out=density)
}


