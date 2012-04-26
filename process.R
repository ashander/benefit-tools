#
# Process.R
#
# Routines to extract range of observed flows from USGS gauging station
# and write 
# 


readGauge <- function(name='11458000', start.date=ISOdate(1930,1,1)){
  require(IHA)
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


