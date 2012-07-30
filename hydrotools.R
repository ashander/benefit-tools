### Functions

require(hydroTSM)
require(ggplot2)

## plotting /conversion 
dfHyd <- function(d){
  d2 <- as.data.frame(d)
  class(row.names(d2))
  names(d2) <- 'discharge'
  d2$date <- tryCatch(as.POSIXct(row.names(d2)), error=function(e){as.numeric(row.names(d2))})
  row.names(d2) <- NULL
  return(d2)
}


gHyd <- function(d.z){
  d2 <-  dfHyd(d.z)
  require(ggplot2)
  g <-ggplot(d2, aes(date, discharge)) 
  return(g)
}


## extracting indices of various months
Month2Index <- function(mon, d.zoo){
  mon.number <- match(mon, month.abb)
  if(length(mon.number) > 1) {
    mon.char <- list()
    for(mon in mon.number)
      mon.char <- append(mon.char, ifelse(mon < 10, paste('-0', mon,'-',sep=''), paste('-', mon,'-',sep='')))
    mon.char <- do.call(paste, c(mon.char, sep="|"))
  }
  if(length(mon.number) <= 1)
    mon.char <- ifelse(mon.number < 10, paste('-0', mon.number,'-',sep=''), paste('-', mon.number,'-',sep=''))
  numind <- grep(mon.char, index(d.zoo))
  return(numind)
}

