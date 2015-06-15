RR.plot <- function (RRS, RRS, d=c(names.d, names.d.subgroups), e, zone='NY' ) {
  plot( round(RRS[d[1],e, zone] / RRS[d[1], e, 'USA'], 2) , main=paste('RR by group for ', zone, ' for ', e, sep=''), xlab=d)
  for (i in 2:length(d)) {
    lines( round(RRS[d[i], e, zone]/RRS[d[i], e, 'USA'], 2) )
  }
}
