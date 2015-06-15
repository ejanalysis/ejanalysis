RR.table.vs.us <- function(RRS, d, e, zone) {
  round(RRS[d, e, zone] / RRS[d, e, 'USA'], 2)
}
