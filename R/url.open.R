url.open <- function(myurl) {
  
  # also just see browseURL()
  
  if (get.os()=='win') {
    shell.exec(myurl)
  } else {
    system( paste('open ', myurl, sep='') )
  }  
}

