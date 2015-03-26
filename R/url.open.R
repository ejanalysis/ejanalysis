url.open <- function(myurl) {
  if (get.os()=='win') {
    shell.exec(myurl)
  } else {
    system( paste('open ', myurl, sep='') )
  }  
}

