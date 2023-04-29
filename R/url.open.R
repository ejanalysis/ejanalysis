#' @title Launch a web browser to go to a URL, like browseURL does
#'
#' @description Just the same as [browseURL()]
#' @param myurl required character element, URL to open
#' @return Just launches browser to open URL
#' @seealso [browseURL()], and [shell.exec()] which this uses
#' @export
url.open <- function(myurl) {

  # also just see browseURL()

  if (analyze.stuff::get.os()=='win') {
    shell.exec(myurl)
  } else {
    system( paste('open ', myurl, sep='') )
  }
}

