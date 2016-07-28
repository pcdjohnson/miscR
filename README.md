# miscR
Miscellaneous R functions

These functions can be sourced within R with the following lines, after replacing "************" with the function name:

  library(RCurl)
  options(RCurlOptions=list(cainfo=system.file("CurlSSL","cacert.pem",package="RCurl")))
  eval(expr=parse(text=getURL("https://raw.githubusercontent.com/pcdjohnson/miscR/master/************.R")))

At some point I'll get round to uploading them as a package
