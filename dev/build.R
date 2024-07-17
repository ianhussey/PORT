
library(roxygen2)
#setwd("~/git/")
#devtools::create("PORT")
setwd("~/git/PORT")

devtools::document()

#devtools::build_vignettes()
#devtools::check()

devtools::check(vignettes = FALSE)

#devtools::install()
# or from github, after push
devtools::install_github("ianhussey/PORT")

library(PORT)

?PORT

detach("package:PORT", unload=TRUE)
