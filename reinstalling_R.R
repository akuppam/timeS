# ===============
# re-installing R
# (1) before uninstalling, save all R packages to a file....use the link below
# http://ianmadd.github.io/pages/EasyRPackageReinstall.html
# (2) to uninstall manually....use link below
# https://osxuninstaller.com/uninstall-guides/uninstall-r/
# (3) after removing R and R studio contents from Mac and Trash.....install R and RStudio....use link below
# https://medium.com/@GalarnykMichael/install-r-and-rstudio-on-mac-e911606ce4f4
# (4) after re-installing R and RStudio, load all R packages that were saved to a file....use the link below
# http://ianmadd.github.io/pages/EasyRPackageReinstall.html
# ===============

# to save current packages
setwd("/Users/akuppam/Documents/Hprog/R/")
packages <- installed.packages()[,"Package"]
save(packages, file="Rpackages")
View(packages)  # 234 packages

# to re-load all saved packages
setwd("/Users/akuppam/Documents/Hprog/R/")
load("Rpackages") 
for (p in setdiff(packages, installed.packages()[,"Package"]))
  install.packages(p)

# to cross-check all re-loaded packages
setwd("/Users/akuppam/Documents/Hprog/R/")
packages3 <- installed.packages()[,"Package"]
save(packages3, file="Rpackages3")
View(packages3)   # 240 packages

# if packages are not available for the latest version, us this way of installing
# https://www.haktansuren.com/installing-r-package-fixing-package-xxx-is-not-available-for-r-version-x-y-z-warning/
install.packages('package-name',repos='http://cran.us.r-project.org')

# use this cran-mirror for the region closest to me
install.packages('package-name',repos='http://cran.revolutionanalytics.com')
# example, installing fitdistrplus was done successfully using this
install.packages('fitdistrplus',repos='http://cran.revolutionanalytics.com')
install.packages('survival',repos='http://cran.revolutionanalytics.com')

# R packages available under CRAN
# https://cran.r-project.org/web/packages/available_packages_by_name.html

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir('/Users/akuppam/Documents/fitdistrplus/R')

install.packages("fitdistrplus", repos="http://R-Forge.R-project.org")












