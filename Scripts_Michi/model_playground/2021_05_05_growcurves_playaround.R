# Abstract: Bayesian Semi- and Non-Parametric Models for Longitudinal Data with Multiple Membership Effects in R
#
# "grovcurves offers two alternative sets of hierarchival models"
#
# We introduce growcurves for R that performs analysis of repeated measures multiple
# membership (MM) data. This data structure arises in studies under which an intervention is
# delivered to each subject through the subject’s participation in a set of multiple elements
# that characterize the intervention. In our motivating study design under which subjects receive
# a group cognitive behavioral therapy (CBT) treatment, an element is a group CBT session and each
# subject attends multiple sessions that, together, comprise the treatment. The sets of elements,
# or group CBT sessions, attended by subjects will partly overlap with some of those from other
# subjects to induce a dependence in their responses. The growcurves package offers two alternative
# sets of hierarchical models:
#   1. Separate terms are specified for multivariate subject and MM element
#      random effects, where the subject effects are modeled under a Dirichlet process prior to produce a
#      semi-parametric construction;
#   2. A single term is employed to model joint subject-by-MM effects.
#      A fully non-parametric dependent Dirichlet process formulation allows exploration of differences
#      in subject responses across different MM elements.
#
# This model allows for borrowing information among subjects who express similar longitudinal trajectories
# for flexible estimation. !!!!!!!!!!
#
# growcurves deploys “estimation” functions to perform posterior sampling under
# a suite of prior options. An accompanying set of “plot” functions allows the user to readily extract
# by-subject growth curves. The design approach intends to anticipate inferential goals with tools that
# fully extract information from repeated measures data. Computational efficiency is achieved by
# performing the sampling for estimation functions using compiled C++ code



# Build the package from the newest tarball avialable
#    Tarballs: https://cran.r-project.org/web/packages/growcurves/index.html
#    Set path to rtools for correct build: https://cran.r-project.org/bin/windows/Rtools/

# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# --> then restart R
Sys.which("make") # should return "C:\\rtools40\\usr\\bin\\make.exe"


path_to_tarball_file <- "C:/packages/growcurves_0.2.4.1.tar.gz"
# Package RcppArmadillo makes problems -> aktuelle Version (0.10...) funktioniert nicht.
# growcurves benötigt RcppArmadillo >= 0.5.0 (https://cran.r-project.org/src/contrib/Archive/RcppArmadillo/)

# devtools::install_version("RcppArmadillo", version = "0.5.000.0", repos = "http://cran.us.r-project.org")
devtools::install_version("RcppArmadillo", version = "0.5.100.1.0", repos = "http://cran.us.r-project.org")
# devtools::install_version("RcppArmadillo", version = "0.5.200.1.0", repos = "http://cran.us.r-project.org")
devtools::install_version("RcppArmadillo", version = "0.6.100.0.0", repos = "http://cran.us.r-project.org")

utils::install.packages(pkgs = path_to_tarball_file, repos = NULL, type = "source")

imsbasics::pre_commit()




setwd("C:/packages/growcurves")
devtools::build()
devtools::install()




devtools::install_version("growcurves", version = "0.2.4.1", repos = "http://cran.us.r-project.org")
