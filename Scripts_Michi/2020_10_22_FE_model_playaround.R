## ---------------------------
## Purpose of script: Play around with FE-models
##
## Author: Michael Schmid
##
## Date Created: 2020-10-22
##
## Copyright (c) Timothy Farewell, 2018
## Email: hello@timfarewell.co.uk
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

imsbasics::clc()
library(tidyverse)
library(shp)

## -----------------------------------------------------------------------------

# Import one Wave
shp99_p_user_3.1.0 <- import_SPSS_file("SHP99_P_USER.sav", "data/dataset_932_V.3.2.1/Data_SPSS/SHP-Data-W1-W20-SPSS/W1_1999")

