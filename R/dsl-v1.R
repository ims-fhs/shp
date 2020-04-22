#' open opens and checks a connection to the directory specified by path.
#'
#' @param path
#'
#' @return
#'
#' @examples
#' open("data/rawdata/Data SPSS")
#' open("data/rawdata")
#' open("data/rawdata/Data QWERTZ")
open <- function(path) {
  assertthat::assert_that(dir.exists(path))
  assertthat::assert_that(dir.exists(paste0(path, "/SHP-Data-W1-W19-SPSS")))
  assertthat::assert_that(dir.exists(paste0(path, "/SHP-Data-W1-W19-SPSS/W1_1999")))
  assertthat::assert_that(assertthat::is.string(path))
  path <<- path
  return(path)
}


#' longitudinal: import a longitudinal variable, the longest available sequence will  be taken
#'
#' @param data
#' @param path
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
#' imsbasics::clc()
#' library(tidyverse); library(shp); library(kml)
#' path <- "data/rawdata/Data SPSS"
#' open(path) %>% longitudinal("PXXF50")
#' open(path) %>% longitudinal("PXXC75")
longitudinal <- function(data, var_name, path. = path) {
  # browser()
  available <- years_available("P_USER.sav", paste0(path., "/SHP-Data-W1-W19-SPSS"), var_name)
  available$available <- with(rle(available$available), rep(seq_along(values) == which.max(lengths * values), lengths))
  years <- as.numeric("1999"):as.numeric("2017")
  years <- years[available$available]
  assertthat::assert_that(length(years) > 1, msg = "The data specified is not available for more than one year in a row. Try another variable.")
  print(paste0("Longitudinal available for years: ", min(years), " - ", max(years)))
  data <- import_long_cols("P_USER.sav", paste0(path., "/SHP-Data-W1-W19-SPSS"), cols = c("IDPERS", var_name), year_start = as.character(min(years)), year_end = as.character(max(years)))
  return(data)
}

# cross_sectional <- function(data, path = check_path(path), var_name) {
#   cross_sectional <- import_cols("SHP10_P_USER.sav", "../data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W12_2010", cols = c("IDPERS", "AGE10"))
#   names(age)[1] <- "ID"
#   data <- left_join(data, age)
#   return(data)
# }


#' longitudinal: import a longitudinal variable, the longest available sequence will  be taken
#'
#' @param data
#' @param path
#' @param var_name
#'
#' @return
#' @export
#'
#' @examples
#' imsbasics::clc()
#' library(tidyverse); library(shp); library(kml)
#' path <- "data/rawdata/Data SPSS"
#' open(path) %>% longitudinal("PXXF50") %>% cluster()
#' open(path) %>% longitudinal("PXXC75") %>% cluster()
cluster <- function(data, nr_clusters = c(2:6)) {
  data <- data %>% filter(complete.cases(.))
  data <- clusterLongData(data)
  # res <- kml(data, nr_clusters, parAlgo = parKml(saveFreq = Inf))
  res <- kml(data, nr_clusters)
  data@criterionActif <- "Ray.Turi"
  x11(type = "Xlib"); choice(data)
  return(data)
}

# explain <- function(data) {
# }
