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

#' cross_sectional: import a cross-sectional variable
#'
#' @param data
#' @param var_name
#' @param path.
#'
#' @return
#' @export
#'
#' @examples
#' imsbasics::clc()
#' library(tidyverse); library(shp); library(kml)
#' path <- "data/rawdata/Data SPSS"
#' open(path) %>% longitudinal("PXXF50") %>% cross_sectional("AGE10")
#' open(path) %>% longitudinal("PXXF50") %>% cluster() %>% cross_sectional("AGE10")
#' data <- open(path) %>% longitudinal("PXXF50") %>% cluster() %>% cross_sectional("AGE10")
cross_sectional <- function(data, var_name, path. = path) {
  # browser()
  path. <- paste0(path., "/SHP-Data-W1-W19-SPSS")
  year <- readr::parse_number(var_name)

  file <- paste0("SHP", as.character(sprintf('%02d', year %% 100)), "_P_USER.sav")
  if(year < 50) year <- paste0("20", year) else year <- paste0("19", year)
  path. <- paste0(path., "/W", as.character(as.numeric(year) - 1998), "_", as.character(year))

  df <- import_cols(file, path., cols = c("IDPERS", var_name))
  names(df)[1] <- "ID"

#
#   cross_sectional <- import_cols(
#     paste0("SHP", year, "_P_USER.sav"), paste0("../data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W12_2010", cols = c("IDPERS", "AGE10"))
#   names(age)[1] <- "ID"
  data <- left_join(data, df)
  return(data)
}


#' cluster
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
#' data <- open(path) %>% longitudinal("PXXF50") %>% cluster()
#' data <- open(path) %>% longitudinal("PXXF50") %>% cluster(interactive = TRUE)
#' data <- open(path) %>% longitudinal("PXXF50") %>% cluster(nr_clusters = 5)
#' open(path) %>% longitudinal("PXXC75") %>% cluster()
cluster <- function(data, nr_clusters = c(2:6), interactive = FALSE) {
  # browser()
  res <- data %>% filter(complete.cases(.))
  data <- data %>% filter(complete.cases(.))
  data <- clusterLongData(data)
  # res <- kml(data, nr_clusters, parAlgo = parKml(saveFreq = Inf))
  kml(data, nr_clusters)
  data@criterionActif <- "Ray.Turi"
  if (interactive) { x11(type = "Xlib"); choice(data) }
  res$cluster <- as.character(getClusters(data, nr_clusters[1]))
  plotMeans(data, c(nr_clusters[1], 1))
  return(res)
}

#' Title
#'
#' @param data
#' @param what
#' @param with
#'
#' @return
#' @export
#'
#' @examples
#' imsbasics::clc()
#' library(tidyverse); library(shp); library(kml)
#' path <- "data/rawdata/Data SPSS"
#' open(path) %>% longitudinal("PXXF50") %>% cluster() %>% cross_sectional("SEX10") %>% explain()
#' open(path) %>% longitudinal("PXXF50") %>% cluster() %>% cross_sectional("AGE10") %>% explain()
#' data <- open(path) %>% longitudinal("PXXF50") %>% cluster()
#' data %>% cross_sectional("AGE10") %>% explain()
#' data %>% cross_sectional("SEX10") %>% explain()
explain <- function(data, what = cluster, with = as.name(names(data)[length(names(data))])) {
  if (length(unique(data[[with]])) < 5) {
    what <- enquo(what)
    with <- enquo(with)

    table(select(data, !!what, !!with))
  } else {
    what <- enquo(what)
    with <- enquo(with)

    data %>% group_by(!!what) %>% summarise(!!with := mean(!!with))
  }
}
