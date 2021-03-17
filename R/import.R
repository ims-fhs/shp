#' import_SPSS_file
#'
#' @param file
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' shp99_p_user <- import_SPSS_file("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W1_1999")
import_SPSS_file <- function(file = stop("SHP99_P_USER.sav"),
                             path = stop("data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W1_1999")) {
  df <- foreign::read.spss(paste0(getwd(), "/", path, "/", file), use.value.labels = TRUE, to.data.frame = TRUE, max.value.labels = -1)
  assertthat::assert_that(
    assertthat::are_equal(class(df), "data.frame")
  )
  return(df)
}

#' import_SPSS_file_head
#'
#' @param file
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' shp99_h_user_head <- import_SPSS_file_head("SHP99_H_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W1_1999")
#' shp99_p_user_head <- import_SPSS_file_head("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W1_1999")
#' shp04_p_user_head <- import_SPSS_file_head("SHP04_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W6_2004")
import_SPSS_file_head <- function(file = stop("SHP99_P_USER.sav"),
                                  path = stop("data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W1_1999")) {
  df <- foreign::read.spss(paste0(getwd(), "/", path, "/", file), use.value.labels = TRUE, to.data.frame = TRUE, max.value.labels = -1)
  assertthat::assert_that(
    assertthat::are_equal(class(df), "data.frame")
  )
  return(df[1:5, ])
}


#' import_id
#'
#' @param file
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' shp99_p_user_id <- import_id("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W1_1999")
import_id <- function(file = stop("SHP99_P_USER.sav"),
                      path = stop("data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W1_1999"))
{
  df <- import_SPSS_file(file, path)
  df <- df[, 2]
  return(df)
}


#' import_cols
#'
#' @param file
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' shp99_p_user_cols23 <- import_cols("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W1_1999", cols = c(2,3))
#' difficulties to conciliate personal and professional life last 12 months
#' shp99_p_user_cols_id_p99f09 <- import_cols("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W1_1999", cols = c("IDPERS", "P99F09"))
#' shp04_p_user_cols_id_p04w604 <- import_cols("SHP04_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W6_2004", cols = c("IDPERS", "P04W604"))
import_cols <- function(file = stop("SHP99_P_USER.sav"),
                        path = stop("data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS/W1_1999"),
                        cols = stop(c("IDPERS", "P04W604"))) {
  df <- import_SPSS_file(file, path)
  assertthat::assert_that(all(cols %in% names(df)))
  df <- df[, cols]
  assertthat::assert_that(
    assertthat::are_equal(ncol(df), length(cols))
  )
  return(df)
}

#' import_long_cols
#'
#' @param file
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' shp_p_user_cols_id_p99f09 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS", cols = c("IDPERS", "PYYF09"))
#' shp_p_user_cols_id_p99f09 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS", cols = c("IDPERS", "PYYF09"), year_start = "1999", year_end = "2003")
#' # work conditions stress
#' shp_p_user_cols_id_w604 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS", cols = c("IDPERS", "P04W604"), year_start = "2004", year_end = "2004")
#' shp_p_user_cols_id_w604 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS", cols = c("IDPERS", "P04W604"), year_start = "2004", year_end = "2017")
#' # interference work - private
import_long_cols <- function(file = stop("P_USER.sav"),
                             path = stop("data/rawdata/Data SPSS/SHP-Data-W1-W21-SPSS"),
                             cols = c("IDPERS", "PYYW604"),
                             year_start = "1999",
                             year_end = "2018")
{
  years <- as.numeric(year_start):as.numeric(year_end)
  data <- list()
  for(i in seq_along(years)) {
    file_i <- paste0("SHP", as.character(sprintf('%02d', years[i] %% 100)), "_", file)
    path_i <- paste0(path, "/W", as.character(as.numeric(years[i]) - 1998), "_", as.character(years[i]))

    message(paste0("Importing year ", years[i], "...  "))
    df <- import_cols(file_i, path_i, cols = yearly_col_names(cols, years[i]))
    names(df)[1] <- "ID"
    data[[i]] <- df
  }
  return(data %>% reduce(left_join, by = "ID"))
}


#' Title
#'
#' @param cols
#' @param year
#'
#' @return
#' @export
#'
#' @examples
#' yearly_col_names(c("IDPERS", "AYYA00"), 2012)
#' yearly_col_names(c("IDPERS", "SEXYY"), 2012)
#' yearly_col_names(c("IDHOUS", "H$$H01"), 2012)
yearly_col_names <- function(cols, year) {
  assertthat::assert_that(assertthat::is.string(cols[1]))
  assertthat::assert_that(assertthat::is.number(year))
  assertthat::assert_that(assertthat::see_if(year >= 1999))
  assertthat::assert_that(assertthat::see_if(year <= 2020))
  # assertthat::assert_that(assertthat::are_equal(cols[1], "IDPERS"))

  if(cols[1] == "IDPERS") {
    for (i in 2:length(cols)) {
      cols[i] <- paste0(stringr::str_split(cols[i], "YY")[[1]][1], as.character(sprintf('%02d', year %% 100)), stringr::str_split(cols[i], "YY")[[1]][2])
    }
  }
  if(cols[1] == "IDHOUS") {
    cols[1] <- paste0(cols[1], as.character(sprintf('%02d', year %% 100)))
    for (i in 2:length(cols)) {
      cols[i] <- paste0(stringr::str_split(cols[i], "YY")[[1]][1], as.character(sprintf('%02d', year %% 100)), stringr::str_split(cols[i], "YY")[[1]][2])

      #cols[i] <- paste0(stringr::str_sub(cols[i], 1, 1), as.character(sprintf('%02d', year %% 100)), stringr::str_sub(cols[i], 4))
    }
  }

  assertthat::assert_that(assertthat::is.string(cols[1]))
  return(cols)
}
