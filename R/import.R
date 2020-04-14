#' import_SPSS_file
#'
#' @param file
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' shp99_p_user <- import_SPSS_file("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W1_1999")
import_SPSS_file <- function(file = stop("Please provide a file name in the format xxx.sav"),
                             path = stop("Please provide the path to the file.")) {
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
#' shp99_p_user_head <- import_SPSS_file_head("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W1_1999")
#' shp04_p_user_head <- import_SPSS_file_head("SHP04_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W6_2004")
import_SPSS_file_head <- function(file = stop("Please provide a file name in the format xxx.sav"),
                             path = stop("Please provide the path to the file.")) {
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
#' shp99_p_user_id <- import_id("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W1_1999")
import_id <- function(file = stop("Please provide a file name in the format xxx.sav"),
                      path = stop("Please provide the path to the file.")) {
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
#' shp99_p_user_cols23 <- import_cols("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W1_1999", cols = c(2,3))
#' difficulties to conciliate personal and professional life last 12 months
#' shp99_p_user_cols_id_p99f09 <- import_cols("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W1_1999", cols = c("IDPERS", "P99F09"))
#' shp04_p_user_cols_id_p04w604 <- import_cols("SHP04_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS/W6_2004", cols = c("IDPERS", "P04W604"))
import_cols <- function(file = stop("Please provide a file name in the format xxx.sav"),
                      path = stop("Please provide the path to the file."),
                      cols = stop("Please provide a vector of the column numbers you want to import.")) {
  df <- import_SPSS_file(file, path)
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
#' shp_p_user_cols_id_p99f09 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", cols = c("IDPERS", "P99F09"))
#' shp_p_user_cols_id_p99f09 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", cols = c("IDPERS", "P99F09"), year_start = "1999", year_end = "2003")
#' # work conditions stress
#' shp_p_user_cols_id_w604 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", cols = c("IDPERS", "P04W604"), year_start = "2004", year_end = "2004")
#' shp_p_user_cols_id_w604 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", cols = c("IDPERS", "P04W604"), year_start = "2004", year_end = "2017")
import_long_cols <- function(file = stop("Please provide a file name in the format xxx.sav"),
                                     path = stop("Please provide the path to the file."),
                                     cols = stop("Please provide a vector of the column numbers you want to import."),
                                     year_start = "1999",
                                     year_end = "2017") {
  years <- as.numeric(year_start):as.numeric(year_end)
  data <- list()
  for(i in seq_along(years)) {
    file_i <- paste0("SHP", as.character(sprintf('%02d', years[i] %% 100)), "_", file)
    path_i <- paste0(path, "/W", as.character(as.numeric(years[i]) - 1998), "_", as.character(years[i]))

    df <- import_cols(file_i, path_i, cols = c("IDPERS", paste0("P", as.character(sprintf('%02d', years[i] %% 100)), "W604")))
    data[[i]] <- df
  }
  return(data)
}
