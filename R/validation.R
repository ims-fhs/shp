#' Title
#'
#' @param file
#' @param path
#' @param col_name
#' @param year_start
#' @param year_end
#'
#' @return
#' @export
#'
#' @examples
#' years_available("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", "PXXC71")
#' years_available("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", "PXXC75")
#' years_available("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", "PXXC106")
#' years_available("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W19-SPSS", "PXXC108")
years_available <- function(file = stop("Please provide a file name in the format xxx.sav"),
                            path = stop("Please provide the path to the file."),
                            col_name = stop("Please provide a single col_name you want the availability."),
                            year_start = "1999",
                            year_end = "2017") {
  years <- as.numeric(year_start):as.numeric(year_end)
  available <- logical(length(years))
  for(i in seq_along(years)) {
    file_i <- paste0("SHP", as.character(sprintf('%02d', years[i] %% 100)), "_", file)
    path_i <- paste0(path, "/W", as.character(as.numeric(years[i]) - 1998), "_", as.character(years[i]))
    available[i] <- yearly_col_names(c("IDPERS", col_name), years[i])[2] %in% names(import_SPSS_file_head(file_i, path_i))
  }
  return(data.frame(years = years, available = available))
}
