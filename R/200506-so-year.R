y1 <- "AB99"
y2 <- "04CD"
y3 <- "X90Z"
y4 <- "EF09"
y5 <- "12GH"

fun <- function(x) {
  year <- readr::parse_number(x)
  if(year < 50) year <- paste0("20", year) else year <- paste0("19", year)
  print(year)
}
fun(y1)
fun(y2)
fun(y3)
fun(y4)
fun(y5)

fun <- function(x) {
  num <- gsub("\\D", "", x)
  return(paste0(ifelse(num >= "90", "19", "20"), num))
}

fun <- function(x) {
  year <- readr::parse_number(x)
  if (year < 50) {
    year <- paste0("20", stringr::str_pad(year, 2, side="left", "0"))
  } else {
    year <- paste0("19", year)
  }
  return(year)
}

# https://stackoverflow.com/questions/61654975/parse-string-extract-two-digit-year-and-complete-into-four-digit-format
