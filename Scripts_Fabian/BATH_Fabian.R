#install.packages('tidyverse')
library(tidyverse)

#' import_SPSS_file
#'
#' @param file
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' shp99_p_user <- import_SPSS_file("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS/W1_1999")
import_SPSS_file <- function(file = stop("SHP99_P_USER.sav"),
                             path = stop("~/FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/W1_1999/SHP99_P_USER.sav")) {
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
#' shp99_h_user_head <- import_SPSS_file_head("SHP99_H_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS/W1_1999")
#' shp99_p_user_head <- import_SPSS_file_head("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS/W1_1999")
#' shp04_p_user_head <- import_SPSS_file_head("SHP04_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS/W6_2004")
import_SPSS_file_head <- function(file = stop("SHP99_P_USER.sav"),
                                  path = stop("~/FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/W1_1999/SHP99_P_USER.sav")) {
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
#' shp99_p_user_id <- import_id("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS/W1_1999")
import_id <- function(file = stop("SHP99_P_USER.sav"),
                      path = stop("~/FHS/5. Semester/BATHV/dataset_932                                                                     (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/"))
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
#' shp99_p_user_cols23 <- import_cols("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS/W1_1999", cols = c(2,3))
#' difficulties to conciliate personal and professional life last 12 months
#' shp99_p_user_cols_id_p99f09 <- import_cols("SHP99_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS/W1_1999", cols = c("IDPERS", "P99F09"))
#' shp04_p_user_cols_id_p04w604 <- import_cols("SHP04_P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS/W6_2004", cols = c("IDPERS", "P04W604"))
import_cols <- function(file = stop("SHP99_P_USER.sav"),
                        path = stop("~/FHS/5. Semester/BATHV/dataset_932                                                                     (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/"),
                        cols = stop(c("IDPERS", "P04W604")))

{
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
#' shp_p_user_cols_id_p99f09 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXF09"))
#' shp_p_user_cols_id_p99f09 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "PXXF09"), year_start = "1999", year_end = "2003")
#' # work conditions stress
#' shp_p_user_cols_id_w604 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "P04W604"), year_start = "2004", year_end = "2004")
#' shp_p_user_cols_id_w604 <- import_long_cols("P_USER.sav", "data/rawdata/Data SPSS/SHP-Data-W1-W20-SPSS", cols = c("IDPERS", "P04W604"), year_start = "2004", year_end = "2017")
#' # interference work - private






import_long_cols <- function(file = stop("P_USER.sav"),
                             path = stop("~/FHS/5. Semester/BATHV/dataset_932(2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/"),
                             cols = c("IDPERS", "PXXW604"),
                             year_start = "1999",
                             year_end = "2018")
{
  years <- as.numeric(year_start):as.numeric(year_end)
  data <- list()
  for(i in seq_along(years))
  {
    file_i <- paste0("SHP", as.character(sprintf('%02d', years[i] %% 100)), "_", file)
    path_i <- paste0(path, "/W", as.character(as.numeric(years[i]) - 1998), "_", as.character(years[i]))



    cat(paste0("Importing year ", years[i], "...  "))
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






depression <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYC17"), year_start = "2004", year_end = "2018")

ermuedung <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYF51"), year_start = "2004", year_end = "2018")

stress_arbeit <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW604"), year_start = "2004", year_end = "2018")

gesundheitliche_erkrankung <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYC04A"), year_start = "2004", year_end = "2018")

arbeit_zufriedenheit_aufgaben <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW229"), year_start = "2004", year_end = "2018")

art_arbeitszeit <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW71A"), year_start = "2004", year_end = "2018")

einbezug_entscheidungen <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW91"), year_start = "2004", year_end = "2018")

arbeitskontrolle_personen <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW87"), year_start = "2004", year_end = "2018")

qualifikation_arbeit <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW100"), year_start = "2004", year_end = "2018")

arbeitsstunden_woche <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW77"), year_start = "2004", year_end = "2018")

wochenend_arbeit <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW218"), year_start = "2004", year_end = "2018")

nachtarbeit <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW216"), year_start = "2004", year_end = "2018")

taegl_pendel_in_min <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW84"), year_start = "2004", year_end = "2018")

arbeits_intensität <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW603"), year_start = "2004", year_end = "2018")

zufriedenheit_arbeitsatmosphaere <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW94"),  year_start = "2004", year_end = "2018")

arbeit_laerm_schmutz <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW605"), year_start = "2004", year_end = "2018")

arbeit_ermuedende_koerperha <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYW606"), year_start = "2004", year_end = "2018")

hausarbeit_stunden_woche <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYF08"), year_start = "2004", year_end = "2018")

beeintraechtigung_arbeit_privat <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYF50"), year_start = "2004", year_end = "2018")

abschalten_nach_arbeit <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYF52"), year_start = "2004", year_end = "2018")

einschraenkung_weg_ges_zustand <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYC08"), year_start = "2004", year_end = "2018")

tage_gesunheits_prob <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYC11"), year_start = "2004", year_end = "2018")

chronische_krankheit <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYC19A"), year_start = "2004", year_end = "2018")

geschlecht <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","SEXYY"),year_start = "2004", year_end = "2018")

hoechster_bildungsabschluss <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","EDUCATYY"), year_start = "2004", year_end = "2018")

partnerschaft <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYD29"), year_start = "2004", year_end = "2018")

tod_person <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYL11"), year_start = "2004", year_end = "2018")

person_haushalt<- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","IDHOUSYY"), year_start = "2004", year_end = "2018")

haushaltsaequivalenzeinkommen <- import_long_cols("H_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDHOUS","IYYEQON"), year_start = "2004", year_end = "2018")

#schweizerstaatsbürgerschaft <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYD19"), year_start = "2004", year_end = "2018")

migrationshintergrund <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","PYYD160"),  year_start = "2004", year_end = "2018")

#pflege_angehoerige <- import_long_cols("H_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDHOUS","HYYF74"),  year_start = "2004", year_end = "2012")

haushaltstyp <- import_long_cols("H_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDHOUS","HLDTYPYY"), year_start = "2004", year_end = "2018")

alter <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","AGEYY"),  year_start = "2004", year_end = "2018")

status <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","STATUSYY"),  year_start = "2004", year_end = "2018")

beschaeftigung <- import_long_cols("P_USER.sav", "FHS/5. Semester/BATHV/dataset_932 (2)/Data_SPSS/Data_SPSS/SHP-Data-W1-W20-SPSS/", cols = c("IDPERS","OCCUPAYY"),  year_start = "2004", year_end = "2018")


schweizerstaatsbürgerschaft <- SHP18_P_USER %>% select(P18D1) # PDYYD19 gibt es gar nicht

mannfrau <- geschlecht %>% drop_na() %>% filter(SEX04+SEX05+SEX06+SEX07+SEX08+SEX09+SEX10+SEX11+SEX12+SEX13+SEX14+SEX15+SEX16+SEX17+SEX18<30) %>%
   filter(SEX04+SEX05+SEX06+SEX07+SEX08+SEX09+SEX10+SEX11+SEX12+SEX13+SEX14+SEX15+SEX16+SEX17+SEX18>15) # Überprüfung von Geschlechtsumwandlung

mannfrau

geschlecht <- geschlecht %>% transmute(ID, SEX04) #

df <- mutate(depression, ermuedung, stress_arbeit, gesundheitliche_erkrankung, arbeit_zufriedenheit_aufgaben, art_arbeitszeit, einbezug_entscheidungen, arbeitskontrolle_personen,
             qualifikation_arbeit, arbeitsstunden_woche, wochenend_arbeit, nachtarbeit, taegl_pendel_in_min, arbeits_intensität, zufriedenheit_arbeitsatmosphaere,
             arbeit_laerm_schmutz, arbeit_ermuedende_koerperha, hausarbeit_stunden_woche, beeintraechtigung_arbeit_privat, abschalten_nach_arbeit, einschraenkung_weg_ges_zustand,
             tage_gesunheits_prob, chronische_krankheit, hoechster_bildungsabschluss, partnerschaft, tod_person, person_haushalt, migrationshintergrund, geschlecht, alter, status,
             beschaeftigung)


# Zusammen basteln von P und H Datensatz
h_datensatz <- mutate(haushaltsaequivalenzeinkommen, haushaltstyp)

h_datensatz_04 <- h_datensatz %>% transmute(ID, I04EQON, HLDTYP04)
h_datensatz_05 <- h_datensatz %>% transmute(ID, I05EQON, HLDTYP05)
h_datensatz_06 <- h_datensatz %>% transmute(ID, I06EQON, HLDTYP06)
h_datensatz_07 <- h_datensatz %>% transmute(ID, I07EQON, HLDTYP07)
h_datensatz_08 <- h_datensatz %>% transmute(ID, I08EQON, HLDTYP08)
h_datensatz_09 <- h_datensatz %>% transmute(ID, I09EQON, HLDTYP09)
h_datensatz_10 <- h_datensatz %>% transmute(ID, I10EQON, HLDTYP10)
h_datensatz_11 <- h_datensatz %>% transmute(ID, I11EQON, HLDTYP11)
h_datensatz_12 <- h_datensatz %>% transmute(ID, I12EQON, HLDTYP12)
h_datensatz_13 <- h_datensatz %>% transmute(ID, I13EQON, HLDTYP13)
h_datensatz_14 <- h_datensatz %>% transmute(ID, I14EQON, HLDTYP14)
h_datensatz_15 <- h_datensatz %>% transmute(ID, I15EQON, HLDTYP15)
h_datensatz_16 <- h_datensatz %>% transmute(ID, I16EQON, HLDTYP16)
h_datensatz_17 <- h_datensatz %>% transmute(ID, I17EQON, HLDTYP17)
h_datensatz_18 <- h_datensatz %>% transmute(ID, I18EQON, HLDTYP18)

df <- full_join(df, h_datensatz_04, by = c("IDHOUS04" = "ID"))
df <- full_join(df, h_datensatz_05, by = c("IDHOUS05" = "ID"))
df <- full_join(df, h_datensatz_06, by = c("IDHOUS06" = "ID"))
df <- full_join(df, h_datensatz_07, by = c("IDHOUS07" = "ID"))
df <- full_join(df, h_datensatz_08, by = c("IDHOUS08" = "ID"))
df <- full_join(df, h_datensatz_09, by = c("IDHOUS09" = "ID"))
df <- full_join(df, h_datensatz_10, by = c("IDHOUS10" = "ID"))
df <- full_join(df, h_datensatz_11, by = c("IDHOUS11" = "ID"))
df <- full_join(df, h_datensatz_12, by = c("IDHOUS12" = "ID"))
df <- full_join(df, h_datensatz_13, by = c("IDHOUS13" = "ID"))
df <- full_join(df, h_datensatz_14, by = c("IDHOUS14" = "ID"))
df <- full_join(df, h_datensatz_15, by = c("IDHOUS15" = "ID"))
df <- full_join(df, h_datensatz_16, by = c("IDHOUS16" = "ID"))
df <- full_join(df, h_datensatz_17, by = c("IDHOUS17" = "ID"))
df <- full_join(df, h_datensatz_18, by = c("IDHOUS18" = "ID"))

df <- df %>% drop_na(ID)

df <- df %>% filter(AGE04 <= 65)
df <- df %>% filter(AGE05 <= 65)
df <- df %>% filter(AGE06 <= 65)
df <- df %>% filter(AGE07 <= 65)
df <- df %>% filter(AGE08 <= 65)
df <- df %>% filter(AGE09 <= 65)
df <- df %>% filter(AGE10 <= 65)
df <- df %>% filter(AGE11 <= 65)
df <- df %>% filter(AGE12 <= 65)
df <- df %>% filter(AGE13 <= 65)
df <- df %>% filter(AGE14 <= 65)
df <- df %>% filter(AGE15 <= 65)
df <- df %>% filter(AGE16 <= 65)
df <- df %>% filter(AGE17 <= 65)
df <- df %>% filter(AGE18 <= 65)

df <- df %>% filter(AGE04 >= 15)
df <- df %>% filter(AGE05 >= 15)
df <- df %>% filter(AGE06 >= 15)
df <- df %>% filter(AGE07 >= 15)
df <- df %>% filter(AGE08 >= 15)
df <- df %>% filter(AGE09 >= 15)
df <- df %>% filter(AGE10 >= 15)
df <- df %>% filter(AGE11 >= 15)
df <- df %>% filter(AGE12 >= 15)
df <- df %>% filter(AGE13 >= 15)
df <- df %>% filter(AGE14 >= 15)
df <- df %>% filter(AGE15 >= 15)
df <- df %>% filter(AGE16 >= 15)
df <- df %>% filter(AGE17 >= 15)
df <- df %>% filter(AGE18 >= 15)


df <- df %>% filter(STATUS04 == 0)
df <- df %>% filter(STATUS05 == 0)
df <- df %>% filter(STATUS06 == 0)
df <- df %>% filter(STATUS07 == 0)
df <- df %>% filter(STATUS08 == 0)
df <- df %>% filter(STATUS09 == 0)
df <- df %>% filter(STATUS10 == 0)
df <- df %>% filter(STATUS11 == 0)
df <- df %>% filter(STATUS12 == 0)
df <- df %>% filter(STATUS13 == 0)
df <- df %>% filter(STATUS14 == 0)
df <- df %>% filter(STATUS15 == 0)
df <- df %>% filter(STATUS16 == 0)
df <- df %>% filter(STATUS17 == 0)
df <- df %>% filter(STATUS18 == 0)

df <- df %>% filter(OCCUPA04 >= 1)
df <- df %>% filter(OCCUPA05 >= 1)
df <- df %>% filter(OCCUPA06 >= 1)
df <- df %>% filter(OCCUPA07 >= 1)
df <- df %>% filter(OCCUPA08 >= 1)
df <- df %>% filter(OCCUPA09 >= 1)
df <- df %>% filter(OCCUPA10 >= 1)
df <- df %>% filter(OCCUPA11 >= 1)
df <- df %>% filter(OCCUPA12 >= 1)
df <- df %>% filter(OCCUPA13 >= 1)
df <- df %>% filter(OCCUPA14 >= 1)
df <- df %>% filter(OCCUPA15 >= 1)
df <- df %>% filter(OCCUPA16 >= 1)
df <- df %>% filter(OCCUPA17 >= 1)
df <- df %>% filter(OCCUPA18 >= 1)

df <- df %>% filter(OCCUPA04 <= 3)
df <- df %>% filter(OCCUPA05 <= 3)
df <- df %>% filter(OCCUPA06 <= 3)
df <- df %>% filter(OCCUPA07 <= 3)
df <- df %>% filter(OCCUPA08 <= 3)
df <- df %>% filter(OCCUPA09 <= 3)
df <- df %>% filter(OCCUPA10 <= 3)
df <- df %>% filter(OCCUPA11 <= 3)
df <- df %>% filter(OCCUPA12 <= 3)
df <- df %>% filter(OCCUPA13 <= 3)
df <- df %>% filter(OCCUPA14 <= 3)
df <- df %>% filter(OCCUPA15 <= 3)
df <- df %>% filter(OCCUPA16 <= 3)
df <- df %>% filter(OCCUPA17 <= 3)
df <- df %>% filter(OCCUPA18 <= 3)




#install.packages("writexl")
library(writexl)


#install.packages('Rcmdr')
library('Rcmdr')

name_col <- colnames(depression)
name_col
dt_na <- df %>% select(name_col) %>% subset(select = -c(ID)) %>% sapply(function(x) sum(is.na(x)))
dt_na <- t(dt_na)
dt_na <- as.data.frame(dt_na)
dt_na
#dt <- name_col %>% subset(select = -c(ID)) %>% sapply(function(x) summary(x))
dt <- df %>% select(name_col) %>% subset(select = -c(ID)) %>% sapply(function(x) table(factor(x, levels = 0:10)))
dt <- as.data.frame(dt)
dt
#dt_perc_ohne_na <- dt %>% colPercents(digits=2)
#dt_perc_ohne_na
#dt_perc_ohne_na <- as.data.frame(dt_perc_ohne_na)
dt_mit_na <- rbind(dt, dt_na)
dt_mit_na <- as.data.frame(dt_mit_na)
dt_mit_na
#dt_perc_mit_na <- dt_mit_na %>% colPercents(digits=2)
#dt_perc_mit_na
#dt_perc_mit_na <- as.data.frame(dt_perc_mit_na)
Means=rowMeans(dt_mit_na)
Means <- t(Means)
Means <- as.data.frame(Means)
Means
#liste <- list(dt_mit_na, dt)
write_xlsx(x=Means, path = "means.xlsx", col_names=TRUE, format_headers = TRUE)








