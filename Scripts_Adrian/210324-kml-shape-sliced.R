library(tidyverse)
library(kmlShape)

load("data/df_long.R")

# Was sind die namen von OCCUOA und HLDTYP?
colnames(df_long) <- c("id","year","depression","ermuedung","stress_arbeit","rueckenschmerzen","arbeit_zufriedenheit_aufgaben","art_arbeitszeit","arbeit_einbezug_entscheidungen","arbeitskontrolle_personen","arbeit_qualifikation","arbeitsstunden_woche","wochenend_arbeit","nachtarbeit","taegl_pendeln_min","arbeit_intensitaet","zufriedenheit_arbeitsatmosphaere","arbeit_laerm_schmutz","arbeit_ermuedende_koerperha","hausarbeit_stunden_woche","beeintraechtigung_arbeit_privat","abschalten_nach_arbeit","einschraenkung_weg_ges_zustand","tage_gesunheits_prob","chronische_krankheit","ausbildung","partnerschaft","tod_person","person_haushalt","migrationshintergrund","geschlecht","alter","status","occupa","haushaltsaequivalenzeinkommen","hldtyp")

df_long = as.data.frame(sapply(df_long, as.numeric))

df_short = df_long[1:4]

### small test sample
data = df_long[1:4] %>% filter(id <= 202102)

#### slice function
slice_data_in_sequence <- function(data, n)
{
  ### Makes list where each dataframe from single id is an item in the list, without any na's
  data_per_id <- list()
  for(i in 1:length(unique(data$id)))
  {
    data_per_id[[i]] = data %>% filter(id == unique(data$id)[i]) %>% drop_na()
  }

  ### Makes list where each dataframe from single id is an item in the list, cut down to the sequence length
  data_cut <- list()
  for(i in 1:length(data_per_id))
  {
    ### checks if length of not na rows longer than sequence
    if(nrow(data_per_id[[i]]) >= n)
    {
      ### checks if the year is continous without missing years
      if(max(data_per_id[[i]]$year) - min(data_per_id[[i]]$year) == length(data_per_id[[i]]$year)-1)
      {
        data_cut[[i]] = data_per_id[[i]][1:n,]
        data_cut[[i]]$rel_year <- seq.int(nrow(data_cut[[i]]))
      }
    }
  }

  ### drop empty lists
  data_cut = data_cut[-which(sapply(data_cut, is.null))]

  ### change list to data frame
  dataframe_sliced <- do.call(rbind.data.frame, data_cut)
  return(dataframe_sliced)
}

### can take some minutes!
sliced_data = slice_data_in_sequence(df_short[1:10000,],5)

kml_data = sliced_data %>% select(id, rel_year, depression)
kml_data_100 = kml_data %>% filter(id <= unique(kml_data$id)[100])

clustered <- cldsLong(kml_data_100)

Shape <- kmlShape(clustered, nbClusters = 4)
